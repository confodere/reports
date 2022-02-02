use chrono::{Datelike, Duration, NaiveDate, Weekday};
use chronoutil::{delta, RelativeDuration};
use rusqlite::ToSql;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::hash::Hash;
use std::ops::{Add, Div, Sub};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, Copy)]
pub struct TimeSpan {
    start_date: NaiveDate,
    frequency: TimeFrequency,
}

impl TimeSpan {
    pub fn new(date: &NaiveDate, frequency: TimeFrequency) -> TimeSpan {
        TimeSpan {
            start_date: TimeSpan::find_start_date(date, &frequency),
            frequency,
        }
    }

    fn find_start_date(date: &NaiveDate, frequency: &TimeFrequency) -> NaiveDate {
        match frequency {
            TimeFrequency::Yearly => NaiveDate::from_ymd(date.year(), 1, 1),
            TimeFrequency::Quarterly => {
                NaiveDate::from_ymd(date.year(), ((date.month() / 4) * 3) + 1, 1)
            }
            TimeFrequency::Monthly => NaiveDate::from_ymd(date.year(), date.month(), 1),
            TimeFrequency::Weekly => {
                NaiveDate::from_isoywd(date.year(), date.iso_week().week(), Weekday::Mon)
            }
            TimeFrequency::Daily => *date,
        }
    }

    pub fn start(&self) -> &NaiveDate {
        &self.start_date
    }

    pub fn end(&self) -> NaiveDate {
        (self + self.frequency).start_date - Duration::days(1)
    }

    pub fn freq(&self) -> TimeFrequency {
        self.frequency
    }
}

fn diff(span: &TimeSpan, frequency: &TimeFrequency, change: i32) -> NaiveDate {
    match frequency {
        TimeFrequency::Yearly => match span.frequency {
            TimeFrequency::Weekly => NaiveDate::from_isoywd(
                span.start_date.year() + change,
                span.start_date.iso_week().week(),
                span.start_date.weekday(),
            ),
            _ => delta::shift_years(span.start_date, change),
        },
        TimeFrequency::Quarterly => delta::shift_months(span.start_date, 3 * change),
        TimeFrequency::Monthly => delta::shift_months(span.start_date, change),
        TimeFrequency::Weekly => span.start_date + Duration::days((7 * change).into()),
        TimeFrequency::Daily => span.start_date + Duration::days(change.into()),
    }
}

impl Add<TimeFrequency> for &TimeSpan {
    type Output = TimeSpan;

    fn add(self, rhs: TimeFrequency) -> TimeSpan {
        TimeSpan {
            start_date: diff(self, &rhs, 1),
            frequency: self.frequency,
        }
    }
}

impl Sub<TimeFrequency> for &TimeSpan {
    type Output = TimeSpan;

    fn sub(self, rhs: TimeFrequency) -> TimeSpan {
        TimeSpan {
            start_date: diff(self, &rhs, -1),
            frequency: self.frequency,
        }
    }
}

pub struct TimeSpanIter {
    span: TimeSpan,
    frequency: TimeFrequency,
    depth: i32,
    count: i32,
}

impl TimeSpanIter {
    pub fn new(span: TimeSpan, frequency: TimeFrequency, depth: i32) -> TimeSpanIter {
        TimeSpanIter {
            span,
            frequency,
            depth,
            count: 0,
        }
    }
}

impl Iterator for TimeSpanIter {
    type Item = TimeSpan;

    fn next(&mut self) -> Option<Self::Item> {
        if self.count < self.depth {
            if self.count > 0 {
                self.span.start_date = TimeSpan::find_start_date(
                    &diff(&self.span, &self.frequency, -1),
                    &self.span.frequency,
                );
            }
            self.count += 1;
            Some(self.span)
        } else {
            None
        }
    }
}

/// Returns how many numerator are in the denominator.  
/// Year/Quarter/Month are divided exactly.
/// Week/Day are divided exactly.  
/// Week or Day / Year or Quarter or Month are divided based on the number of instances in the TimeSpan date
///
/// For the unclear cases of Week/Quarter and Week/Month, the following rule is applied ([as described here](<https://en.wikipedia.org/wiki/ISO_week_date#Weeks_per_month>)):
/// - Quarters have 13 weeks except the final quarter of a year with 53 weeks (which has 14)
/// - Months have 4 or 5 weeks based on an extrapolation of ISO 8601-1 week system to months instead of years
///
/// # Examples
///
/// ```
/// use chrono::NaiveDate;
/// use reports::{TimeSpan, TimeFrequency};
/// let date = NaiveDate::from_ymd(2022, 1, 21);
/// let date_53 = NaiveDate::from_ymd(2020, 1, 21);
///
/// assert_eq!(TimeSpan::new(&date, TimeFrequency::Weekly) / TimeFrequency::Yearly, 52.0);
/// assert_eq!(TimeSpan::new(&date_53, TimeFrequency::Weekly) / TimeFrequency::Yearly, 53.0);
///
/// assert_eq!(TimeSpan::new(&date, TimeFrequency::Yearly) / TimeFrequency::Quarterly, 0.25);
/// assert_eq!(TimeSpan::new(&date, TimeFrequency::Monthly) / TimeFrequency::Quarterly, 3.0);
/// assert_eq!(TimeSpan::new(&date, TimeFrequency::Weekly) / TimeFrequency::Quarterly, 13.0);
///
/// assert_eq!(TimeSpan::new(&date, TimeFrequency::Weekly) / TimeFrequency::Monthly, 4.0);
///
/// assert_eq!(TimeSpan::new(&date, TimeFrequency::Daily) / TimeFrequency::Weekly, 7.0);
/// ```
impl Div<TimeFrequency> for TimeSpan {
    type Output = f64;

    /// Internally assigns a numeric value to both the denominator & numerator so that the result gives the expected value
    /// The value of Weeks & Days is adjusted specific to the different circumstances of the other frequency
    /// i.e. neither divides evenly with 'Years', 'Quarters', or 'Months', so a circumstantial result is given.
    fn div(self, rhs: TimeFrequency) -> Self::Output {
        let final_day = NaiveDate::from_ymd(self.start_date.year() + 1, 1, 1).pred();
        let (week, day) = (final_day.iso_week().week(), final_day.ordinal());

        let [num, denom] =
            [(&self.frequency, &rhs), (&rhs, &self.frequency)].map(|(freq, other)| match freq {
                TimeFrequency::Yearly | TimeFrequency::Quarterly | TimeFrequency::Monthly => {
                    // Assumes values 1 | 4 | 12
                    freq.clone() as u32
                }
                TimeFrequency::Weekly => match other {
                    TimeFrequency::Yearly => week,
                    TimeFrequency::Quarterly => match week {
                        53 if self.start_date.month() > 9 => 56, // 14
                        _ => 52,                                 // 13
                    },
                    TimeFrequency::Monthly => {
                        let first_of_month = self.start_date.with_day(1).unwrap();
                        let last_of_month = (first_of_month + RelativeDuration::months(1)).pred();
                        match first_of_month.weekday() {
                            Weekday::Thu if last_of_month.day() >= 29 => 60,
                            Weekday::Wed if last_of_month.day() >= 30 => 60,
                            Weekday::Tue
                                if (last_of_month.day() == 31
                                    && last_of_month.weekday() == Weekday::Thu) =>
                            {
                                60
                            } // 5 weeks in month
                            _ => 48, // 4 weeks
                        }
                    }
                    _ => week, // daily is adjusted seperately, relying on weeks to be the number of weeks in the year
                },
                TimeFrequency::Daily => match other {
                    TimeFrequency::Yearly => day,
                    TimeFrequency::Quarterly => {
                        let quarter = ((self.start_date.month() - 1) / 3) + 1;
                        match quarter {
                            1 if day == 365 => 360,
                            1 | 2 => 364,
                            3 | 4 => 368,
                            _ => panic!("Invalid quarter number: {}", quarter),
                        }
                    }
                    TimeFrequency::Monthly => match self.start_date.month() {
                        1 | 3 | 5 | 7 | 8 | 10 | 12 => 372,
                        2 => {
                            if day == 365 {
                                336
                            } else {
                                348
                            }
                        }
                        4 | 6 | 9 | 11 => 360,
                        _ => panic!("Invalid month number: {}", self.start_date.month()),
                    },
                    TimeFrequency::Weekly => match week {
                        52 => 364,
                        53 => 371,
                        _ => panic!("Invalid week number: {}", week),
                    },
                    TimeFrequency::Daily => day,
                },
            });
        num as f64 / denom as f64
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum TimeFrequency {
    Yearly = 1,
    Quarterly = 4,
    Monthly = 12,
    Weekly = 2,
    Daily = 14,
}

impl TimeFrequency {
    pub fn from_str(variant: String) -> Result<TimeFrequency, &'static str> {
        Ok(match variant.as_str() {
            "Yearly" => TimeFrequency::Yearly,
            "Quarterly" => TimeFrequency::Quarterly,
            "Monthly" => TimeFrequency::Monthly,
            "Weekly" => TimeFrequency::Weekly,
            "Daily" => TimeFrequency::Daily,
            _ => return Err("Read TimeFrequency not found"),
        })
    }
}

impl ToSql for TimeFrequency {
    fn to_sql(&self) -> rusqlite::Result<rusqlite::types::ToSqlOutput<'_>> {
        Ok(format!("{:#?}", self).into())
    }
}

impl fmt::Display for TimeFrequency {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

/// Provides a display format for each variant of TimePeriod
///
/// # Examples
///
/// ```
/// use chrono::NaiveDate;
/// use reports::{TimeFrequency, TimeSpan};
/// let date = NaiveDate::from_ymd(2022, 1, 17);
/// let day = TimeSpan::new(&date, TimeFrequency::Daily);
/// let week = TimeSpan::new(&date, TimeFrequency::Weekly);
/// let month = TimeSpan::new(&date, TimeFrequency::Monthly);
/// let quarter = TimeSpan::new(&date, TimeFrequency::Quarterly);
/// let year = TimeSpan::new(&date, TimeFrequency::Yearly);
///
/// assert_eq!(day.to_string(), "17/01/2022");
/// assert_eq!(week.to_string(), "2022 (17th January to 23rd January)");
/// assert_eq!(month.to_string(), "January 2022");
/// assert_eq!(quarter.to_string(), "Q1 2022");
/// assert_eq!(year.to_string(), "2022");
/// ```
impl fmt::Display for TimeSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.frequency {
            TimeFrequency::Yearly => write!(f, "{}", self.start_date.format("%Y")),
            TimeFrequency::Quarterly => write!(
                f,
                "Q{} {}",
                self.start_date.month() / 4 + 1,
                self.start_date.year()
            ),
            TimeFrequency::Monthly => {
                write!(
                    f,
                    "{} {}",
                    self.start_date.format("%B"),
                    self.start_date.year(),
                )
            }
            TimeFrequency::Weekly => {
                let weeks: Vec<String> = [&self.start_date, &self.end()]
                    .into_iter()
                    .map(|date| {
                        format!(
                            "{}{} {}",
                            date.format("%-d"),
                            ordinal_date(&date.day()),
                            date.format("%B")
                        )
                    })
                    .collect();
                write!(
                    f,
                    "{} ({} to {})",
                    self.start_date.year(),
                    weeks[0],
                    weeks[1],
                )
            }
            TimeFrequency::Daily => write!(f, "{}", self.start_date.format("%d/%m/%Y")),
        }
    }
}

fn ordinal_date(n: &u32) -> &str {
    let s = n.to_string();
    if s.ends_with("1") && !s.ends_with("11") {
        "st"
    } else if s.ends_with("2") && !s.ends_with("12") {
        "nd"
    } else if s.ends_with("3") && !s.ends_with("13") {
        "rd"
    } else {
        "th"
    }
}

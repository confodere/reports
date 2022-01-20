use chrono::{Datelike, Duration, IsoWeek, NaiveDate, Weekday};
use chronoutil::delta;
use rusqlite::ToSql;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::hash::Hash;
use std::ops::{Add, Sub};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
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
        (self.clone() + self.clone().frequency).start_date - Duration::days(1)
    }
}

impl Add<TimeFrequency> for TimeSpan {
    type Output = Self;

    fn add(self, rhs: TimeFrequency) -> Self::Output {
        TimeSpan {
            start_date: match rhs {
                TimeFrequency::Yearly => match self.frequency {
                    TimeFrequency::Weekly => NaiveDate::from_isoywd(
                        self.start_date.year() + 1,
                        self.start_date.iso_week().week(),
                        self.start_date.weekday(),
                    ),
                    _ => delta::shift_years(self.start_date, 1),
                },
                TimeFrequency::Quarterly => delta::shift_months(self.start_date, 3),
                TimeFrequency::Monthly => delta::shift_months(self.start_date, 1),
                TimeFrequency::Weekly => self.start_date + Duration::days(7),
                TimeFrequency::Daily => self.start_date + Duration::days(1),
            },
            frequency: self.frequency,
        }
    }
}

impl Sub<TimeFrequency> for TimeSpan {
    type Output = Self;

    fn sub(self, rhs: TimeFrequency) -> Self::Output {
        TimeSpan {
            start_date: match rhs {
                TimeFrequency::Yearly => match self.frequency {
                    TimeFrequency::Weekly => NaiveDate::from_isoywd(
                        self.start_date.year() - 1,
                        self.start_date.iso_week().week(),
                        self.start_date.weekday(),
                    ),
                    _ => delta::shift_years(self.start_date, -1),
                },
                TimeFrequency::Quarterly => delta::shift_months(self.start_date, -3),
                TimeFrequency::Monthly => delta::shift_months(self.start_date, -1),
                TimeFrequency::Weekly => self.start_date - Duration::days(7),
                TimeFrequency::Daily => self.start_date - Duration::days(1),
            },
            frequency: self.frequency,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TimeFrequencyMismatch {
    pub message: String,
}

impl fmt::Display for TimeFrequencyMismatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TimeFrequency is not cross compatible: {}", self.message)
    }
}

impl std::error::Error for TimeFrequencyMismatch {}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash)]
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

    /// Returns how many numerator are in denominator,
    /// but only if TimeFrequencies can be divided evenly.
    ///
    /// Assumes that Monthly, Quarterly, and Yearly can be freely converted.
    /// As well as Daily and Weekly.
    /// But that these two groups are incompatible.
    ///
    /// ## Panics
    /// Panics if attempting to cross convert (Months, Quarters, Years) and (Days, Weeks)
    pub fn divide(
        numerator: &TimeFrequency,
        denominator: &TimeFrequency,
    ) -> Result<f64, TimeFrequencyMismatch> {
        // Assumes small and large are mutually exclusive
        // Enum discriminants are used compare variants
        let (small, large) = ([2, 14], [1, 4, 12]);
        let (numerator, denominator) = (numerator.clone() as i32, denominator.clone() as i32);
        if (large.contains(&numerator) && large.contains(&denominator))
            | (small.contains(&numerator) && small.contains(&denominator))
        {
            Ok(numerator as f64 / denominator as f64)
        } else {
            Err(TimeFrequencyMismatch {
                message: denominator.to_string(),
            })
        }
    }
}

impl ToSql for TimeFrequency {
    fn to_sql(&self) -> rusqlite::Result<rusqlite::types::ToSqlOutput<'_>> {
        Ok(format!("{:#?}", self).into())
    }
}

type Year = i32;
type Quarter = u8;
type Month = u32;

/// TimePeriod allows a NaiveDate to be expressed with the specificity implicit in each TimeFrequency variant,
/// i.e. all dates in a month have the same TimePeriod::Month value
///
/// # Examples
///
/// ```
/// use chrono::NaiveDate;
/// use reports::{TimeFrequency, TimePeriod};
///
/// let date = NaiveDate::from_ymd(2022, 6, 6);
/// let freq = TimeFrequency::Quarterly;
/// let time_period = TimePeriod::new(&date, &freq);
///
/// if let TimePeriod::Quarter(year, quarter) = time_period {
/// 	assert_eq!(year, 2022);
/// 	assert_eq!(quarter, 2);
/// }
///
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TimePeriod {
    Year(Year),
    // Quarter is expressed as (Jan, Feb, Mar) = 1, (Apr, May, Jun) = 2, (Jul, Aug, Sep) = 3, (Oct, Nov, Dec) = 4
    Quarter(Year, Quarter),
    Month(Year, Month),
    Week(IsoWeek),
    Day(NaiveDate),
}

impl TimePeriod {
    pub fn new(date: &NaiveDate, frequency: &TimeFrequency) -> TimePeriod {
        match frequency {
            TimeFrequency::Yearly => TimePeriod::Year(date.year()),
            TimeFrequency::Quarterly => {
                TimePeriod::Quarter(date.year(), ((date.month0() / 3) + 1) as u8)
            }
            TimeFrequency::Monthly => TimePeriod::Month(date.year(), date.month()),
            TimeFrequency::Weekly => TimePeriod::Week(date.iso_week()),
            TimeFrequency::Daily => TimePeriod::Day(date.clone()),
        }
    }

    pub fn start_date(&self) -> NaiveDate {
        match self {
            TimePeriod::Year(year) => NaiveDate::from_ymd(*year, 1, 1),
            TimePeriod::Quarter(year, quarter) => {
                NaiveDate::from_ymd(*year, (quarter * 3 - 1) as u32, 1)
            }
            TimePeriod::Month(year, month) => NaiveDate::from_ymd(*year, *month, 1),
            TimePeriod::Week(week) => {
                NaiveDate::from_isoywd(week.year(), week.week(), Weekday::Mon)
            }
            TimePeriod::Day(date) => *date,
        }
    }

    pub fn end_date(&self) -> NaiveDate {
        self.next_or_prev(1).start_date() - Duration::days(1)
    }

    fn next_or_prev(&self, change: i32) -> TimePeriod {
        match self {
            TimePeriod::Year(year) => TimePeriod::Year(year + change),
            TimePeriod::Quarter(year, quarter) => {
                let new_quarter = (*quarter as i32) + change;
                if new_quarter <= 4 && new_quarter > 0 {
                    TimePeriod::Quarter(
                        *year,
                        ((*quarter as i32) + change)
                            .try_into()
                            .expect("Couldn't create Quarter with such a value"),
                    )
                } else {
                    TimePeriod::Quarter(
                        year + (change / 12) + if change > 0 { 1 } else { -1 },
                        (change % 12 + if change < 0 { 13 } else { 0 })
                            .try_into()
                            .unwrap(),
                    )
                }
            }
            TimePeriod::Month(year, month) => {
                let new_month = (*month as i32) + change;
                if new_month <= 12 && new_month > 0 {
                    TimePeriod::Month(
                        *year,
                        ((*month as i32) + change)
                            .try_into()
                            .expect("Couldn't create Month with such a value"),
                    )
                } else {
                    if change > 0 {
                        TimePeriod::Month(
                            *year + (change / 12) + 1,
                            (change % 12).try_into().unwrap(),
                        )
                    } else {
                        TimePeriod::Month(
                            *year + (change / 12) - 1,
                            (change % 12 + 13).try_into().unwrap(),
                        )
                    }
                }
            }
            // Wrap around is handled by Chrono Durations for Week and Day
            // as a year does not contain a constant amount of either
            TimePeriod::Week(week) => TimePeriod::Week(
                (NaiveDate::from_isoywd(week.year(), week.week(), Weekday::Mon)
                    + Duration::weeks(change.into()))
                .iso_week(),
            ),
            TimePeriod::Day(date) => TimePeriod::Day(*date + Duration::days(change.into())),
        }
    }

    /// The next relevant TimePeriod
    ///
    /// # Examples
    ///
    /// ```
    /// use reports::TimePeriod;
    /// let q4 = TimePeriod::Quarter(2022, 4);
    /// assert!(matches!(q4.succ(), TimePeriod::Quarter(2023, 1)))
    /// ```
    pub fn succ(&self) -> TimePeriod {
        self.next_or_prev(1)
    }

    /// The previous relevant TimePeriod
    ///
    /// # Examples
    ///
    /// ```
    /// use reports::TimePeriod;
    /// let jan = TimePeriod::Month(2022, 1);
    /// assert!(matches!(jan.prev(), TimePeriod::Month(2021, 12)))
    /// ```
    pub fn prev(&self) -> TimePeriod {
        self.next_or_prev(-1)
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
/// assert_eq!(week.to_string(), "2022 (17th January to 23rd  January)");
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

impl Hash for TimePeriod {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            TimePeriod::Year(year) => year.hash(state),
            TimePeriod::Quarter(year, quarter) => {
                year.hash(state);
                quarter.hash(state)
            }
            TimePeriod::Month(year, month) => {
                year.hash(state);
                month.hash(state)
            }
            TimePeriod::Week(week) => {
                week.year().hash(state);
                week.week().hash(state)
            }
            TimePeriod::Day(date) => date.hash(state),
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
        "rd "
    } else {
        "th"
    }
}

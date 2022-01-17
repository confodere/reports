use core::fmt;
use num_traits::FromPrimitive;
use std::{collections::HashMap, fmt::Display, hash::Hash};

use chrono::{Datelike, Duration, IsoWeek, NaiveDate, Weekday};
use itertools::Itertools;
use serde::{Deserialize, Serialize};

use rusqlite::{params, Connection, ToSql};

const DATABASE_FILE: &str = "ignore/data.db";

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

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum TimeFrequency {
    Yearly = 1,
    Quarterly = 4,
    Monthly = 12,
    Weekly = 2,
    Daily = 14,
}

impl TimeFrequency {
    fn from_str(variant: String) -> Result<TimeFrequency, &'static str> {
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
/// use reports::{TimeFrequency, TimePeriod};
/// let date = NaiveDate::from_ymd(2022, 1, 17);
/// let day = TimePeriod::new(&date, &TimeFrequency::Daily);
/// let week = TimePeriod::new(&date, &TimeFrequency::Weekly);
/// let month = TimePeriod::new(&date, &TimeFrequency::Monthly);
/// let quarter = TimePeriod::new(&date, &TimeFrequency::Quarterly);
/// let year = TimePeriod::new(&date, &TimeFrequency::Yearly);
///
/// assert_eq!(day.to_string(), "17/01/2022");
/// assert_eq!(week.to_string(), "2022 (17th January to 21st January)");
/// assert_eq!(month.to_string(), "January 2022");
/// assert_eq!(quarter.to_string(), "Q1 2022");
/// assert_eq!(year.to_string(), "2022");
/// ```
impl Display for TimePeriod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TimePeriod::Year(year) => write!(f, "{}", year),
            TimePeriod::Quarter(year, quarter) => write!(f, "Q{} {}", quarter, year),
            TimePeriod::Month(year, month) => write!(
                f,
                "{} {}",
                {
                    if let Some(m) = chrono::Month::from_u32(*month) {
                        m.name()
                    } else {
                        panic!("Doesn't fit!!!")
                    }
                },
                year
            ),
            TimePeriod::Week(week) => write!(
                f,
                "{} ({} to {})",
                week.year(),
                {
                    let date = NaiveDate::from_isoywd(week.year(), week.week(), Weekday::Mon);
                    format!(
                        "{}{} {}",
                        date.format("%-d"),
                        ordinal_date(&date.day()),
                        date.format("%B")
                    )
                },
                {
                    let date = NaiveDate::from_isoywd(week.year(), week.week(), Weekday::Sun);
                    format!(
                        "{}{} {}",
                        date.format("%-d"),
                        ordinal_date(&date.day()),
                        date.format("%B")
                    )
                },
            ),
            TimePeriod::Day(date) => write!(f, "{}", date.format("%d/%m/%Y")),
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

pub trait Figure {
    /// Inserts data into description by replacing the characters {} in the description
    /// Panics if {} not present in description
    fn format(&self, description: &String, data: String) -> String {
        let insert_position = description
            .find("{}")
            .expect("Couldn't find place to insert data");
        format!(
            "{} {}{}",
            &description[0..(insert_position - 1)],
            data,
            &description[(insert_position + 2)..]
        )
    }

    fn averaged(&self, frequency: &TimeFrequency) -> f64 {
        self.fig()
            / TimeFrequency::divide(frequency, &self.metric_info().frequency)
                .expect("Cannot average accurately")
    }

    fn period(&self) -> TimePeriod {
        TimePeriod::new(self.when(), &self.metric_info().frequency)
    }

    fn metric_info(&self) -> &Metric;
    fn when(&self) -> &NaiveDate;

    fn fig(&self) -> f64;
}

#[derive(Clone, Serialize, Deserialize)]
pub struct Metric {
    name: String,
    description: Option<String>,
    print_text: String,
    frequency: TimeFrequency,
}

impl Metric {
    pub fn new(
        name: String,
        description: Option<String>,
        print_text: String,
        frequency: TimeFrequency,
    ) -> Metric {
        Metric {
            name,
            description,
            print_text,
            frequency,
        }
    }

    /// Reads all Metrics saved in sqlite3
    pub fn read() -> rusqlite::Result<HashMap<String, Metric>> {
        let conn = Connection::open(DATABASE_FILE)?;

        let mut stmt =
            conn.prepare("SELECT name, description, print_text, frequency FROM metric")?;

        let metric_iter = stmt.query_map([], |row| {
            Ok(Metric::new(
                row.get(0)?,
                row.get(1)?,
                row.get(2)?,
                TimeFrequency::from_str(row.get(3)?)
                    .expect("Couldn't match TimeFrequency read from database"),
            ))
        })?;

        let mut found: HashMap<String, Metric> = HashMap::new();
        for metric in metric_iter {
            if let Ok(f) = metric {
                found.insert(f.name.clone(), f);
            }
        }
        Ok(found)
    }

    /// Inserts current metric into sqlite3 database
    pub fn write(&self) -> rusqlite::Result<()> {
        let conn = Connection::open(DATABASE_FILE)?;

        conn.execute(
            r#"CREATE TABLE IF NOT EXISTS metric (
            name TEXT PRIMARY KEY, 
            description TEXT, 
            print_text TEXT, 
            frequency TEXT)"#,
            [],
        )?;

        conn.execute(
            "INSERT INTO metric (name, description, print_text, frequency) VALUES (?1, ?2, ?3, ?4)",
            params![self.name, self.description, self.print_text, self.frequency],
        )?;

        Ok(())
    }
}

#[derive(Serialize, Deserialize)]
pub struct FigChange {
    old: f64,
    new: f64,
    metric: Metric,
    when: NaiveDate,
}

impl Figure for FigChange {
    fn fig(&self) -> f64 {
        (self.new - self.old) / self.old
    }

    fn metric_info(&self) -> &Metric {
        &self.metric
    }

    fn when(&self) -> &NaiveDate {
        &self.when
    }
}

impl FigChange {
    pub fn new(metric: Metric, when: NaiveDate, old: f64, new: f64) -> FigChange {
        FigChange {
            old,
            new,
            metric,
            when,
        }
    }

    pub fn from_period(
        metric: Metric,
        period: &TimePeriod,
        datapoints: &HashMap<TimePeriod, Datapoint>,
    ) -> Option<FigChange> {
        if datapoints.contains_key(&period) && datapoints.contains_key(&period.prev()) {
            Some(FigChange {
                old: datapoints.get(&period).unwrap().fig(),
                new: datapoints.get(&period.prev()).unwrap().fig(),
                metric,
                when: period.start_date(),
            })
        } else {
            None
        }
    }
}

impl Display for FigChange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.format(
                &self.metric.print_text,
                DisplayType::DescribedPercentage(self).to_string()
            )
        )
    }
}

#[derive(Clone)]
pub struct Datapoint {
    value: f64,
    metric: Metric,
    when: NaiveDate,
}

impl Datapoint {
    pub fn new(value: f64, metric: Metric, when: NaiveDate) -> Datapoint {
        Datapoint {
            value,
            metric,
            when,
        }
    }

    pub fn write(&self) -> rusqlite::Result<()> {
        let conn = Connection::open(DATABASE_FILE)?;

        conn.execute(
            r#"CREATE TABLE IF NOT EXISTS data (
            metric_name TEXT NOT NULL, 
            naive_date TEXT NOT NULL, 
            val REAL, 
            PRIMARY KEY (metric_name, naive_date), 
            FOREIGN KEY(metric_name) REFERENCES metric(name))"#,
            [],
        )?;

        conn.execute(
            "INSERT INTO data (metric_name, naive_date, val) VALUES (?1, ?2, ?3)",
            params![self.metric.name, self.when, self.value],
        )?;

        Ok(())
    }

    pub fn read(metric: Metric) -> rusqlite::Result<HashMap<TimePeriod, Datapoint>> {
        let conn = Connection::open(DATABASE_FILE)?;

        let mut stmt = conn.prepare("SELECT naive_date, val FROM data WHERE metric_name = ?1")?;

        let point_iter = stmt.query_map(params![metric.name], |row| {
            Ok(Datapoint {
                value: row.get(1)?,
                metric: metric.clone(),
                when: row.get(0)?,
            })
        })?;

        let mut found: HashMap<TimePeriod, Datapoint> = HashMap::new();
        for point in point_iter {
            if let Ok(f) = point {
                found.insert(TimePeriod::new(f.when(), &f.metric_info().frequency), f);
            }
        }
        Ok(found)
    }
}

impl Figure for Datapoint {
    fn metric_info(&self) -> &Metric {
        &self.metric
    }

    fn when(&self) -> &NaiveDate {
        &self.when
    }

    fn fig(&self) -> f64 {
        self.value
    }
}

impl Display for Datapoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        DisplayType::Rounded(self).fmt(f)
    }
}

pub enum DisplayType<'a, T: Figure> {
    Rounded(&'a T),
    Percentage(&'a T),
    DescribedPercentage(&'a T),
    PerFrequency(&'a T, &'a TimeFrequency),
}

impl<'a, T: Figure> Display for DisplayType<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DisplayType::Rounded(fig) => write!(f, "{:.1}", fig.fig()),
            DisplayType::Percentage(fig) => write!(f, "{:.1}%", (100.0 * fig.fig())),
            DisplayType::DescribedPercentage(fig) => {
                let description = if fig.fig() > 0.0 { "up" } else { "down" };
                write!(f, "{} {:.1}%", description, (100.0 * fig.fig().abs()))
            }
            DisplayType::PerFrequency(fig, freq) => write!(
                f,
                "{:.1} per {}",
                fig.averaged(freq),
                match freq {
                    TimeFrequency::Yearly => "year",
                    TimeFrequency::Quarterly => "quarter",
                    TimeFrequency::Monthly => "month",
                    TimeFrequency::Weekly => "week",
                    TimeFrequency::Daily => "day",
                }
            ),
        }
    }
}

pub enum FormatType {
    JoinStatement(String, String),
}

impl Display for FormatType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FormatType::JoinStatement(one, two) => write!(f, "{}—{}", one, two),
        }
    }
}

pub trait Component {}

impl Component for String {}
impl Component for FigChange {}

#[derive(Debug, Serialize, Deserialize)]
pub struct Statement<C: Component> {
    pub contents: Vec<C>,
}

impl<C: Component + Display> fmt::Display for Statement<C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.contents.iter().map(|v| v.to_string()).format(" ")
        )
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Paragraph<C: Component> {
    pub contents: Vec<Statement<C>>,
    pub name: String,
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn make_paragraph() {
        let description = String::from("Website users were");
        let comment = String::from("down");
        let figure = String::from("5%");

        let statement = Statement {
            contents: vec![description, comment, figure],
        };

        assert_eq!(
            statement.to_string(),
            String::from("Website users were down 5%")
        );

        assert_eq!(
            statement
                .contents
                .iter()
                .map(|id| id.to_string() + " ")
                .collect::<String>(),
            String::from("Website users were down 5% ")
        );
    }
}

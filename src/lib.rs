use core::fmt;
use std::{collections::HashMap, fmt::Display};

use chrono::{Datelike, IsoWeek, NaiveDate};
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
/// let time_period = TimePeriod::new(date, freq);
///
/// if let TimePeriod::Quarter(year, quarter) = time_period {
/// 	assert_eq!(year, 2022);
/// 	assert_eq!(quarter, 2);
/// }
///
/// ```
#[derive(Debug)]
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

    pub fn read(metric: Metric) -> rusqlite::Result<Vec<Datapoint>> {
        let conn = Connection::open(DATABASE_FILE)?;

        let mut stmt = conn.prepare("SELECT naive_date, val FROM data WHERE metric_name = ?1")?;

        let points: Result<Vec<_>, _> = stmt
            .query_map(params![metric.name], |row| {
                Ok(Datapoint {
                    value: row.get(1)?,
                    metric: metric.clone(),
                    when: row.get(0)?,
                })
            })?
            .collect();

        points
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

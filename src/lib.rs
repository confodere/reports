use core::fmt;
use std::{collections::HashMap, fmt::Display, vec};

use chrono::NaiveDate;
use serde::{Deserialize, Serialize};

use rusqlite::{params, Connection};

mod time_span;
pub use crate::time_span::{TimeFrequency, TimeSpan};

const DATABASE_FILE: &str = "ignore/data.db";

#[derive(Clone, Serialize, Deserialize, Debug, PartialEq, Eq)]
pub struct Metric {
    pub long_text: String,
    pub frequency: TimeFrequency,
    pub calculation_type: String,
}

impl Metric {
    pub fn new(long_text: String, frequency: TimeFrequency, calculation_type: String) -> Metric {
        Metric {
            long_text,
            frequency,
            calculation_type,
        }
    }

    /// Inserts current metric into sqlite3 database
    pub fn write(&self, name: String) -> rusqlite::Result<()> {
        let conn = Connection::open(DATABASE_FILE)?;

        conn.execute(
            r#"CREATE TABLE IF NOT EXISTS Metric (
            data_name TEXT NOT NULL,
            long_text TEXT NOT NULL,
            calculation_type TEXT NOT NULL,
            frequency TEXT NOT NULL,
            PRIMARY KEY (data_name, calculation_type, frequency),
            FOREIGN KEY (data_name) REFERENCES Data(name)
            )"#,
            [],
        )?;

        conn.execute(
            "INSERT INTO Metric (data_name, long_text, calculation_type, frequency) VALUES (?1, ?2, ?3, ?4)",
            params![name, self.frequency],
        )?;

        Ok(())
    }

    pub fn freq(&self) -> &TimeFrequency {
        &self.frequency
    }
}

pub trait Figure: Serialize {
    fn fig(&self) -> f64;
    fn render(&self, ctx: RenderContext) -> ComputedStringMetric;
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Change {
    old: f64,
    new: f64,
    span: TimeSpan,
    frequency: TimeFrequency,
}

impl Figure for Change {
    fn fig(&self) -> f64 {
        (self.new - self.old) / self.old
    }

    fn render(&self, ctx: RenderContext) -> ComputedStringMetric {
        ComputedStringMetric {
            fig: match ctx {
                RenderContext::Words => DisplayType::DescribedPercentage(self).to_string(),
                RenderContext::Numbers => DisplayType::Percentage(self).to_string(),
            },
            time_periods: vec![
                (self.span.clone() - self.frequency.clone()).to_string(),
                self.span.clone().to_string(),
            ],
        }
    }
}

impl<'a> Change {
    pub fn from(data: &Data, span: TimeSpan, frequency: TimeFrequency) -> Result<Change, String> {
        let datapoints = data
            .read_points((span.clone() - frequency.clone()).start(), span.end())
            .expect("Couldn't read underlying datapoint");

        let [new, old] = [&span, &(span.clone() - frequency.clone())].map(|p| {
            if let Some(point) = datapoints.get(p) {
                Ok(point.fig())
            } else {
                return Err(format!("Couldn't find datapoint: {:#?}", p));
            }
        });

        Ok(Change {
            old: old.unwrap(),
            new: new.unwrap(),
            span: span.clone(),
            frequency: frequency.clone(),
        })
    }
}

impl Display for Change {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", DisplayType::DescribedPercentage(self))
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ComputedStringMetric {
    pub fig: String,
    pub time_periods: Vec<String>,
}

pub enum RenderContext {
    Words,
    Numbers,
}

#[derive(Serialize, Deserialize)]
pub struct Fig {
    pub fig: f64,
}

impl Figure for Fig {
    fn fig(&self) -> f64 {
        self.fig
    }

    fn render(&self, _ctx: RenderContext) -> ComputedStringMetric {
        ComputedStringMetric {
            fig: self.fig().to_string(),
            time_periods: vec![],
        }
    }
}

pub enum Figures {
    Change(Change),
    Fig(Fig),
    Datapoint(Point),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Data {
    name: String,
    long_name: String,
    description: Option<String>,
    pub frequency: TimeFrequency,
    pub metrics: Vec<Metric>,
}

impl Data {
    pub fn new(
        name: String,
        long_name: String,
        description: Option<String>,
        frequency: TimeFrequency,
        metrics: Vec<Metric>,
    ) -> Data {
        Data {
            name,
            long_name,
            description,
            frequency,
            metrics,
        }
    }

    pub fn write(&self) -> rusqlite::Result<()> {
        let conn = Connection::open(DATABASE_FILE)?;

        conn.execute(
            r#"
        CREATE TABLE IF NOT EXISTS Data (
            name TEXT NOT NULL,
            long_name TEXT NOT NULL,
            description TEXT,
            frequency TEXT NOT NULL,
            PRIMARY KEY (name)
        )"#,
            [],
        )?;

        conn.execute(
            "INSERT INTO Data (name, long_name, description, frequency) VALUES (?1, ?2, ?3, ?4)",
            params![self.name, self.long_name, self.description, self.frequency],
        )?;

        Ok(())
    }

    pub fn read(name: String) -> rusqlite::Result<Data> {
        let conn = Connection::open(DATABASE_FILE)?;

        let mut data_stmt = conn.prepare(
            r#"
            SELECT name, long_name, description, frequency
            FROM Data
            WHERE name = ?1
        "#,
        )?;

        let mut data = data_stmt.query_row(params![name], |row| {
            Ok(Data {
                name: row.get(0)?,
                long_name: row.get(1)?,
                description: row.get(2)?,
                frequency: TimeFrequency::from_str(row.get(3)?)
                    .expect("Couldn't match TimeFrequency read from database"),
                metrics: vec![],
            })
        })?;

        let mut metric_stmt = conn.prepare(
            r#"
            SELECT long_text, frequency, calculation_type 
            FROM Metric 
            WHERE data_name = ?1
            "#,
        )?;

        let metrics =
            metric_stmt.query_and_then(params![name], |row| -> Result<_, rusqlite::Error> {
                Ok(Metric::new(
                    row.get(0)?,
                    TimeFrequency::from_str(row.get(1)?)
                        .expect("Couldn't match TimeFrequency read from database"),
                    row.get(2)?,
                ))
            })?;

        for metric in metrics {
            data.metrics.push(metric?);
        }
        Ok(data)
    }

    pub fn read_points(
        &self,
        start_date: &NaiveDate,
        end_date: NaiveDate,
    ) -> rusqlite::Result<HashMap<TimeSpan, Point>> {
        let conn = Connection::open(DATABASE_FILE)?;

        let mut stmt = conn.prepare(
            r#"
        SELECT naive_date, val
        FROM Point
        WHERE data_name = ?1 AND naive_date BETWEEN ?2 AND ?3
        "#,
        )?;

        let point_iter = stmt.query_map(params![self.name, start_date, end_date], |row| {
            Ok(Point::new(row.get(1)?, row.get(0)?))
        })?;

        let mut found: HashMap<TimeSpan, Point> = HashMap::new();
        for point in point_iter {
            if let Ok(point) = point {
                found.insert(TimeSpan::new(point.when(), self.frequency.clone()), point);
            } else {
                panic!("{:#?}", point);
            }
        }
        Ok(found)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Point {
    value: f64,
    when: NaiveDate,
}

impl Point {
    pub fn new(value: f64, when: NaiveDate) -> Point {
        Point { value, when }
    }

    pub fn write(&self, data: &Data) -> rusqlite::Result<()> {
        let conn = Connection::open(DATABASE_FILE)?;

        conn.execute(
            r#"CREATE TABLE IF NOT EXISTS Point (
            data_name TEXT NOT NULL, 
            naive_date TEXT NOT NULL, 
            val REAL, 
            PRIMARY KEY (data_name, naive_date), 
            FOREIGN KEY(data_name) REFERENCES Data(name))"#,
            [],
        )?;

        conn.execute(
            "INSERT INTO Point (data_name, naive_date, val) VALUES (?1, ?2, ?3)",
            params![data.name, self.when, self.value],
        )?;

        Ok(())
    }
}

impl Figure for Point {
    fn fig(&self) -> f64 {
        self.value
    }

    fn render(&self, _ctx: RenderContext) -> ComputedStringMetric {
        ComputedStringMetric {
            fig: self.value.to_string(),
            time_periods: vec![self.when.to_string()],
        }
    }
}

impl Point {
    fn when(&self) -> &NaiveDate {
        &self.when
    }
}

impl Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        DisplayType::Rounded(self).fmt(f)
    }
}

#[derive(Serialize)]
pub enum DisplayType<'a, T: Figure> {
    Rounded(&'a T),
    Percentage(&'a T),
    DescribedPercentage(&'a T),
    PerFrequency(&'a T, TimeSpan, TimeFrequency),
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
            DisplayType::PerFrequency(fig, span, freq) => write!(
                f,
                "{:.1} per {}",
                { fig.fig() * (span.clone() / freq.clone()) },
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

#[derive(Debug, Serialize, Deserialize)]
pub struct Statement {
    pub fig: f64,
}

impl Statement {
    fn _from_fig<T: Figure>(f: T) -> Statement {
        Statement { fig: f.fig() }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.fig)
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Paragraph<T: Figure> {
    pub contents: Vec<Statement>,
    pub name: String,
    pub fig: T,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn setup() -> (Data, NaiveDate, TimeSpan) {
        let metric = Metric::new(
            String::from("users"),
            TimeFrequency::Yearly,
            String::from("Change"),
        );
        let date = NaiveDate::from_ymd(2022, 2, 4);
        let span = TimeSpan::new(&date, TimeFrequency::Weekly);
        let data = Data::new(
            String::from("website_visits"),
            String::from("Visits to the website"),
            None,
            TimeFrequency::Weekly,
            vec![metric],
        );
        (data, date, span)
    }

    #[test]
    fn create_context() {
        let (data, _, span) = setup();

        let change = Change::from(&data, span.clone(), TimeFrequency::Yearly);
        if let Ok(change) = change {
            let data = change.render(RenderContext::Words);

            assert_eq!(data.fig, "up 64.6%");
        } else {
            panic!("Couldn't create change")
        }
    }

    #[test]
    fn read_data() {
        let name = String::from("website_visits");

        let metric_1 = Metric::new(
        String::from("Web visits are {{fig}} compared to this same reporting period last year ({{time_periods.0}})"),
        TimeFrequency::Yearly,
        String::from("Change"),
        );

        let metric_2 = Metric::new(
            String::from("Total website users were {{fig}}"),
            TimeFrequency::Weekly,
            String::from("Change"),
        );

        let metric_3 = Metric::new(
            String::from("we are now averaging {{fig}}"),
            TimeFrequency::Daily,
            String::from("AvgFreq"),
        );

        let data_1 = Data::new(
            name.clone(),
            String::from("Visits to the website"),
            Some(String::from("Total times people checked out the website")),
            TimeFrequency::Weekly,
            vec![metric_3, metric_2, metric_1],
        );

        let data_2 = Data::read(name).unwrap();

        assert_eq!(data_1, data_2);
    }
}

use anyhow::{anyhow, Error, Result};
use chrono::NaiveDate;
use core::fmt;
use parser::FigureExpression;
use pre_process::block::{CommandDisplay, CommandFreq};
use serde::{Deserialize, Serialize};
use std::str::FromStr;
use std::{collections::HashMap, fmt::Display};

use rusqlite::{params, Connection};

mod time_span;
pub use crate::time_span::{TimeFrequency, TimeSpan, TimeSpanIter};

mod pre_process;
pub use crate::pre_process::Processor;

pub mod functions;
pub mod parser;

const DATABASE_FILE: &str = "ignore/data.db";

const READ_DATA: &str = r#"
    SELECT name, long_name, description, frequency
    FROM Data
    WHERE name = ?1"#;

const READ_DATA_NAMES: &str = "
    SELECT DISTINCT name FROM Data;
";

const READ_METRIC: &str = r#"
    SELECT long_text, frequency, calculation_type 
    FROM Metric 
    WHERE data_name = ?1 AND calculation_type = ?2 AND frequency = ?3
    "#;

const READ_POINT: &str = r#"
    SELECT val, naive_date
    FROM Point
    WHERE data_name = ?1 AND naive_date BETWEEN ?2 AND ?3
    ORDER BY naive_date DESC
    LIMIT 1
"#;

#[derive(Clone, Serialize, Deserialize, Debug, PartialEq, Eq)]
pub struct Metric {
    pub data: Data,
    pub long_text: String,
    pub frequency: TimeFrequency,
    pub calculation_type: String,
}

impl Metric {
    pub fn new(
        data: Data,
        long_text: String,
        frequency: TimeFrequency,
        calculation_type: String,
    ) -> Metric {
        Metric {
            data,
            long_text,
            frequency,
            calculation_type,
        }
    }

    /// Inserts current metric into sqlite3 database
    pub fn write(&self) -> rusqlite::Result<()> {
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
            "INSERT OR IGNORE INTO Metric (data_name, long_text, calculation_type, frequency) VALUES (?1, ?2, ?3, ?4)",
            params![self.data.name, self.long_text, self.calculation_type, self.frequency],
        )?;

        Ok(())
    }

    pub fn read(
        name: String,
        date: &NaiveDate,
        calculation_type: String,
        frequency: TimeFrequency,
    ) -> rusqlite::Result<Metric> {
        let conn = Connection::open(DATABASE_FILE)?;

        let data = Data::from_name(&conn, &name, date)?;

        let mut stmt = conn.prepare(READ_METRIC)?;

        stmt.query_row(params![name, calculation_type, frequency], |row| {
            Ok(Metric::new(
                data,
                row.get(0)?,
                frequency,
                calculation_type.clone(),
            ))
        })
    }

    pub fn freq(&self) -> &TimeFrequency {
        &self.frequency
    }

    pub fn partial_name(&self) -> String {
        format!(
            "{}_{}_{}",
            self.data.name, self.calculation_type, self.frequency
        )
    }

    pub fn render(&self) -> String {
        match &self.calculation_type[..] {
            "Change" => Change::from(&self)
                .expect("Failed to create change")
                .to_string(),
            "AvgFreq" => AvgFreq::from(&self)
                .expect("Failed to create AvgFreq")
                .to_string(),
            _ => panic!("Unidentified figure type"),
        }
    }

    pub fn get_string(&self, name: &str) -> Result<String> {
        Ok(match name {
            "fig" => self.render(),
            "prev" => (&self.data.span - self.frequency).to_string(),
            "freq" => self.frequency.to_string(),
            "calc" => self.calculation_type.clone(),
            "name" => self.data.name.clone(),
            "Name" => self.data.long_name.clone(),
            "desc" => {
                if let Some(desc) = self.data.description.clone() {
                    desc
                } else {
                    "-".to_string()
                }
            }
            "span" => self.data.span.to_string(),
            _ => return Err(anyhow!("{} is not a recognised variable for Metric", name)),
        })
    }
}

pub trait Figure {
    fn fig(&self) -> f64;
    fn display_type(&self) -> DisplayType;

    fn from_inside(
        points: HashMap<TimeSpan, Point>,
        span: TimeSpan,
        frequency: TimeFrequency,
        depth: i32,
    ) -> Result<Vec<f64>> {
        TimeSpanIter::new(span, frequency, depth)
            .map(|span| {
                if let Some(point) = points.get(&span) {
                    Ok(point.fig())
                } else {
                    return Err(anyhow!(
                        "Couldn't find datapoint from span: {:#?} in {:#?}",
                        span,
                        points
                    ));
                }
            })
            .collect::<Result<Vec<_>>>()
    }
}

pub struct ShowFigure<F>(pub F);

impl<T: Figure> Display for ShowFigure<&T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0.display_type() {
            DisplayType::Rounded => write!(f, "{:.1}", self.0.fig()),
            DisplayType::DescribedRounded => {
                let description = if self.0.fig() > 0.0 { "up" } else { "down" };
                write!(f, "{} {:.1}", description, self.0.fig().abs())
            }
            DisplayType::Percentage => write!(f, "{:.1}%", (100.0 * self.0.fig())),
            DisplayType::DescribedPercentage => {
                let description = if self.0.fig() > 0.0 { "up" } else { "down" };
                write!(f, "{} {:.1}", description, (100.0 * self.0.fig().abs()))
            }
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Change {
    old: f64,
    new: f64,
    span: TimeSpan,
    frequency: TimeFrequency,
    display_type: DisplayType,
}

impl Figure for Change {
    fn fig(&self) -> f64 {
        (self.new - self.old) / self.old
    }

    fn display_type(&self) -> DisplayType {
        self.display_type
    }
}

impl TryFrom<CommandFreq> for Change {
    type Error = Error;

    /// Tries to convert CommandFreq into Change
    /// Defaults display_type to DisplayType::Percentage if None
    fn try_from(value: CommandFreq) -> Result<Self, Self::Error> {
        let points = value.data.read_points(
            (&value.data.span - value.frequency).start(),
            value.data.span.end(),
        )?;
        let vals = Change::from_inside(points, value.data.span, value.frequency, 2)?;

        Ok(Change {
            old: vals[1],
            new: vals[0],
            span: value.data.span,
            frequency: value.frequency,
            display_type: match value.display_type {
                Some(v) => v,
                None => DisplayType::Percentage,
            },
        })
    }
}

impl Display for Change {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ShowFigure(self).fmt(f)
    }
}

impl<'a> Change {
    pub fn from_figure_expresssion(exp: &FigureExpression, data: &Data) -> Result<Self> {
        let points = data.read_points((&data.span - exp.frequency).start(), data.span.end())?;
        let vals = Change::from_inside(points, data.span, exp.frequency, 2)?;

        Ok(Change {
            old: vals[1],
            new: vals[0],
            span: data.span,
            frequency: exp.frequency,
            display_type: DisplayType::Percentage,
        })
    }

    pub fn from(metric: &Metric) -> Result<Change> {
        let datapoints = metric
            .data
            .read_points(
                (&metric.data.span - metric.frequency).start(),
                metric.data.span.end(),
            )
            .expect("Couldn't read underlying datapoint");

        let vals = Change::from_inside(datapoints, metric.data.span, metric.frequency, 2)?;

        Ok(Change {
            old: vals[1],
            new: vals[0],
            span: metric.data.span,
            frequency: metric.frequency,
            display_type: DisplayType::Percentage,
        })
    }
}
#[derive(Serialize, Deserialize)]
pub struct AvgFreq {
    fig: f64,
    span: TimeSpan,
    frequency: TimeFrequency,
    display_type: DisplayType,
}

impl Figure for AvgFreq {
    fn fig(&self) -> f64 {
        self.fig * (self.span.clone() / self.frequency.clone())
    }

    fn display_type(&self) -> DisplayType {
        self.display_type
    }
}

impl TryFrom<CommandFreq> for AvgFreq {
    type Error = Error;

    /// Tries to convert CommandFreq into AvgFreq
    /// Defaults display_type to DisplayType::Rounded if None
    fn try_from(value: CommandFreq) -> Result<Self, Self::Error> {
        let point = value.data.read_point()?;
        Ok(AvgFreq {
            fig: point.fig(),
            span: value.data.span,
            frequency: value.frequency,
            display_type: match value.display_type {
                Some(v) => v,
                None => DisplayType::Rounded,
            },
        })
    }
}

impl Display for AvgFreq {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.display_type() {
            DisplayType::Rounded => write!(f, "{:.1}", self.fig()),
            DisplayType::DescribedRounded => {
                let freq = match self.frequency {
                    TimeFrequency::Yearly => "year",
                    TimeFrequency::Quarterly => "quarter",
                    TimeFrequency::Monthly => "month",
                    TimeFrequency::Weekly => "week",
                    TimeFrequency::Daily => "day",
                };
                write!(f, "{:.1} per {}", self.fig(), freq)
            }
            DisplayType::Percentage => {
                panic!("AvgFreq cannot be represented as a percentage")
            }
            DisplayType::DescribedPercentage => {
                panic!("AvgFreq cannot be represented as a described percentage")
            }
        }
    }
}

impl AvgFreq {
    pub fn from_figure_expresssion(exp: &FigureExpression, data: &Data) -> Result<Self> {
        let point = data.read_point()?;
        Ok(AvgFreq {
            fig: point.fig(),
            span: data.span,
            frequency: exp.frequency,
            display_type: DisplayType::Rounded,
        })
    }

    pub fn from(metric: &Metric) -> Result<AvgFreq, String> {
        let datapoints = metric
            .data
            .read_points(metric.data.span.start(), metric.data.span.end())
            .expect("Failed to read datapoints for AvgFreq");

        if let Some(point) = datapoints.get(&metric.data.span) {
            Ok(AvgFreq {
                fig: point.fig(),
                span: metric.data.span,
                frequency: metric.frequency,
                display_type: DisplayType::Rounded,
            })
        } else {
            Err(String::from("Failed to create AvgFreq"))
        }
    }
}
#[derive(Debug, Serialize, Deserialize)]
pub struct ComputedStringMetric {
    pub fig: String,
    pub time_periods: Vec<String>,
    pub partial_name: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum RenderContext {
    Words,
    Numbers,
}

impl RenderContext {
    fn _from_str(s: &str) -> Option<Self> {
        Some(match s {
            "Words" => RenderContext::Words,
            "Numbers" => RenderContext::Numbers,
            _ => return None,
        })
    }
}

#[derive(Serialize, Deserialize)]
pub struct Fig {
    pub fig: f64,
    display_type: DisplayType,
}

impl Fig {
    pub fn new(fig: f64, display_type: Option<DisplayType>) -> Self {
        Fig {
            fig,
            display_type: match display_type {
                Some(display_type) => display_type,
                None => DisplayType::Rounded,
            },
        }
    }
}

impl Figure for Fig {
    fn fig(&self) -> f64 {
        self.fig
    }
    fn display_type(&self) -> DisplayType {
        self.display_type
    }
}

impl TryFrom<CommandDisplay> for Fig {
    type Error = Error;

    fn try_from(value: CommandDisplay) -> Result<Self, Self::Error> {
        Ok(Fig::new(value.data.read_point()?.fig(), value.display_type))
    }
}

impl Display for Fig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ShowFigure(self).fmt(f)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize, Default)]
pub struct Data {
    pub name: String,
    pub long_name: String,
    pub description: Option<String>,
    pub span: TimeSpan,
}

impl Data {
    pub fn new(
        name: String,
        long_name: String,
        description: Option<String>,
        frequency: TimeFrequency,
        date: &NaiveDate,
    ) -> Data {
        Data {
            name,
            long_name,
            description,
            span: TimeSpan::new(&date, frequency),
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
            "INSERT OR IGNORE INTO Data (name, long_name, description, frequency) VALUES (?1, ?2, ?3, ?4)",
            params![
                self.name,
                self.long_name,
                self.description,
                self.span.freq()
            ],
        )?;

        Ok(())
    }

    pub fn read(name: &String, date: &NaiveDate) -> rusqlite::Result<Data> {
        let conn = Connection::open(DATABASE_FILE)?;

        Data::from_name(&conn, name, date)
    }

    pub fn read_names() -> rusqlite::Result<Vec<String>> {
        let conn = Connection::open(DATABASE_FILE)?;
        let mut stmt = conn.prepare(READ_DATA_NAMES)?;

        let rows = stmt.query_and_then([], |row| row.get::<_, String>(0))?;

        let mut names = Vec::new();
        for name in rows {
            names.push(name?)
        }

        Ok(names)
    }

    fn from_name(conn: &Connection, name: &String, date: &NaiveDate) -> rusqlite::Result<Data> {
        let mut stmt = conn.prepare(READ_DATA)?;

        stmt.query_row(params![name], |row| {
            let freq: String = row.get(3)?;
            Ok(Data {
                name: row.get(0)?,
                long_name: row.get(1)?,
                description: row.get(2)?,
                span: TimeSpan::new(
                    date,
                    freq.parse::<TimeFrequency>()
                        .expect("Couldn't match TimeFrequency read from database"),
                ),
            })
        })
    }

    pub fn read_to_metric(name: String, date: &NaiveDate) -> rusqlite::Result<Vec<Metric>> {
        let conn = Connection::open(DATABASE_FILE)?;

        let mut data_stmt = conn.prepare(
            r#"
            SELECT name, long_name, description, frequency
            FROM Data
            WHERE name = ?1
        "#,
        )?;

        let data = data_stmt.query_row(params![name], |row| {
            let freq: String = row.get(3)?;
            Ok(Data {
                name: row.get(0)?,
                long_name: row.get(1)?,
                description: row.get(2)?,
                span: TimeSpan::new(
                    date,
                    freq.parse::<TimeFrequency>()
                        .expect("Couldn't match TimeFrequency read from database"),
                ),
            })
        })?;

        let mut metric_stmt = conn.prepare(
            r#"
            SELECT long_text, frequency, calculation_type 
            FROM Metric 
            WHERE data_name = ?1
            "#,
        )?;

        let metrics = metric_stmt
            .query_and_then(params![name], |row| -> Result<_, rusqlite::Error> {
                let freq: String = row.get(1)?;

                Ok(Metric::new(
                    data.clone(),
                    row.get(0)?,
                    freq.parse::<TimeFrequency>()
                        .expect("Couldn't match TimeFrequency read from database"),
                    row.get(2)?,
                ))
            })?
            .collect::<Result<Vec<_>, _>>();

        metrics
    }

    pub fn read_point(&self) -> rusqlite::Result<Point> {
        let conn = Connection::open(DATABASE_FILE)?;

        let mut stmt = conn.prepare(READ_POINT)?;

        stmt.query_row(
            params![self.name, self.span.start(), self.span.end()],
            |row| {
                Ok(Point {
                    value: row.get(0)?,
                    when: row.get(1)?,
                })
            },
        )
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
                found.insert(TimeSpan::new(point.when(), self.span.freq()), point);
            } else {
                panic!("{:#?}", point);
            }
        }
        Ok(found)
    }

    fn get_string(&self, name: String) -> Result<String> {
        Ok(match name.as_str() {
            "name" => self.name.clone(),
            "Name" => self.long_name.clone(),
            "desc" => {
                if let Some(desc) = self.description.clone() {
                    desc
                } else {
                    "-".to_string()
                }
            }
            "span" => self.span.to_string(),
            _ => return Err(anyhow!("{} is not a recognised variable for Data", name)),
        })
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
            "INSERT OR IGNORE INTO Point (data_name, naive_date, val) VALUES (?1, ?2, ?3)",
            params![data.name, self.when, self.value],
        )?;

        Ok(())
    }
}

impl Figure for Point {
    fn fig(&self) -> f64 {
        self.value
    }

    fn display_type(&self) -> DisplayType {
        DisplayType::Rounded
    }
}

impl Point {
    fn when(&self) -> &NaiveDate {
        &self.when
    }
}

impl Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ShowFigure(self).fmt(f)
    }
}

#[derive(Serialize, Debug, Clone, Copy, Deserialize)]
pub enum DisplayType {
    Rounded,
    DescribedRounded,
    Percentage,
    DescribedPercentage,
}

impl Display for DisplayType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self)
    }
}

impl FromStr for DisplayType {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "rounded" => Ok(Self::Rounded),
            "describedrounded" => Ok(Self::DescribedRounded),
            "percentage" => Ok(Self::Percentage),
            "describedpercetange" => Ok(Self::DescribedPercentage),
            _ => Err(anyhow!("{} is not a valid DisplayType", s)),
        }
    }
}

pub enum FormatType {
    Sentence,
    Table,
    Description,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Statement {
    pub name: String,
    pub calculation_type: String,
    pub frequency: TimeFrequency,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Paragraph {
    pub name: String,
    pub contents: Vec<Vec<Statement>>,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn setup() -> Metric {
        let date = NaiveDate::from_ymd(2022, 2, 4);
        let data = Data::new(
            String::from("website_visits"),
            String::from("Visits to the website"),
            None,
            TimeFrequency::Weekly,
            &date,
        );

        let metric = Metric::new(
            data,
            String::from(""),
            TimeFrequency::Yearly,
            String::from("Change"),
        );
        metric
    }

    #[test]
    fn create_context() {
        let metric = setup();

        let change = Change::from(&metric);
        if let Ok(change) = change {
            let data = change.to_string();

            assert_eq!(data, "up 64.6%");
        } else {
            panic!("Couldn't create change")
        }
    }

    #[test]
    fn read_data() {
        let name = String::from("website_visits");
        let date = NaiveDate::from_ymd(2022, 2, 4);

        let data_1 = Data::new(
            name.clone(),
            String::from("Visits to the website"),
            Some(String::from("Total times people checked out the website")),
            TimeFrequency::Weekly,
            &date,
        );

        let metric_1 = Metric::new(
            data_1.clone(),
        String::from("Web visits are {{fig}} compared to this same reporting period last year ({{prev}})"),
        TimeFrequency::Yearly,
        String::from("Change"),
        );

        let metric_2 = Metric::new(
            data_1.clone(),
            String::from("Total website users were {{fig}}"),
            TimeFrequency::Weekly,
            String::from("Change"),
        );

        let metric_3 = Metric::new(
            data_1.clone(),
            String::from("we are now averaging {{fig}}"),
            TimeFrequency::Daily,
            String::from("AvgFreq"),
        );

        let metrics = Data::read_to_metric(name, &date).unwrap();

        assert_eq!(vec![metric_3, metric_2, metric_1], metrics);
    }
}

pub mod functions;
pub mod parser;
pub mod pre_process;
pub mod time_span;

use crate::functions::{AvgFreq, Change, Figure, ShowFigure};
pub use crate::time_span::{TimeFrequency, TimeSpan};
use anyhow::{anyhow, Result};
use chrono::NaiveDate;
use functions::display::RenderContext;
use rusqlite::{params, Connection};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::{self, Display};

const DATABASE_FILE: &str = "ignore/data.db";

const READ_DATA: &str = r#"
    SELECT name, long_name, description, frequency
    FROM Data
    WHERE name = ?1"#;

const READ_DATA_NAMES: &str = "
    SELECT name, long_name FROM Data;
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

#[derive(Debug, Serialize, Deserialize)]
pub struct ComputedStringMetric {
    pub fig: String,
    pub time_periods: Vec<String>,
    pub partial_name: String,
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

    pub fn read_names() -> Result<HashMap<String, String>> {
        let conn = Connection::open(DATABASE_FILE)?;
        let mut stmt = conn.prepare(READ_DATA_NAMES)?;

        let rows = stmt
            .query_and_then([], |row| {
                Ok((row.get::<_, String>(0)?, row.get::<_, String>(1)?))
            })?
            .collect::<Result<HashMap<_, _>>>()?;

        Ok(rows)
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

    pub fn get_string(&self, name: String) -> Result<String> {
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

    fn display_type(&self) -> RenderContext {
        RenderContext::Numbers
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

            assert_eq!(data, "64.6%");
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

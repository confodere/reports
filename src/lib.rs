use core::fmt;
use std::{collections::HashMap, fmt::Display};

use chrono::NaiveDate;
use itertools::Itertools;
use serde::{Deserialize, Serialize};

use rusqlite::{params, Connection, Error, ToSql};

const DATABASE_FILE: &str = "ignore/data.db";

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum TimeFrequency {
    Yearly,
    Monthly,
    Weekly,
    Daily,
}

impl ToSql for TimeFrequency {
    fn to_sql(&self) -> rusqlite::Result<rusqlite::types::ToSqlOutput<'_>> {
        Ok(format!("{:#?}", self).into())
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

    fn metric_info(&self) -> &Metric;
    fn when(&self) -> &NaiveDate;
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
            let freq = match row.get::<_, String>(3)?.as_str() {
                "Yearly" => TimeFrequency::Yearly,
                "Monthly" => TimeFrequency::Monthly,
                "Weekly" => TimeFrequency::Weekly,
                "Daily" => TimeFrequency::Daily,
                _ => return Err(Error::InvalidQuery),
            };
            Ok(Metric::new(row.get(0)?, row.get(1)?, row.get(2)?, freq))
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

    fn diff(&self) -> f64 {
        (self.new - self.old) / self.old
    }

    fn diff_format(&self) -> String {
        let diff = self.diff();
        let mut output = String::new();
        output.push_str({
            if diff > 0.0 {
                "up"
            } else {
                "down"
            }
        });
        output.push_str(&format!(" {:.1}%", (100.0 * diff.abs())));
        output
    }
}

impl Display for FigChange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.format(&self.metric.print_text, self.diff_format())
        )
    }
}

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

    pub fn value(&self) -> f64 {
        self.value
    }

    pub fn when(&self) -> NaiveDate {
        self.when
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

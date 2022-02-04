use chrono::{Local, NaiveDate};
use lazy_static::lazy_static;
use mdbook::book::{BookItem, Chapter};
use mdbook::preprocess::Preprocessor;
use regex::Regex;
use std::error::Error;
use toml::{map::Map, value::Value};

use crate::{Data, Figure, Metric, RenderContext, TimeFrequency};

type BoxResult<T> = Result<T, Box<dyn Error>>;

pub struct Processor;

impl Processor {
    pub fn new() -> Self {
        Self
    }
}

impl Preprocessor for Processor {
    fn name(&self) -> &str {
        "reports"
    }

    fn run(
        &self,
        ctx: &mdbook::preprocess::PreprocessorContext,
        mut book: mdbook::book::Book,
    ) -> mdbook::errors::Result<mdbook::book::Book> {
        // In testing we want to tell the preprocessor to blow up by setting a particular config value
        let app_cfg = if let Some(app_cfg) = ctx.config.get_preprocessor(self.name()) {
            if app_cfg.contains_key("blow-up") {
                anyhow::bail!("Boom!!1!");
            }
            Some(app_cfg)
        } else {
            None
        };

        book.for_each_mut(|section| {
            if let BookItem::Chapter(ref mut ch) = *section {
                if let Err(e) = pre_process(ch, app_cfg) {
                    eprintln!("report_process error: {:?}", e);
                }
                ch.content.push_str("\n ### An addition \n \n Love it");
            }
        });

        Ok(book)
    }

    fn supports_renderer(&self, _renderer: &str) -> bool {
        _renderer != "not-supported"
    }
}

struct Substitutions {
    start: usize,
    end: usize,
    text: String,
}

fn pre_process(chapter: &mut Chapter, cfg: Option<&Map<String, Value>>) -> BoxResult<()> {
    let date = cfg_date(cfg)?;
    lazy_static! {
        static ref RE: Regex = Regex::new(
            r"(?x)  # Ignore whitespace
        (?:\{\{\#)  # {{#
        ([^\s]*)\s  # Keyword
        (.+?)        # Capture one or more of anything
        (?:\}\})    # }}
        "
        )
        .unwrap();
    }

    // Call the appropriate substitution function for each match,
    // passing on any non-keywords in the match
    let mut subs: Vec<Substitutions> = Vec::new();
    for cap in RE.captures_iter(&chapter.content) {
        let keyword = match cap.get(1) {
            None => {
                eprintln!("Missing Keyword in preprocessor");
                continue;
            }
            Some(word) => word.as_str(),
        };
        let words = cap[2].split(" ").collect::<Vec<&str>>();
        let substitute = match keyword {
            "table" => make_table(words, &date),
            "seti" => make_single_table(words, &date),
            _ => {
                eprintln!("Failed to match keyword {}", keyword);
                continue;
            }
        }?;
        subs.push(Substitutions {
            start: cap.get(0).unwrap().start(),
            end: cap.get(0).unwrap().end(),
            text: substitute,
        });
    }

    let mut added = 0;
    for sub in subs {
        chapter
            .content
            .replace_range((sub.start + added)..(sub.end + added), &sub.text);
        added += sub.text.len() - (sub.end - sub.start);
    }

    Ok(())
}

fn cfg_now() -> NaiveDate {
    Local::today().naive_local()
}

fn cfg_date(cfg: Option<&Map<String, Value>>) -> BoxResult<NaiveDate> {
    Ok(if let Some(cfg) = cfg {
        if let Some(date) = cfg.get("date") {
            if let Some(date) = date.as_str() {
                NaiveDate::parse_from_str(&date.to_string(), "%Y-%m-%d")?
            } else {
                cfg_now()
            }
        } else {
            cfg_now()
        }
    } else {
        cfg_now()
    })
}

/// Expects words to be a list of names of Data
fn make_table(words: Vec<&str>, date: &NaiveDate) -> BoxResult<String> {
    let mut table =
        String::from("\n| Source | Number | Description | \n| ----- | ----- | ----- | \n");

    for word in &words {
        let data = Data::read(&word.to_string(), &date)?;

        let point = data.read_point()?;

        let row = if let Some(description) = data.description {
            format!(
                "| {} | {} | {} | \n",
                data.long_name,
                point.fig().to_string(),
                description
            )
        } else {
            format!("| {} | {} |  | \n", data.long_name, point.fig().to_string(),)
        };
        table.push_str(&row);
    }
    table.push_str("\n");
    Ok(table)
}

fn make_single_table(words: Vec<&str>, date: &NaiveDate) -> BoxResult<String> {
    let mut table = String::from("\n| Calculation | Frequency | Value | \n| --- | --- | --- | \n");
    for word in words {
        let words = word.split(",").collect::<Vec<&str>>();
        if let [name, calc, freq] = &words[..] {
            let freq_obj = TimeFrequency::from_str(freq.to_string())?;
            //let rc = RenderContext::from_str(rc).unwrap_or(RenderContext::Numbers);
            let metric = Metric::read(name.to_string(), &date, calc.to_string(), freq_obj)?;

            table.push_str(&format!(
                "| {} | {} | {} |",
                calc,
                freq,
                metric.render(RenderContext::Numbers)
            ));
            table.push_str("\n");
        } else {
            return Err(mdbook::errors::Error::msg("Not enough arguments passed").into());
        }
    }
    Ok(table)
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{Point, TimeSpan};

    fn setup() -> BoxResult<()> {
        let date = Local::today().naive_local();
        let freq = TimeFrequency::Weekly;
        let span = TimeSpan::new(&date, freq);

        let data_1 = Data::new(
            String::from("cat_purrs"),
            String::from("Cat Purrs"),
            Some(String::from(
                "A measure of the number of times my cat purred",
            )),
            freq,
            &date,
        );
        let data_2 = Data::new(
            String::from("dog_woofs"),
            String::from("Dog Woofs"),
            Some(String::from(
                "A measure of the number of times my dog woofed",
            )),
            freq,
            &date,
        );
        let data_3 = Data::new(
            String::from("fish_zooms"),
            String::from("Fish Zooms"),
            Some(String::from(
                "A measure of the number of times my fish zoomed",
            )),
            freq,
            &date,
        );

        data_1.write()?;
        data_2.write()?;
        data_3.write()?;

        Point::new(10.0, date.clone()).write(&data_1)?;
        Point::new(8.0, *(&span - freq).start()).write(&data_1)?;
        Point::new(3.0, *(&span - TimeFrequency::Quarterly).start()).write(&data_1)?;

        Point::new(25.0, date.clone()).write(&data_2)?;
        Point::new(64.0, *(&span - freq).start()).write(&data_2)?;
        Point::new(3.0, *(&span - TimeFrequency::Yearly).start()).write(&data_2)?;

        Point::new(3.0, date.clone()).write(&data_3)?;
        Point::new(3.5, *(&span - freq).start()).write(&data_3)?;
        Point::new(4.2, *(&span - TimeFrequency::Monthly).start()).write(&data_3)?;

        Metric::new(
            data_1.clone(),
            String::from("Weekly change was {{fig}}"),
            freq,
            String::from("Change"),
        )
        .write()?;

        Metric::new(
            data_1.clone(),
            String::from("Change from {{prev}} was {{fig}}"),
            TimeFrequency::Quarterly,
            String::from("Change"),
        )
        .write()?;

        Metric::new(
            data_2.clone(),
            String::from("Change from {{prev}} was {{fig}}"),
            TimeFrequency::Yearly,
            String::from("Change"),
        )
        .write()?;

        Metric::new(
            data_3.clone(),
            String::from("Daily equivalent is {{fig}}"),
            TimeFrequency::Daily,
            String::from("AvgFreq"),
        )
        .write()?;

        Ok(())
    }

    #[test]
    fn test_table() {
        setup().unwrap();

        let date = Local::today().naive_local();
        let words = vec!["cat_purrs", "dog_woofs", "fish_zooms"];

        let table = make_table(words, &date).unwrap();
        let expected_table = String::from(
            "
| Source | Number | Description | 
| ----- | ----- | ----- | 
| Cat Purrs | 10 | A measure of the number of times my cat purred | 
| Dog Woofs | 25 | A measure of the number of times my dog woofed | 
| Fish Zooms | 3 | A measure of the number of times my fish zoomed | 

",
        );

        assert_eq!(table, expected_table);
    }

    #[test]
    fn test_single_table() {
        setup().unwrap();

        let date = Local::today().naive_local();
        let words = vec![
            "cat_purrs,Change,Quarterly",
            "cat_purrs,Change,Weekly",
            "dog_woofs,Change,Yearly",
            "fish_zooms,AvgFreq,Daily",
        ];

        let table = make_single_table(words, &date).unwrap();
        let expected_table = String::from(
            "
| Calculation | Frequency | Value | 
| --- | --- | --- | 
| Change | Quarterly | 233.3% |
| Change | Weekly | 25.0% |
| Change | Yearly | 733.3% |
| AvgFreq | Daily | 0.4 |
",
        );

        assert_eq!(table, expected_table);
    }
}

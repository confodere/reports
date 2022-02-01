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
    lazy_static! {
        static ref RE: Regex = Regex::new(
            r"(?x)  # Ignore whitespace
        (?:\{\{\#)  # {{#
        (.+)        # Capture one or more of anything
        (?:\}\})    # }}
        "
        )
        .unwrap();
    }

    // Call the appropriate substitution function for each match,
    // passing on any non-keywords in the match
    let mut subs: Vec<Substitutions> = Vec::new();
    for cap in RE.captures_iter(&chapter.content) {
        let mut words = cap[1].split(" ");
        let keyword = words.next().expect("Missing keyword");
        let words = words.collect::<Vec<&str>>();
        let substitute = match keyword {
            "table" => make_table(words, cfg),
            "seti" => make_single_table(words, cfg),
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
fn make_table(words: Vec<&str>, cfg: Option<&Map<String, Value>>) -> BoxResult<String> {
    let date = cfg_date(cfg)?;

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

fn make_single_table(words: Vec<&str>, cfg: Option<&Map<String, Value>>) -> BoxResult<String> {
    let date = cfg_date(cfg)?;
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

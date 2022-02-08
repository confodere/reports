use crate::parser::{
    find_expressions, key_var_list, BasicSubstitute, Block, Expression, ExpressionType,
    Substitution,
};
use crate::{parser::find_blocks, Data, Figure, Metric, TimeFrequency};
use anyhow::{anyhow, Result};
use chrono::{Local, NaiveDate};
use mdbook::book::{BookItem, Chapter};
use mdbook::preprocess::Preprocessor;
use toml::{map::Map, value::Value};

pub mod block;

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
        let date = cfg_date(app_cfg)?;

        book.for_each_mut(|section| {
            if let BookItem::Chapter(ref mut ch) = *section {
                /*if let Err(e) = pre_process(ch, app_cfg) {
                    eprintln!("report_process error: {:?}", e);
                }*/
                if let Err(e) = pre_process_blocks(ch, &date) {
                    eprintln!("report_process block error: {:#?}", e);
                }
                if let Err(e) = pre_process_funcs(ch, &date) {
                    eprintln!("report_process func error: {:#?}", e)
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

fn pre_process_blocks(chapter: &mut Chapter, date: &NaiveDate) -> Result<()> {
    if let Some(pre_blocks) = find_blocks(&chapter.content.clone().as_str())? {
        let mut block_sub = Substitution {
            template: &mut chapter.content,
            added: 0,
        };
        // Thrown out value is text leftover after last block
        let (_, (padding, vars)) = pre_blocks;

        // Each block context
        for ((vars, template), start, end) in vars {
            let (block_name, vars) = key_var_list(vars)?;

            let mut block = Block {
                block_name,
                vars,
                template: template.to_string(),
                start: start + padding.len(),
                end: end + padding.len(),
            };

            let data = Data::read(&block_name.to_string(), date)?;

            // Each expression
            if let Some((_, (padding, exps))) = find_expressions(template)? {
                let mut exp_sub = Substitution {
                    template: &mut block.template,
                    added: 0,
                };

                for ((exp, _), start, end) in exps {
                    let exp = exp.parse::<ExpressionType>()?;
                    let exp = Expression {
                        vars: exp,
                        start: start + padding.len(),
                        end: end + padding.len(),
                        data: &data,
                    };

                    exp_sub.substitute(&exp)?;
                }
                block_sub.substitute(&block)?;
            }
        }
    }
    Ok(())
}

fn pre_process_funcs(chapter: &mut Chapter, date: &NaiveDate) -> Result<()> {
    if let Some((junk, (_, exps))) = find_expressions(&chapter.content.clone().as_str())? {
        let mut exp_sub = Substitution {
            template: &mut chapter.content,
            added: 0,
        };

        for ((exp, _), start, end) in exps {
            let (func_name, vars) = key_var_list(exp)?;
            let text = match func_name {
                "table" => make_table(vars, date),
                _ => return Err(anyhow!("Unrecognised function: {}", func_name)),
            }?;
            let sub = BasicSubstitute {
                start: start + junk.len(),
                end: end + junk.len(),
                text,
            };
            exp_sub.substitute(&sub)?;
        }
    }
    Ok(())
}

fn cfg_now() -> NaiveDate {
    Local::today().naive_local()
}

fn cfg_date(cfg: Option<&Map<String, Value>>) -> Result<NaiveDate> {
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
fn make_table(words: Vec<&str>, date: &NaiveDate) -> Result<String> {
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

fn _make_single_table(words: Vec<&str>, date: &NaiveDate) -> Result<String> {
    let mut table = String::from("\n| Calculation | Frequency | Value | \n| --- | --- | --- | \n");
    for word in words {
        let words = word.split(",").collect::<Vec<&str>>();
        if let [name, calc, freq] = &words[..] {
            let freq_obj = freq.parse::<TimeFrequency>()?;
            //let rc = RenderContext::from_str(rc).unwrap_or(RenderContext::Numbers);
            let metric = Metric::read(name.to_string(), &date, calc.to_string(), freq_obj)?;

            table.push_str(&format!("| {} | {} | {} |", calc, freq, metric.render()));
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

    fn setup() -> Result<()> {
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

        let date = NaiveDate::from_ymd(2022, 2, 4);
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

        let mut ch = Chapter::new(
            "test",
            "{{#table cat_purrs }}
            {{Change Quarterly}},
            {{Change Weekly}},
            {{AvgFreq Monthly}}
            {{/#}}"
                .to_string(),
            "test.md",
            vec![],
        );
        let date = NaiveDate::from_ymd(2022, 2, 4);

        let expected_table = String::from(
            "
| Calculation | Frequency | Value | 
| --- | --- | --- | 
| Change | Quarterly | 2.3 |
| Change | Weekly | 0.0 |
| Change | Yearly | 7.3 |
| AvgFreq | Daily | 0.4 |
",
        );

        assert_eq!(pre_process_blocks(&mut ch, &date).unwrap(), ());
        assert_eq!(ch.content, expected_table);
    }

    #[test]
    fn test_pre_process_blocks() {
        let mut ch = Chapter::new(
            "test",
            "{{#cat_purrs}}
        Weekly change in cat purrs was {{Change Weekly DescribedPercentage}}.
        Quarterly change in cat purrs was {{Change Quarterly}} compared to {{prev Quarterly}}.{{/#}}
        "
            .to_string(),
            "test.md",
            vec![],
        );

        let date = NaiveDate::from_ymd(2022, 2, 4);

        assert_eq!(pre_process_blocks(&mut ch, &date).unwrap(), ());
        assert_eq!(
            ch.content,
            "
        Weekly change in cat purrs was up 25.0%.
        Quarterly change in cat purrs was up 233.3% compared to 2021 (25th October to 31st October).
        "
        );
    }

    #[test]
    fn test_block_vars() {
        let mut ch = Chapter::new(
            "test",
            "{{# fish_zooms }}
            {{AvgFreq Daily}}
            {{Change Weekly}}
            {{name}}
            {{Name}}
            {{desc}}
            {{span}}
            {{/#}}
            
            {{# cat_purrs }}
            {{Name}} are {{Change Quarterly}} since {{prev Quarterly}}
            {{/#}}"
                .to_string(),
            "test.md",
            vec![],
        );
        let date = NaiveDate::from_ymd(2022, 2, 4);

        assert_eq!(pre_process_blocks(&mut ch, &date).unwrap(), ());
        assert_eq!(
            ch.content,
            "
            0.4 per day
            down 14.3%
            fish_zooms
            Fish Zooms
            A measure of the number of times my fish zoomed
            2022 (31st January to 6th February)
            
            
            
            Cat Purrs are up 233.3% since 2021 (25th October to 31st October)
            "
        );
    }

    #[test]
    fn test_block_vars_2() {
        let mut ch = Chapter::new(
            "test",
            "
# Sample
- {{# website_visits }}Total website users were {{Change Weekly}}, we are now averaging {{AvgFreq Daily}}.
- {{Name}} are {{Change Yearly}} compared to this same reporting period last year {{prev Yearly}}.{{/#}}
            
- {{# cat_purrs }}{{Name}} are {{Change Quarterly}} since {{prev Quarterly}}{{/#}}"
                .to_string(),
            "test.md",
            vec![],
        );
        let date = NaiveDate::from_ymd(2022, 2, 4);

        assert_eq!(pre_process_blocks(&mut ch, &date).unwrap(), ());
        assert_eq!(
            ch.content,
            "
# Sample
- Total website users were down 12.9%, we are now averaging 833.7 per day.
- Visits to the website are up 64.6% compared to this same reporting period last year 2021 (1st February to 7th February).
            
- Cat Purrs are up 233.3% since 2021 (25th October to 31st October)"
        );
    }

    #[test]
    fn test_block_table() {
        let mut ch = Chapter::new(
            "test",
            "{{#table Change Rounded}}
            {{col Daily Weekly Monthly Quarterly Yearly}}
            {{row cat_purrs dog_woofs fish_zooms}}
            {{/#}}"
                .to_string(),
            "test.md",
            vec![],
        );
        let date = NaiveDate::from_ymd(2022, 2, 4);

        assert_eq!(pre_process_funcs(&mut ch, &date).unwrap(), ());
        assert_eq!(
            ch.content,
            "
| Source | Number | Description | 
| ----- | ----- | ----- | 
| Cat Purrs | 10 | A measure of the number of times my cat purred | 
| Dog Woofs | 25 | A measure of the number of times my dog woofed | 
| Fish Zooms | 3 | A measure of the number of times my fish zoomed | 

"
        );
    }
}

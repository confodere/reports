use crate::parser::Segments;
use crate::pre_process::block::{Expression, ExpressionVariable};
use crate::pre_process::tree::{Component, Node};
use anyhow::Result;
use chrono::{Local, NaiveDate};
use mdbook::book::{BookItem, Chapter};
use mdbook::preprocess::Preprocessor;
use toml::{map::Map, value::Value};

pub mod block;
pub mod tree;

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
                if let Err(e) = pre_process_blocks(ch, date.clone()) {
                    eprintln!("report_process block error: {:#?}", e);
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

fn pre_process_blocks(chapter: &mut Chapter, date: NaiveDate) -> Result<()> {
    let mut node = Node::from(Segments::try_from(chapter.content.clone())?.0);
    node.value += ExpressionVariable::Date(date);
    chapter.content = node.render(&Expression::new())?;

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

#[cfg(test)]
mod tests {

    use super::*;
    use crate::pre_process::block::Expression;
    use crate::time_span::TimeFrequency;
    use crate::Data;
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

        Ok(())
    }

    #[test]
    fn test_single_table() {
        setup().unwrap();

        let mut ch = Chapter::new(
            "test",
            "{{# cat_purrs }}{{*table rows=[Quarterly Weekly ] cols=[change] }}{{/#}}".to_string(),
            "test.md",
            vec![],
        );
        let date = NaiveDate::from_ymd(2022, 2, 4);

        let expected_table = String::from(
            "
| _ | change |
| --- | --- |
| Quarterly | 233.3% |
| Weekly | 25.0% |
",
        );

        assert_eq!(pre_process_blocks(&mut ch, date).unwrap(), ());
        assert_eq!(ch.content, expected_table);
    }

    #[test]
    fn test_pre_process_blocks() {
        let mut ch = Chapter::new(
            "test",
            "{{#cat_purrs}}
        Weekly change in cat purrs was {{change Weekly describedpercentage}}.
        Quarterly change in cat purrs was {{change Quarterly}} compared to {{prev Quarterly}}.{{/#}}
        "
            .to_string(),
            "test.md",
            vec![],
        );

        let date = NaiveDate::from_ymd(2022, 2, 4);

        assert_eq!(pre_process_blocks(&mut ch, date).unwrap(), ());
        assert_eq!(
            ch.content,
            "
        Weekly change in cat purrs was up 25.0%.
        Quarterly change in cat purrs was 233.3% compared to 2021 (25th October to 31st October).
        "
        );
    }

    #[test]
    fn test_block_vars() {
        let mut ch = Chapter::new(
            "test",
            "{{# fish_zooms describedpercentage }}
            {{avg_freq Daily describedrounded}}
            {{change Weekly}}
            {{name}}
            {{description}}
            {{span}}
            {{/#}}
            
            {{# cat_purrs describedpercentage }}
            {{name}} are {{change Quarterly}} since {{prev Quarterly}}
            {{/#}}"
                .to_string(),
            "test.md",
            vec![],
        );
        let date = NaiveDate::from_ymd(2022, 2, 4);

        assert_eq!(pre_process_blocks(&mut ch, date).unwrap(), ());
        assert_eq!(
            ch.content,
            "
            0.4 per day
            down 14.3%
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
{{# describedpercentage }}
- {{# website_visits  }}Total website users were {{change Weekly}}, we are now averaging {{avg_freq Daily describedrounded}}.
- {{name}} are {{change Yearly }} compared to this same reporting period last year {{prev Yearly}}.{{/#}}
            
- {{# cat_purrs  }}{{name}} are {{change Quarterly}} since {{prev Quarterly}}{{/#}}{{/#}}"
                .to_string(),
            "test.md",
            vec![],
        );
        let date = NaiveDate::from_ymd(2022, 2, 4);

        assert_eq!(pre_process_blocks(&mut ch, date).unwrap(), ());
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
        let text =
            "{{# change }}{{*table cols=[Daily] rows=[cat_purrs dog_woofs fish_zooms]}}{{/#}}"
                .to_string();

        let mut node = Node::from(Segments::try_from(text).unwrap().0);
        node.value.set_date(NaiveDate::from_ymd(2022, 2, 4));
        let text = node.render(&Expression::new()).unwrap();

        assert_eq!(
            text,
            "
| _ | Daily |
| --- | --- |
| cat_purrs | 25.0% |
| dog_woofs | -60.9% |
| fish_zooms | -14.3% |
"
            .to_string()
        );
    }

    #[test]
    fn test_description_table() {
        let mut ch = Chapter::new(
            "test",
            "{{*table rows=[cat_purrs dog_woofs fish_zooms] cols=[ fig description]}}".to_string(),
            "test.md",
            vec![],
        );
        let date = NaiveDate::from_ymd(2022, 2, 4);

        assert_eq!(pre_process_blocks(&mut ch, date).unwrap(), ());
        assert_eq!(
            ch.content,
            "
| _ | fig | description |
| --- | --- | --- |
| cat_purrs | 10.0 | A measure of the number of times my cat purred |
| dog_woofs | 25.0 | A measure of the number of times my dog woofed |
| fish_zooms | 3.0 | A measure of the number of times my fish zoomed |
"
        );
    }
}

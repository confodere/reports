use chrono::{Datelike, Month, NaiveDate};
use handlebars::RenderError;
use handlebars::{
    handlebars_helper, BlockContext, Context, Handlebars, Helper, HelperDef, HelperResult, Output,
    Renderable,
};
use num_traits::FromPrimitive;
use reports::*;
use serde::{Deserialize, Serialize};
use serde_json::{self, json};
use std::fs;

fn _add_year_month(year: i32, month: u32) -> (i32, u32) {
    let month = if let Some(month) = Month::from_u32(month) {
        month.succ().number_from_month()
    } else {
        panic!("Invalid month")
    };
    let year = match month {
        1 => year + 1,
        _ => year,
    };
    (year, month)
}

fn _add_month(date: NaiveDate) -> NaiveDate {
    let (year, month) = _add_year_month(date.year(), date.month());
    let mut day = date.day();
    let max_days = {
        let (year_next, month_next) = _add_year_month(year, month);
        NaiveDate::from_ymd(year_next, month_next, 1)
            .signed_duration_since(NaiveDate::from_ymd(year, month, 1))
            .num_days() as u32
    };
    day = if day > max_days { max_days } else { day };
    NaiveDate::from_ymd(year, month, day)
}

fn simple_helper(
    h: &Helper,
    hbs: &Handlebars,
    ctx: &Context,
    _rc: &mut handlebars::RenderContext,
    out: &mut dyn Output,
) -> HelperResult {
    //println!("{:#?}", ctx.data());

    let data_option: Statement = serde_json::from_value(h.param(0).unwrap().value().clone())?;

    let render_context = serde_json::from_value(h.param(1).unwrap().value().clone())?;

    let metric = Metric::read(
        data_option.name,
        &serde_json::from_value(ctx.data().get("date").unwrap().clone()).unwrap(),
        data_option.calculation_type,
        data_option.frequency,
    )
    .expect("Failures");

    match render_context {
        RenderContext::Words => {
            let result = hbs.render_template(
                &metric.long_text,
                &Sample {
                    fig: metric.render(DisplayType::DescribedPercentage),
                    prev: (&metric.data.span - metric.frequency).to_string(),
                },
            )?;

            out.write(&result)?;
        }
        RenderContext::Numbers => {
            out.write(&metric.render(DisplayType::Rounded))?;
        }
    }
    //out.write(&metric.render(render_context))?;

    Ok(())
}

fn create_table(
    h: &Helper,
    _hbs: &Handlebars,
    _ctx: &Context,
    _rc: &mut handlebars::RenderContext,
    out: &mut dyn Output,
) -> HelperResult {
    let [name, date] = [0, 1].map(|key| h.param(key).unwrap().value().clone());
    let (name, date) = (
        serde_json::from_value(name).unwrap(),
        serde_json::from_value(date).unwrap(),
    );
    let data = Data::read(&name, &date).expect("Failed to create data");
    let point = data.read_point().expect("Failed to read point");
    if let Some(description) = data.description {
        out.write(&format!(
            "| {} | {} | {}. |",
            data.long_name,
            point.fig(),
            description
        ))?;
    } else {
        out.write(&format!("| {} | {} |  |", data.long_name, point.fig()))?;
    }
    Ok(())
}

#[derive(Clone, Copy)]
struct TableHelper;

impl HelperDef for TableHelper {
    fn call<'reg: 'rc, 'rc>(
        &self,
        h: &Helper<'reg, 'rc>,
        r: &'reg Handlebars<'reg>,
        ctx: &'rc Context,
        rc: &mut handlebars::RenderContext<'reg, 'rc>,
        out: &mut dyn Output,
    ) -> HelperResult {
        let data = h
            .param(0)
            .ok_or_else(|| RenderError::new("Data param not found for helper \"table\""))?;
        let date = h
            .param(1)
            .ok_or_else(|| RenderError::new("Date param not found for helper \"table\""))?;
        let data = data.value().as_str().ok_or_else(|| {
            RenderError::new("Couldn't convert value to string for helper \"table\"")
        })?;
        let date = serde_json::from_value(date.value().clone())?;

        let data = Data::read(&data.to_string(), &date).expect("Cannot read data");
        let point = data.read_point().expect("Cannot read point");

        let mut values = vec![data.long_name, point.fig().to_string()];
        if let Some(description) = data.description {
            values.push(description)
        }

        let mut block = BlockContext::new();
        block.set_base_value(json!(values));

        rc.push_block(block);

        if let Some(template) = h.template() {
            template.render(r, ctx, rc, out)?;
        };

        rc.pop_block();

        Ok(())
    }
}

#[derive(Serialize, Deserialize)]
struct Sample {
    fig: String,
    prev: String,
}

#[derive(Serialize)]
struct FedData {
    date: NaiveDate,
    paragraphs: Vec<Paragraph>,
    words_render_context: RenderContext,
    nums_render_context: RenderContext,
    data_table: Vec<String>,
}

fn main() {
    let date = NaiveDate::from_ymd(2022, 2, 4);

    //let metrics = Data::read(name, &date).unwrap();
    let paragraphs = vec![Paragraph {
        contents: vec![
            vec![
                Statement {
                    name: String::from("website_visits"),
                    calculation_type: String::from("Change"),
                    frequency: TimeFrequency::Weekly,
                },
                Statement {
                    name: String::from("website_visits"),
                    calculation_type: String::from("AvgFreq"),
                    frequency: TimeFrequency::Daily,
                },
            ],
            vec![Statement {
                name: String::from("website_visits"),
                calculation_type: String::from("Change"),
                frequency: TimeFrequency::Yearly,
            }],
        ],
        name: String::from("Top Highlights"),
    }];

    let config_file = fs::File::create("ignore/struct.json").expect("Error reading config");
    serde_json::to_writer_pretty(&config_file, &paragraphs).expect("Error writing config");

    let mut hbs = Handlebars::new();

    handlebars_helper!(prev_span: | metric: Metric | (&metric.data.span - metric.frequency).to_string());
    handlebars_helper!(partial_name: | metric: Metric | metric.partial_name() );
    hbs.register_helper("prev_span", Box::new(prev_span));
    hbs.register_helper("partial_name", Box::new(partial_name));
    hbs.register_helper("num", Box::new(simple_helper));
    hbs.register_helper("tbl", Box::new(create_table));
    hbs.register_helper("table", Box::new(TableHelper));

    //- {{> (lookup item "partial_name") fig=(draw (lookup ../figs @key) ../context)}}

    let fd = FedData {
        date,
        paragraphs,
        words_render_context: RenderContext::Words,
        nums_render_context: RenderContext::Numbers,
        data_table: vec![
            "dealer_reveals".into(),
            "email_reveals".into(),
            "website_reveals".into(),
            "phone_reveals".into(),
            "free_calls".into(),
        ],
    };

    hbs.register_template_file("tpl", "templates/template.md")
        .unwrap();

    let file = fs::File::create("ignore/example-book/src/output.md").unwrap();
    hbs.render_to_write("tpl", &fd, &file).unwrap();
}

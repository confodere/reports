use chrono::{Datelike, Month, NaiveDate};
use handlebars::{handlebars_helper, Context, Handlebars, Helper, HelperResult, Output};
use num_traits::FromPrimitive;
use reports::*;
use serde::Serialize;
use serde_json;
use std::collections::HashMap;

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
    _: &Handlebars,
    _: &Context,
    _rc: &mut handlebars::RenderContext,
    out: &mut dyn Output,
) -> HelperResult {
    let metric: Metric = serde_json::from_value(h.param(0).unwrap().value().clone())?;

    let render_context = serde_json::from_value(h.param(1).unwrap().value().clone())?;

    out.write(&compute_figure(&metric, render_context).fig)?;

    Ok(())
}

fn compute_figure(metric: &Metric, render_context: RenderContext) -> ComputedStringMetric {
    let fig: Figures = match &metric.calculation_type[..] {
        "Change" => Figures::Change(Change::from(&metric).expect("Oh no!")),
        "AvgFreq" => {
            let datapoints = metric
                .data
                .read_points(metric.data.span.start(), metric.data.span.end())
                .expect("Couldn't find AvgFreq point");
            if let Some(point) = datapoints.get(&metric.data.span) {
                Figures::AvgFreq(AvgFreq::from(point.clone(), metric.clone()))
            } else {
                panic!("Oh no!");
            }
        }
        _ => panic!("Oh no!"),
    };

    fig.render(render_context, metric.partial_name())
}

#[derive(Serialize)]
struct FedData {
    computed: HashMap<String, ComputedStringMetric>,
    metrics: Vec<Metric>,
    render_context: RenderContext,
}

fn main() {
    let date = NaiveDate::from_ymd(2022, 2, 4);
    let name = String::from("website_visits");

    let metrics = Data::read(name, &date).unwrap();

    let mut hbs = Handlebars::new();

    let mut computed_figures = HashMap::new();

    for metric in metrics.clone() {
        hbs.register_partial(&metric.partial_name(), &metric.long_text)
            .expect("Failed to register partial");
        computed_figures.insert(
            metric.partial_name(),
            compute_figure(&metric, RenderContext::Words),
        );
    }

    handlebars_helper!(render: | obj: Change, context: RenderContext | obj.render(context, String::from("nah")).fig);
    hbs.register_helper("num", Box::new(simple_helper));

    //- {{> (lookup item "partial_name") fig=(draw (lookup ../figs @key) ../context)}}

    hbs.register_template_string(
        "template",
        r#"
    {{#> layout}}
    {{#*inline "thingo"}}
        {{#each computed as |item|}}
        - {{> (lookup item "partial_name")}}
        {{/each}}
    {{/inline}}
    {{#*inline "description"}}
        | {{frequency}} | {{calculation_type}} | {{num this ../render_context}} |
    {{/inline}}
    {{/layout}}
    
    {{#*inline "layout"}}
    # Top Highlights
    {{> thingo}}

    | Frequency | Calculation | Value |
    | --------- | ----------- | ----- |
    {{#each metrics}}
        {{> description}}
    {{/each}}
    {{/inline}}
    
    {{> layout}}

    "#,
    )
    .expect("Error registering template");

    let fd = FedData {
        computed: computed_figures,
        metrics,
        render_context: RenderContext::Words,
    };

    let rendered = hbs
        .render("template", &fd)
        .expect("Error rendering template");

    println!("{}", rendered);
}

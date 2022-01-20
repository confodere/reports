use chrono::{Datelike, Month, NaiveDate};
use handlebars::Handlebars;
use num_traits::FromPrimitive;
use reports::*;
use std::vec;

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

fn main() {
    let date = NaiveDate::from_ymd(2022, 2, 4);

    let name = String::from("website_visits");
    let data = Data::read(name).unwrap();
    let span = TimeSpan::new(&date, data.frequency.clone());

    let hbs = Handlebars::new();
    for metric in &data.metrics {
        match &metric.calculation_type[..] {
            "Change" => {
                let change = Change::from(&data, span.clone(), metric.frequency.clone())
                    .expect("Oh no!")
                    .render(RenderContext::Words);
                let text = hbs.render_template(&metric.long_text, &change).unwrap();
                println!("{}", text);
            }
            "AvgFreq" => {
                let datapoints = data
                    .read_points(span.start(), span.end())
                    .expect("Couldn't read");
                if let Some(point) = datapoints.get(&span) {
                    let num = ComputedStringMetric {
                        fig: DisplayType::PerFrequency(point, &data.frequency, &metric.frequency)
                            .to_string(),
                        time_periods: vec![],
                    };
                    let text = hbs.render_template(&metric.long_text, &num).unwrap();
                    println!("{}", text);
                }
            }
            _ => panic!("Oh no"),
        }
    }

    // Assumes sample data (not distributed) is already in database:
    // Metrics {users, users_change, website_visits}
    // Datapoints {matching users x2}

    /*
    let metrics = Metric::read().expect("Error reading metrics from database");

    let [users_m, users_change_m, website_visits] = ["users", "users_change", "website_visits"]
        .map(|name| -> Metric {
            if let Some(metric) = metrics.get(name) {
                metric.clone()
            } else {
                panic!("Couldn't find {} in the database", name)
            }
        });

    let users_points = Datapoint::read(&users_m).expect("Couldn't read users datapoint");

    let search_period = TimePeriod::new(&NaiveDate::from_ymd(2022, 2, 4), &TimeFrequency::Weekly);
    let website_users_change = FigChange::from_period(users_change_m, &search_period).unwrap();

    println!(
        "{} - we are now averaging {}.",
        website_users_change,
        DisplayType::PerFrequency(
            users_points.get(&search_period).unwrap(),
            &TimeFrequency::Daily
        )
    );

    let website_visits_change =
        FigChange::new(website_visits, Utc::today().naive_utc(), 100.0, 164.58);

    println!("{}", website_visits_change);
    */

    /*println!(
        "{} ({})",
        website_visits_change,
        website_visits_change.when().format("%Y")
    );

    let paragraph = Paragraph {
        name: String::from("Top highlights"),
        contents: vec![
            Statement {
                contents: vec![website_users_change.clone()],
            },
            Statement {
                contents: vec![website_visits_change],
            },
        ],
        fig: website_users_change,
    };
    */

    /*

    handlebars_helper!(pretty_print: | obj: FigChange | obj.to_string());

    handlebars_helper!(dp: |obj : FigChange | DisplayType::DescribedPercentage(&obj).to_string());
    handlebars_helper!(when: |obj: FigChange | obj.period().to_string());
    handlebars_helper!(when_prev: |obj: FigChange | obj.period().prev().to_string());

    let mut hbs = Handlebars::new();
    hbs.register_helper("pp", Box::new(pretty_print));
    hbs.register_helper("dp", Box::new(dp));
    hbs.register_helper("when", Box::new(when));
    hbs.register_helper("when_prev", Box::new(another_simple_helper));

    hbs.register_template_file("tpl", "templates/template.md")
        .unwrap();

    let file = fs::File::create("ignore/output.md").unwrap();
    //hbs.render_to_write("tpl", &paragraph, &file).unwrap();

    hbs.register_template_string("web_visits", website_visits_change.metric_info().template())
        .unwrap();
    hbs.render_to_write("web_visits", &website_visits_change, &file)
        .unwrap();

    */
}

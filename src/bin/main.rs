use chrono::{Datelike, Month, NaiveDate, Utc};
use handlebars::{handlebars_helper, Handlebars};
use num_traits::FromPrimitive;
use reports::*;
use std::{fs, vec};

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
    // Assumes sample data (not distributed) is already in database:
    // Metrics {users, users_change, website_visits}
    // Datapoints {matching users x2}

    let metrics = Metric::read().expect("Error reading metrics from database");

    let [users_m, users_change_m, website_visits_m] = ["users", "users_change", "website_visits"]
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
        FigChange::new(website_visits_m, Utc::today().naive_utc(), 100.0, 164.58);

    println!(
        "{} ({})",
        website_visits_change,
        website_visits_change.when().format("%Y")
    );

    let paragraph = Paragraph {
        name: String::from("Top highlights"),
        contents: vec![
            Statement {
                contents: vec![website_users_change],
            },
            Statement {
                contents: vec![website_visits_change],
            },
        ],
    };

    handlebars_helper!(pretty_print: | obj: FigChange | obj.to_string());
    let mut hbs = Handlebars::new();
    hbs.register_helper("pp", Box::new(pretty_print));

    hbs.register_template_file("tpl", "templates/template.md")
        .unwrap();

    let file = fs::File::create("ignore/output.md").unwrap();
    hbs.render_to_write("tpl", &paragraph, &file).unwrap();
}

use crate::pre_process::arg::Arg;
use crate::pre_process::{
    block::{Command, Expression},
    tree::Component,
};
use anyhow::Result;

/// Table is two dimensional structure that adds one [ExpressionVariable] from each dimension
/// into an [Expression] to complete it.  
///
/// A row or column adds the *same* [ExpressionVariable] to the entire row/column,
/// so that a cross-section of two variables is generated.
#[derive(Clone, Debug)]
pub struct Table {
    rows: Vec<Arg>,
    cols: Vec<Arg>,
    ctx: Expression,
}

impl Table {
    pub fn new(rows: Vec<Arg>, cols: Vec<Arg>, ctx: Expression) -> Self {
        Self { rows, cols, ctx }
    }
}

impl Component for Table {
    fn render(&self, ctx: &Expression) -> Result<String> {
        let mut expr = self.ctx.clone();
        expr.fill_blank(ctx.to_owned());

        let mut table = String::from("\n| _ |");
        // Column Headers
        for col in &self.cols {
            table.push_str(&format!(" {} |", &col.to_string()));
        }
        table.push_str("\n|");
        // Header Separator
        for _ in 0..&self.cols.len() + 1 {
            table.push_str(" --- |");
        }
        // Rows
        for row in &self.rows {
            let mut ctx = expr.clone();
            ctx.fill_arg(row.to_owned());
            // Row Heading
            table.push_str(&format!("\n| {} |", row.to_string()));
            // Row Cells
            for col in &self.cols {
                let mut ctx = ctx.clone();
                ctx.fill_arg(col.to_owned());
                table.push_str(&format!(
                    " {} |",
                    &String::try_from(Command::try_from(&ctx)?)?
                ));
            }
        }
        table.push_str("\n");
        Ok(table)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::RenderContext;
    use crate::TimeFrequency;
    use chrono::NaiveDate;

    #[test]
    fn test_table() {
        let date = NaiveDate::from_ymd(2022, 2, 4);
        let mut ctx = Expression::new();
        ctx.set_data_name("cat_purrs".to_string());
        ctx.set_date(date);
        ctx.set_display_type(RenderContext::Numbers);
        let cols = vec![
            Arg::TimeFrequency(TimeFrequency::Weekly),
            Arg::TimeFrequency(TimeFrequency::Quarterly),
        ];
        let rows = vec![Arg::Command("change"), Arg::Command("avg_freq")];
        let tbl = Table::new(rows, cols, ctx);
        let tbl = tbl.render(&Expression::new()).unwrap();

        assert_eq!(
            tbl,
            "
| _ | Weekly | Quarterly |
| --- | --- | --- |
| Change | 25.0% | 233.3% |
| Figure per Frequency | 10.0 | 130.0 |
"
            .to_string()
        );
    }
}

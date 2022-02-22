use crate::pre_process::{
    block::{Command, Expression, ExpressionVariable},
    tree::Component,
};
use anyhow::{Error, Result};

/// Table is two dimensional structure that adds one [ExpressionVariable] from each dimension
/// into an [Expression] to complete it.  
///
/// A row or column adds the *same* [ExpressionVariable] to the entire row/column,
/// so that a cross-section of two variables is generated.
#[derive(Clone)]
pub struct Table {
    rows: Vec<ExpressionVariable>,
    cols: Vec<ExpressionVariable>,
    ctx: Expression,
}

impl Table {
    pub fn new(
        rows: Vec<ExpressionVariable>,
        cols: Vec<ExpressionVariable>,
        ctx: Expression,
    ) -> Self {
        Self { rows, cols, ctx }
    }
}

impl TryFrom<Table> for String {
    type Error = Error;

    fn try_from(value: Table) -> Result<Self, Self::Error> {
        let mut table = String::from("\n| _ |");
        // Column Headers
        for col in &value.cols {
            table.push_str(&format!(" {} |", &col.to_string()));
        }
        table.push_str("\n|");
        // Header Separator
        for _ in 0..&value.cols.len() + 1 {
            table.push_str(" --- |");
        }
        // Rows
        for row in &value.rows {
            let ctx = value.ctx.clone() + row.clone();
            // Row Heading
            table.push_str(&format!("\n| {} |", row.to_string()));
            // Row Cells
            for col in &value.cols {
                let ctx = ctx.clone() + col.clone();
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

impl Component for Table {
    fn render(&mut self, ctx: &Expression) -> Result<String> {
        self.ctx.fill_blank(ctx.clone());
        String::try_from(self.clone())
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
            ExpressionVariable::TimeFrequency(TimeFrequency::Weekly),
            ExpressionVariable::TimeFrequency(TimeFrequency::Quarterly),
        ];
        let rows = vec![
            ExpressionVariable::Command("change".to_string()),
            ExpressionVariable::Command("avg_freq".to_string()),
        ];
        let tbl = Table::new(rows, cols, ctx);
        let tbl = String::try_from(tbl).unwrap();

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

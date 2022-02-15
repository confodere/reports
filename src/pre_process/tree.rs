use anyhow::Result;

use super::block::Expression;

pub trait Component {
    fn render(&mut self, ctx: &Expression) -> Result<String>;
}

pub struct Filler(pub String);

impl Component for Filler {
    fn render(&mut self, _: &Expression) -> Result<String> {
        Ok(self.0.clone())
    }
}

pub struct Node {
    pub value: Expression,
    children: Vec<Box<dyn Component>>,
}

impl Node {
    pub fn new(value: Expression) -> Self {
        Node {
            value,
            children: vec![],
        }
    }
    /// Borrows internal variables mutably to update
    pub fn add_child(&mut self, child: Box<dyn Component>) {
        self.children.push(child);
    }
}

impl Component for Node {
    fn render(&mut self, ctx: &Expression) -> Result<String> {
        if let None = self.value.command {
            if let Some(v) = &ctx.command {
                self.value.set_command(v.clone())
            }
        }
        if let None = self.value.frequency {
            if let Some(v) = ctx.frequency {
                self.value.set_frequency(v)
            }
        }
        if let None = self.value.data {
            if let Some(v) = &ctx.data {
                self.value.set_data(&v)
            }
        }
        if let None = self.value.display_type {
            if let Some(v) = ctx.display_type {
                self.value.set_display_type(v)
            }
        }
        let mut rendered = String::new();
        for child in &mut self.children {
            rendered.push_str(child.render(&self.value)?.as_str());
        }

        Ok(rendered)
    }
}

#[cfg(test)]
mod tests {
    use chrono::NaiveDate;

    use crate::pre_process::block::ExpressionVariable;
    use crate::time_span::TimeFrequency;

    use super::*;

    #[test]
    fn create_node_public() {
        let date = NaiveDate::from_ymd(2022, 2, 4);
        let mut vars = Vec::new();
        for var in ["Weekly", "cat_purrs", "change"] {
            vars.push(ExpressionVariable::try_new(var, &date).unwrap());
        }
        let vars_2 = vec![ExpressionVariable::Command("avg_freq".to_string())];
        let vars_3 = vec![ExpressionVariable::TimeFrequency(TimeFrequency::Quarterly)];

        let leaf_1 = Box::new(Expression::from(vars_3));
        let leaf_2 = Box::new(Expression::from(vars_2));

        let mut branch = Node::new(Expression::from(vars));

        branch.add_child(leaf_1);
        branch.add_child(leaf_2);

        assert_eq!(
            branch.render(&Expression::new()).unwrap(),
            String::from("233.3%10.0")
        );
    }
}

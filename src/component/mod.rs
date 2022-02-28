pub mod arg;
pub mod expression;
pub mod flatten;

use anyhow::Result;
use expression::Expression;
use std::{cell::RefCell, rc::Rc};

pub trait Component: core::fmt::Debug {
    fn render(&self, ctx: &Expression) -> Result<String>;
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Text(pub String);

impl Component for Text {
    fn render(&self, _: &Expression) -> Result<String> {
        Ok(self.0.clone())
    }
}

impl<T: Component> Component for Vec<T> {
    fn render(&self, ctx: &Expression) -> Result<String> {
        let mut s = String::new();
        for c in self {
            s.push_str(c.render(ctx)?.as_str())
        }
        Ok(s)
    }
}

#[derive(Debug)]
pub struct Node {
    pub value: Expression,
    children: Rc<RefCell<Vec<Box<dyn Component>>>>,
}

impl Node {
    pub fn new(value: Expression) -> Self {
        Node {
            value,
            children: Rc::new(RefCell::new(vec![])),
        }
    }
    pub fn new_all(value: Expression, children: Rc<RefCell<Vec<Box<dyn Component>>>>) -> Self {
        Node { value, children }
    }

    /// Borrows internal variables mutably to update
    pub fn add_child(&mut self, child: Box<dyn Component>) {
        self.children.borrow_mut().push(child);
    }

    pub fn with_expr(&self, expr: Expression) -> Self {
        Node {
            value: expr,
            children: Rc::clone(&self.children),
        }
    }
}

impl Component for Node {
    fn render(&self, ctx: &Expression) -> Result<String> {
        let mut expr = self.value.clone();
        expr.fill_blank(ctx.to_owned());

        let rendered = self
            .children
            .borrow()
            .iter()
            .map(|component| component.render(&expr))
            .collect::<Result<Vec<String>>>()?
            .join("");

        Ok(rendered)
    }
}

#[cfg(test)]
mod tests {
    use super::arg::Arg;
    use super::*;
    use crate::time_span::TimeFrequency;
    use chrono::NaiveDate;

    #[test]
    fn create_node_public() {
        let date = NaiveDate::from_ymd(2022, 2, 4);
        let mut vars = Vec::new();
        for var in ["Weekly", "cat_purrs", "change", "Numbers"] {
            vars.push(var.parse::<Arg>().unwrap());
        }
        let vars_2 = vec![Arg::Command("avg_freq")];
        let vars_3 = vec![Arg::TimeFrequency(TimeFrequency::Quarterly)];

        let mut leaf_1 = Box::new(Expression::from(vars_3));
        let mut leaf_2 = Box::new(Expression::from(vars_2));
        leaf_1.set_date(date.clone());
        leaf_2.set_date(date);

        let mut branch = Node::new(Expression::from(vars));

        branch.add_child(leaf_1);
        branch.add_child(leaf_2);

        assert_eq!(
            branch.render(&Expression::new()).unwrap(),
            String::from("233.3%10.0")
        );
    }
}

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

use anyhow::Result;

use super::block::{Command, Expression, ExpressionNamedCollection, ExpressionVariable};
use crate::functions::table::Table;

#[derive(Debug, PartialEq, Eq)]
pub enum Component<'a> {
    Filler(&'a str),
    Vars(Vec<ExpressionVariable<'a>>),
    Collection(&'a str, HashMap<&'a str, Vec<ExpressionVariable<'a>>>),
}

#[derive(Debug)]
pub struct Node<'a> {
    pub value: Component<'a>,
    parent: RefCell<Weak<Node<'a>>>,
    children: RefCell<Vec<Rc<Node<'a>>>>,
}

impl<'a> Node<'a> {
    pub fn new(value: Component<'a>) -> Self {
        Node {
            value,
            parent: RefCell::new(Weak::new()),
            children: RefCell::new(vec![]),
        }
    }
    /// Borrows internal variables mutably to update
    pub fn add_child(parent: &Rc<Node<'a>>, child: &Rc<Node<'a>>) {
        parent.children.borrow_mut().push(Rc::clone(child));
        *child.parent.borrow_mut() = Rc::downgrade(parent);
    }

    fn get_parent_expr(&self) -> Option<Expression<'a>> {
        if let Some(parent) = &self.parent.borrow().upgrade() {
            if let Some(mut expr) = parent.get_parent_expr() {
                if let Component::Vars(vars) = &parent.value {
                    for var in vars {
                        expr += var.clone();
                    }
                }
                Some(expr)
            } else {
                if let Component::Vars(vars) = &parent.value {
                    Some(Expression::from(vars.clone()))
                } else {
                    None
                }
            }
        } else {
            None
        }
    }

    pub fn render(&self) -> Result<String> {
        let mut rendered = String::new();
        if self.children.borrow().len() > 0 {
            for child in self.children.borrow().iter() {
                rendered.push_str(&child.render()?);
            }
        } else {
            match &self.value {
                Component::Filler(fill) => rendered.push_str(fill),
                Component::Vars(vars) => {
                    let expr = if let Some(mut expr) = self.get_parent_expr() {
                        for var in vars {
                            expr += var.clone()
                        }
                        expr
                    } else {
                        Expression::new()
                    };
                    rendered.push_str(&String::try_from(Command::try_from(&expr)?)?);
                }
                /* Component::Expression(expr) => {
                    let mut expr = expr.clone();

                    lookup!(expr, self, ExpressionVariable::Command, command);
                    lookup!(expr, self, ExpressionVariable::TimeFrequency, frequency);
                    lookup!(expr, self, ExpressionVariable::DisplayType, display_type);
                    lookup!(expr, self, ExpressionVariable::Data, data);

                    rendered.push_str(&String::try_from(Command::try_from(&expr)?)?);
                } */
                Component::Collection(command, collections) => {
                    let ctx = if let Some(expr) = self.get_parent_expr() {
                        expr
                    } else {
                        Expression::new()
                    };
                    rendered.push_str(&String::try_from(Table::try_from(
                        ExpressionNamedCollection::new(command, collections, &ctx),
                    )?)?);
                }
            }
        }

        Ok(rendered)
    }
}

#[cfg(test)]
mod tests {
    use chrono::NaiveDate;

    use crate::time_span::TimeFrequency;

    use super::*;

    #[test]
    fn create_node_public() {
        let date = NaiveDate::from_ymd(2022, 2, 4);
        let mut vars = Vec::new();
        for var in ["Weekly", "cat_purrs", "change"] {
            vars.push(ExpressionVariable::try_new(var, &date).unwrap());
        }
        let vars_2 = vec![ExpressionVariable::Command("avg_freq")];
        let vars_3 = vec![ExpressionVariable::TimeFrequency(TimeFrequency::Quarterly)];

        let leaf_1 = Rc::new(Node::new(Component::Vars(vars_3)));
        let leaf_2 = Rc::new(Node::new(Component::Vars(vars_2)));

        let branch = Rc::new(Node::new(Component::Vars(vars)));

        assert!(leaf_1.parent.borrow().upgrade().is_none());

        Node::add_child(&branch, &leaf_1);
        Node::add_child(&branch, &leaf_2);

        assert_eq!(leaf_1.render().unwrap(), String::from("233.3%"));
        assert_eq!(leaf_2.render().unwrap(), String::from("10.0"));
        assert_eq!(branch.render().unwrap(), String::from("233.3%10.0"));

        assert!(leaf_1.parent.borrow().upgrade().is_some());
        assert_eq!(branch.children.borrow().len(), 2);
    }

    #[test]
    fn create_node() {
        let leaf_1 = Rc::new(Node {
            value: Component::Filler("Alpha Beta Gamma"),
            parent: RefCell::new(Weak::new()),
            children: RefCell::new(vec![]),
        });

        let leaf_2 = Rc::new(Node {
            value: Component::Collection(
                "table",
                HashMap::from([(
                    "row",
                    vec![
                        ExpressionVariable::Command("description"),
                        ExpressionVariable::Command("name"),
                    ],
                )]),
            ),
            parent: RefCell::new(Weak::new()),
            children: RefCell::new(vec![]),
        });

        println!("leaf parent = {:?}", leaf_1.parent.borrow().upgrade());

        let branch = Rc::new(Node {
            value: Component::Vars(vec![ExpressionVariable::Command("change")]),
            children: RefCell::new(vec![Rc::clone(&leaf_1), Rc::clone(&leaf_2)]),
            parent: RefCell::new(Weak::new()),
        });

        *leaf_1.parent.borrow_mut() = Rc::downgrade(&branch);

        println!("leaf parent = {:?}", leaf_1.parent.borrow().upgrade());

        assert_eq!(
            leaf_1.parent.borrow().upgrade().unwrap().value,
            Component::Vars(vec![ExpressionVariable::Command("change")])
        );
        assert_eq!(
            branch.children.borrow()[0].value,
            Component::Filler("Alpha Beta Gamma")
        );
    }
}

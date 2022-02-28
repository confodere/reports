use super::{arg::ArgGroup, expression::Expression};
use crate::functions::table::Table;
use anyhow::{anyhow, Result};

pub trait ItemOrCollection: Sized {
    type Item: Clone;

    fn items(&self) -> &[Self::Item];
    fn collection(collection: Vec<Self>) -> Vec<Vec<Self::Item>> {
        collection.into_iter().map(|s| s.items().to_vec()).collect()
    }
}

/// Mutates a list so that each subsequent permutation of `I` creates a new list with the added `I`
pub fn mutate<T>(groups: Vec<Vec<T>>) -> Vec<Vec<T>>
where
    T: Clone + Copy,
{
    groups.into_iter().fold(vec![vec![]], |acc, group| {
        group
            .into_iter()
            .map(|variant| {
                acc.clone()
                    .into_iter()
                    .map(|items| {
                        let mut new_items = items.clone();
                        new_items.push(variant);
                        new_items
                    })
                    .collect::<Vec<Vec<_>>>()
            })
            .flatten()
            .collect()
    })
}

pub fn find_table(groups: Vec<ArgGroup>) -> Result<Table> {
    let (mut rows, mut cols) = (None, None);
    let mut expr = Expression::new();
    for group in groups {
        match group {
            ArgGroup::Arg(arg) => expr.fill_arg(arg),
            ArgGroup::Collection(args) => {
                for arg in args {
                    expr.fill_arg(arg)
                }
            }
            ArgGroup::NamedCollection(name, args) => match name {
                "rows" => rows = Some(args),
                "cols" => cols = Some(args),
                _ => return Err(anyhow!("Table had unrecognised named collection {name}")),
            },
        }
    }
    let rows = rows.ok_or_else(|| anyhow!("Rows missing in Table"))?;
    let cols = cols.ok_or_else(|| anyhow!("Cols missing in Table"))?;

    Ok(Table::new(rows, cols, expr))
}

#[cfg(test)]
mod tests {
    use super::*;

    enum SimpleEnum<'a> {
        Item(&'a str),
        Collection(Vec<&'a str>),
    }

    impl<'a> ItemOrCollection for SimpleEnum<'a> {
        type Item = &'a str;

        fn items(&self) -> &[<Self as ItemOrCollection>::Item] {
            match self {
                SimpleEnum::Collection(s) => s,
                SimpleEnum::Item(s) => std::slice::from_ref(s),
            }
        }
    }

    #[test]
    fn test_mutate() {
        let one = SimpleEnum::Item("one");
        let two_three = SimpleEnum::Collection(vec!["two", "three"]);
        let four_five = SimpleEnum::Collection(vec!["four", "five"]);
        let group = vec![one, two_three, four_five];

        assert_eq!(
            mutate(ItemOrCollection::collection(group)),
            vec![
                vec!["one", "two", "four"],
                vec!["one", "three", "four"],
                vec!["one", "two", "five"],
                vec!["one", "three", "five"]
            ]
        );
    }
}

pub trait Substitute {
    fn start(&self) -> usize;
    fn end(&self) -> usize;
    fn text(&self) -> &str;
    fn render(&self, template: &mut String, skip: i32) -> i32 {
        let init_len = template.len() as i32;
        template.replace_range(
            (self.start() as i32 + skip) as usize..(self.end() as i32 + skip) as usize,
            self.text(),
        );
        skip + template.len() as i32 - init_len
    }
}

pub trait Render {
    fn render(&self, template: &mut String, skip: i32);
}

pub struct BasicSubstitute<'a> {
    start: usize,
    end: usize,
    text: &'a str,
}

impl<'a> BasicSubstitute<'a> {
    pub fn new(start: usize, end: usize, text: &'a str) -> Self {
        Self { start, end, text }
    }
}

impl<S: Substitute> Render for Vec<S> {
    fn render(&self, template: &mut String, skip: i32) {
        self.into_iter().fold(
            (skip, template),
            |(skip, text): (i32, &mut String), sub: &S| (sub.render(text, skip), text),
        );
    }
}

impl Substitute for BasicSubstitute<'_> {
    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }

    fn text(&self) -> &str {
        self.text
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_replace() {
        let sub_1 = BasicSubstitute::new(0, 9, "Hi Jeff!");
        let sub_2 = BasicSubstitute::new(10, 21, "Hi Mate!");
        let sub_3 = BasicSubstitute::new(22, 32, "Hi Dave!");

        let subs = vec![sub_1, sub_2, sub_3];

        let mut template = "Bye Mike, Bye George, Bye Alfred".to_string();

        subs.render(&mut template, 0);
        //Substitutes::<BasicSubstitute<'_>>::render(0, vec![sub_1, sub_2, sub_3], &mut template);

        assert_eq!(template, "Hi Jeff! Hi Mate! Hi Dave!".to_string());
    }
}

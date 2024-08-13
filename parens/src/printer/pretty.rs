use std::convert::Infallible;

use crate::escape::{escape_string, escape_symbol};

use super::{Print, Printer};
use pretty::DocAllocator as _;

struct PrettyPrinter<'a> {
    arena: &'a pretty::Arena<'a>,
    items: Vec<pretty::DocBuilder<'a, pretty::Arena<'a>>>,
}

impl<'a> Printer for PrettyPrinter<'a> {
    type Error = Infallible;

    fn symbol(&mut self, symbol: &str) -> Result<(), Self::Error> {
        let escaped = escape_symbol(&symbol.to_string());
        let doc = self.arena.text(escaped);
        self.items.push(doc);
        Ok(())
    }

    fn string(&mut self, string: &str) -> Result<(), Self::Error> {
        let escaped = escape_string(string);
        let doc = self.arena.text(escaped);
        self.items.push(doc);
        Ok(())
    }

    fn int(&mut self, int: i64) -> Result<(), Self::Error> {
        let doc = self.arena.text(int.to_string());
        self.items.push(doc);
        Ok(())
    }

    fn list<F>(&mut self, f: F) -> Result<(), Self::Error>
    where
        F: FnOnce(&mut Self) -> Result<(), Self::Error>,
    {
        let position = self.items.len();
        f(self)?;
        let items = self.items.drain(position..);

        let docs = self
            .arena
            .intersperse(items, self.arena.line())
            .nest(2)
            .group();

        self.items.push(
            self.arena
                .text("(")
                .append(docs)
                .append(self.arena.text(")")),
        );

        Ok(())
    }

    fn seq<F>(&mut self, f: F) -> Result<(), Self::Error>
    where
        F: FnOnce(&mut Self) -> Result<(), Self::Error>,
    {
        let position = self.items.len();
        f(self)?;
        let items = self.items.drain(position..);

        let docs = self
            .arena
            .intersperse(items, self.arena.line())
            .nest(1)
            .group();

        self.items.push(
            self.arena
                .text("[")
                .append(docs)
                .append(self.arena.text("]")),
        );

        Ok(())
    }

    fn map<F>(&mut self, f: F) -> Result<(), Self::Error>
    where
        F: FnOnce(&mut Self) -> Result<(), Self::Error>,
    {
        let position = self.items.len();
        f(self)?;
        let items = self.items.drain(position..);

        let docs = self
            .arena
            .intersperse(items, self.arena.line())
            .nest(1)
            .group();

        self.items.push(
            self.arena
                .text("{")
                .append(docs)
                .append(self.arena.text("}")),
        );

        Ok(())
    }

    fn group<F>(&mut self, f: F) -> Result<(), Self::Error>
    where
        F: FnOnce(&mut Self) -> Result<(), Self::Error>,
    {
        let position = self.items.len();
        f(self)?;
        let items = self.items.drain(position..);

        let docs = self.arena.intersperse(items, self.arena.line()).group();
        self.items.push(docs);
        Ok(())
    }
}

/// Pretty print a `T` into an s-expression string.
pub fn to_string_pretty<T>(value: T, width: usize) -> String
where
    T: Print,
{
    let arena = pretty::Arena::new();
    let mut printer = PrettyPrinter {
        items: vec![],
        arena: &arena,
    };

    let _ = value.print(&mut printer);

    let double_line = arena.line().append(arena.line());
    let doc = arena.intersperse(printer.items, double_line);

    let mut string = String::new();
    let _ = doc.render_fmt(width, &mut string);
    string
}

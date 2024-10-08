use std::convert::Infallible;

use crate::escape::{escape_string, escape_symbol};

use super::{Print, Printer};
use pretty::DocAllocator as _;

/// A pretty printer that uses the `pretty` crate to format the output.
struct PrettyPrinter<'a, C> {
    arena: &'a pretty::Arena<'a>,
    items: Vec<pretty::DocBuilder<'a, pretty::Arena<'a>>>,
    context: C,
}

impl<'a, C> Printer<C> for PrettyPrinter<'a, C> {
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

    #[inline]
    fn context(&self) -> &C {
        &self.context
    }

    #[inline]
    fn context_mut(&mut self) -> &mut C {
        &mut self.context
    }
}

/// Pretty print a `T` into an s-expression string.
///
/// This function is useful to produce human-readable output. If performance is a conern
/// and the output is intended to be consumed by another program, consider using the
/// [`to_string`] function instead.
///
/// The `width` parameter specifies the ideal maximum width of the output, which
/// is used to make decisions on when to break lines. The actual output may exceed
/// this width if it not possible to break lines to stay within the limit.
///
/// The `context` parameter is passed to the `Print` implementation and can be used to pass
/// additional information that may be required to print values of type `T`. When no context
/// is needed, the `()` value can be passed as the context.
///
/// [`to_string`]: `crate::printer::to_string`
pub fn to_string_pretty<T: Print<C>, C>(value: T, width: usize, context: C) -> String {
    let arena = pretty::Arena::new();
    let mut printer = PrettyPrinter {
        items: vec![],
        arena: &arena,
        context,
    };

    let _ = value.print(&mut printer);

    let double_line = arena.line().append(arena.line());
    let doc = arena.intersperse(printer.items, double_line);

    let mut string = String::new();
    let _ = doc.render_fmt(width, &mut string);
    string
}

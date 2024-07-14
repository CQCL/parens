use std::convert::Infallible;

use crate::escape::escape_string;

use super::{Print, Printer};
use pretty::BoxDoc;

struct PrettyPrinter {
    items: Vec<BoxDoc<'static>>,
}

impl Printer for PrettyPrinter {
    type Error = Infallible;

    fn atom(&mut self, atom: &str) -> Result<(), Self::Error> {
        self.items
            .push(BoxDoc::text(escape_string(&atom.to_string())));
        Ok(())
    }

    fn list<F>(&mut self, f: F) -> Result<(), Self::Error>
    where
        F: FnOnce(&mut Self) -> Result<(), Self::Error>,
    {
        let position = self.items.len();
        f(self)?;
        let items = self.items.drain(position..);
        let items = BoxDoc::intersperse(items, BoxDoc::line()).nest(2).group();
        self.items
            .push(BoxDoc::text("(").append(items).append(BoxDoc::text(")")));
        Ok(())
    }
}

pub fn to_string_pretty<T>(value: T, width: usize) -> String
where
    T: Print,
{
    let mut printer = PrettyPrinter { items: vec![] };
    let _ = value.print(&mut printer);
    let doc = BoxDoc::intersperse(printer.items, BoxDoc::line());
    let mut string = String::new();
    let _ = doc.render_fmt(width, &mut string);
    string
}

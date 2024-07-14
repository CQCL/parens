use crate::parser::{Parse, Parser, Peek};
use crate::printer::{Print, Printer};

/// Utility to parse lists with a given head.
pub struct Form<K, V = ()> {
    pub keyword: K,
    pub value: V,
}

impl<'a, K: Peek, V> Peek for Form<K, V> {
    fn peek(cursor: crate::parser::Cursor<'_>) -> bool {
        cursor.peek_list(|list| list.peek::<K>())
    }
}

impl<K: Parse, V: Parse> Parse for Form<K, V> {
    fn parse(parser: &mut Parser<'_>) -> crate::parser::Result<Self> {
        parser.list(|parser| {
            Ok(Self {
                keyword: parser.parse()?,
                value: parser.parse()?,
            })
        })
    }
}

impl<K: Print, V: Print> Print for Form<K, V> {
    fn print<P: Printer>(&self, printer: &mut P) -> Result<(), P::Error> {
        printer.list(|printer| {
            printer.print(&self.keyword)?;
            printer.print(&self.value)?;
            Ok(())
        })
    }
}

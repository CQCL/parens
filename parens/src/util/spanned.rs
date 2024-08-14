use crate::{
    parser::{self, Parse, Parser, Span},
    printer::{Print, Printer},
};

/// Records the source span around an inner type.
#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub inner: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    /// Converts into the inner type.
    pub fn into_inner(self) -> T {
        self.inner
    }
}

impl<T: Parse<C>, C> Parse<C> for Spanned<T> {
    fn parse(parser: &mut Parser<'_, C>) -> parser::Result<Self> {
        let start = parser.span().start;
        let inner = parser.parse()?;
        // TOOD: Prev span instead?
        let end = parser.span().end;
        Ok(Spanned {
            inner,
            span: start..end,
        })
    }
}

impl<T: Print<C>, C> Print<C> for Spanned<T> {
    fn print<P: Printer<C>>(&self, printer: &mut P) -> Result<(), P::Error> {
        self.inner.print(printer)
    }
}

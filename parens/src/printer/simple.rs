use crate::escape::{escape_string, escape_symbol};
use std::convert::Infallible;
use std::fmt::Write as _;

use super::{Print, Printer};

/// A simple pretty printer that formats the output as a string.
struct SimplePrinter<C> {
    needs_whitespace: bool,
    string: String,
    context: C,
}

impl<C> SimplePrinter<C> {
    pub fn new(context: C) -> Self {
        Self {
            needs_whitespace: false,
            string: String::new(),
            context,
        }
    }

    #[inline]
    fn print_delimited<F>(&mut self, open: char, close: char, f: F) -> Result<(), Infallible>
    where
        F: FnOnce(&mut Self) -> Result<(), Infallible>,
    {
        if self.needs_whitespace {
            self.string.push(' ');
        }

        self.string.push(open);
        self.needs_whitespace = false;
        f(self)?;
        self.string.push(close);
        self.needs_whitespace = true;

        Ok(())
    }
}

impl<C> Printer<C> for SimplePrinter<C> {
    type Error = Infallible;

    fn symbol(&mut self, atom: &str) -> Result<(), Self::Error> {
        if self.needs_whitespace {
            self.string.push(' ');
        }

        self.needs_whitespace = true;
        // TODO: Can we do without additional allocations here?
        self.string.push_str(&escape_symbol(atom));
        Ok(())
    }

    fn string(&mut self, string: &str) -> Result<(), Self::Error> {
        if self.needs_whitespace {
            self.string.push(' ');
        }

        self.needs_whitespace = true;
        // TODO: Can we do without additional allocations here?
        self.string.push_str(&escape_string(string));
        Ok(())
    }

    fn int(&mut self, int: i64) -> Result<(), Self::Error> {
        if self.needs_whitespace {
            self.string.push(' ');
        }

        self.needs_whitespace = true;
        let _ = write!(&mut self.string, "{}", int);
        Ok(())
    }

    #[inline]
    fn list<F>(&mut self, f: F) -> Result<(), Self::Error>
    where
        F: FnOnce(&mut Self) -> Result<(), Self::Error>,
    {
        self.print_delimited('(', ')', f)
    }

    #[inline]
    fn seq<F>(&mut self, f: F) -> Result<(), Self::Error>
    where
        F: FnOnce(&mut Self) -> Result<(), Self::Error>,
    {
        self.print_delimited('[', ']', f)
    }

    #[inline]
    fn map<F>(&mut self, f: F) -> Result<(), Self::Error>
    where
        F: FnOnce(&mut Self) -> Result<(), Self::Error>,
    {
        self.print_delimited('{', '}', f)
    }

    #[inline]
    fn group<F>(&mut self, f: F) -> Result<(), Self::Error>
    where
        F: FnOnce(&mut Self) -> Result<(), Self::Error>,
    {
        f(self)
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

/// Print a `T` into an s-expression string.
///
/// This function does not produce any line breaks, indentation, or unnecessary whitespace.
/// It is intended to be used when the output is intended to be consumed by another program.
/// Where human readability is a concern, consider using the [`to_string_pretty`] function instead.
///
/// The `context` parameter is passed to the `Print` implementation and can be used to pass
/// additional information that may be required to print values of type `T`. When no context
/// is needed, the `()` value can be passed as the context.
///
/// [`to_string_pretty`]: `crate::printer::to_string_pretty`
pub fn to_string<T: Print<C>, C>(value: T, ctx: C) -> String {
    let mut printer = SimplePrinter::new(ctx);
    let _ = value.print(&mut printer);
    printer.string
}

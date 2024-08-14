//! Print values into s-expressions.
use crate::escape::{escape_string, escape_symbol};
use std::convert::Infallible;
use std::fmt::Write as _;
use std::rc::Rc;
use std::sync::Arc;
mod pretty;
pub use pretty::{to_string_pretty, to_string_pretty_with_ctx};

/// Trait for types that can print s-expressions.
pub trait Printer<C = ()>: Sized {
    type Error;

    /// Print a symbol.
    fn symbol(&mut self, symbol: &str) -> Result<(), Self::Error>;

    /// Print a string.
    fn string(&mut self, string: &str) -> Result<(), Self::Error>;

    /// Print an integer.
    fn int(&mut self, int: i64) -> Result<(), Self::Error>;

    /// Print a list given a function that prints the contents.
    fn list<F>(&mut self, f: F) -> Result<(), Self::Error>
    where
        F: FnOnce(&mut Self) -> Result<(), Self::Error>;

    /// Print a seq given a function that prints the contents.
    fn seq<F>(&mut self, f: F) -> Result<(), Self::Error>
    where
        F: FnOnce(&mut Self) -> Result<(), Self::Error>;

    /// Print a map given a function that prints the contents.
    fn map<F>(&mut self, f: F) -> Result<(), Self::Error>
    where
        F: FnOnce(&mut Self) -> Result<(), Self::Error>;

    /// Print a group given a function that prints the contents.
    ///
    /// Groups are not explicitly delimited in the output, but represent items that
    /// semantically belong together. The printer can use this as a hint
    /// to guide the layout.
    fn group<F>(&mut self, f: F) -> Result<(), Self::Error>
    where
        F: FnOnce(&mut Self) -> Result<(), Self::Error>;

    /// Print a printable value.
    fn print(&mut self, value: impl Print<C>) -> Result<(), Self::Error> {
        value.print(self)
    }

    fn context(&self) -> &C;
    fn context_mut(&mut self) -> &mut C;
}

/// Trait for types that can be printed as an s-expression.
pub trait Print<C = ()> {
    fn print<P: Printer<C>>(&self, printer: &mut P) -> Result<(), P::Error>;
}

impl<T: Print<C> + Sized, C> Print<C> for &T {
    #[inline]
    fn print<P: Printer<C>>(&self, printer: &mut P) -> Result<(), P::Error> {
        (*self).print(printer)
    }
}

impl<T: Print<C>, C> Print<C> for Box<T> {
    #[inline]
    fn print<P: Printer<C>>(&self, printer: &mut P) -> Result<(), P::Error> {
        printer.print(self.as_ref())
    }
}

impl<T: Print<C>, C> Print<C> for Rc<T> {
    #[inline]
    fn print<P: Printer<C>>(&self, printer: &mut P) -> Result<(), P::Error> {
        printer.print(self.as_ref())
    }
}

impl<T: Print<C>, C> Print<C> for Arc<T> {
    #[inline]
    fn print<P: Printer<C>>(&self, printer: &mut P) -> Result<(), P::Error> {
        printer.print(self.as_ref())
    }
}

impl<T: Print<C>, C> Print<C> for Vec<T> {
    #[inline]
    fn print<P: Printer<C>>(&self, printer: &mut P) -> Result<(), P::Error> {
        for item in self {
            printer.print(item)?;
        }
        Ok(())
    }
}

impl<C> Print<C> for str {
    fn print<P: Printer<C>>(&self, printer: &mut P) -> Result<(), P::Error> {
        printer.string(self)
    }
}

/// Implement [`Print`] by using [`Display`] to convert a value to an atom.
///
/// [`Display`]: [`std::fmt::Display`]
#[macro_export]
macro_rules! impl_print_by_display {
    ($($ident:ty),*) => {
        $(impl $crate::printer::Print for $ident {
            fn print<P: Printer>(&self, printer: &mut P) -> ::std::result::Result<(), P::Error> {
                printer.atom(&self.to_string())
            }
        })*
    };
}

pub use impl_print_by_display;

// impl_print_by_display!(&str, SmolStr, String);
// impl_print_by_display!(u8, u16, u32, u64, u128);
// impl_print_by_display!(i8, i16, i32, i64, i128);
// impl_print_by_display!(f32, f64);
// impl_print_by_display!(bool);

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
#[inline]
pub fn to_string<T: Print>(value: T) -> String {
    to_string_with_ctx(value, ())
}

/// Print a `T` into an s-expression string with given a context.
pub fn to_string_with_ctx<T: Print<C>, C>(value: T, ctx: C) -> String {
    let mut printer = SimplePrinter::new(ctx);
    let _ = value.print(&mut printer);
    printer.string
}

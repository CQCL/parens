//! Print values into s-expressions.
use crate::escape::escape_string;
use smol_str::SmolStr;
use std::convert::Infallible;
use std::rc::Rc;
use std::sync::Arc;
mod pretty;
pub use pretty::to_string_pretty;

pub trait Printer: Sized {
    type Error;

    fn atom(&mut self, atom: &str) -> Result<(), Self::Error>;
    fn list<F>(&mut self, f: F) -> Result<(), Self::Error>
    where
        F: FnOnce(&mut Self) -> Result<(), Self::Error>;

    fn print(&mut self, value: impl Print) -> Result<(), Self::Error> {
        value.print(self)
    }
}

/// Trait for types that can be printed as an s-expression.
pub trait Print {
    fn print<P: Printer>(&self, printer: &mut P) -> Result<(), P::Error>;
}

impl<T: Print + Sized> Print for &T {
    #[inline]
    fn print<P: Printer>(&self, printer: &mut P) -> Result<(), P::Error> {
        (*self).print(printer)
    }
}

impl<T: Print> Print for Box<T> {
    #[inline]
    fn print<P: Printer>(&self, printer: &mut P) -> Result<(), P::Error> {
        printer.print(&*self)
    }
}

impl<T: Print> Print for Rc<T> {
    #[inline]
    fn print<P: Printer>(&self, printer: &mut P) -> Result<(), P::Error> {
        printer.print(&*self)
    }
}

impl<T: Print> Print for Arc<T> {
    #[inline]
    fn print<P: Printer>(&self, printer: &mut P) -> Result<(), P::Error> {
        printer.print(&*self)
    }
}

impl<T: Print> Print for Vec<T> {
    #[inline]
    fn print<P: Printer>(&self, printer: &mut P) -> Result<(), P::Error> {
        for item in self {
            printer.print(item)?;
        }
        Ok(())
    }
}

impl Print for str {
    fn print<P: Printer>(&self, printer: &mut P) -> Result<(), P::Error> {
        printer.atom(self)
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

impl_print_by_display!(&str, SmolStr, String);
impl_print_by_display!(u8, u16, u32, u64, u128);
impl_print_by_display!(i8, i16, i32, i64, i128);
impl_print_by_display!(f32, f64);
impl_print_by_display!(bool);

struct SimplePrinter {
    needs_whitespace: bool,
    string: String,
}

impl SimplePrinter {
    pub fn new() -> Self {
        Self {
            needs_whitespace: false,
            string: String::new(),
        }
    }
}

impl Printer for SimplePrinter {
    type Error = Infallible;

    fn atom(&mut self, atom: &str) -> Result<(), Self::Error> {
        if self.needs_whitespace {
            self.string.push(' ');
        }

        self.needs_whitespace = true;
        // TODO: Can we do without additional allocations here?
        self.string.push_str(&escape_string(atom));
        Ok(())
    }

    fn list<F>(&mut self, f: F) -> Result<(), Self::Error>
    where
        F: FnOnce(&mut Self) -> Result<(), Self::Error>,
    {
        if self.needs_whitespace {
            self.string.push(' ');
        }

        self.string.push('(');
        self.needs_whitespace = false;
        f(self)?;
        self.string.push(')');
        self.needs_whitespace = true;

        Ok(())
    }
}

/// Print a `T` into an s-expression string.
pub fn to_string<T>(value: T) -> String
where
    T: Print,
{
    let mut printer = SimplePrinter::new();
    let _ = value.print(&mut printer);
    printer.string
}

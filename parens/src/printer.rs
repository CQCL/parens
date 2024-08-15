//! Print values into s-expressions.
//!
//! Printers may be provided with a context that is passed down to all print functions.
//! This allows to print values whose text representation may only be determined given
//! additional context, for instance when using string interning.
use std::rc::Rc;
use std::sync::Arc;
mod pretty;
mod simple;
pub use pretty::to_string_pretty;
pub use simple::to_string;

/// Trait for types that can print s-expressions.
///
/// The parser carries a context `C` that is passed down to all print functions.
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

    /// Reference to the context.
    fn context(&self) -> &C;

    /// Mutable reference to the context.
    fn context_mut(&mut self) -> &mut C;
}

/// Trait for types that can be printed as an s-expression.
///
/// The type parameter `C` is the type of the context that is passed to the printer.
/// Some types may only be printed in a specific context. If the type does not require
/// any context, we encourage the trait implementation to be polymorphic over `C`
/// to allow mixing types that require context with those that do not.
pub trait Print<C> {
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

/// A macro for defining a keyword token.
///
/// This macro defines a new type that parses and prints as a fixed symbol.
///
/// # Examples
///
/// ```
/// # use parens::util::make_keyword;
/// # use parens::{from_str, to_string};
/// make_keyword!(unit);
/// assert_eq!(from_str::<unit, ()>("unit", ()).unwrap(), unit);
/// assert_eq!(to_string(unit, ()), "unit");
/// ```
///
/// We can define keywords whose symbol representation is different to the name
/// of the generated type. This is particularly useful when the symbol is not
/// a valid type name in Rust.
/// ```
/// # use parens::util::make_keyword;
/// # use parens::{from_str, to_string};
/// make_keyword!(r#fn => "fn");
/// assert_eq!(from_str::<r#fn, ()>("fn", ()).unwrap(), r#fn);
/// assert_eq!(to_string(r#fn, ()), "fn");
/// ```
#[macro_export]
macro_rules! make_keyword {
    ($name:ident) => { make_keyword!($name => stringify!($name)); };
    ($name:ident => $atom:expr) => {
        #[allow(non_camel_case_types)]
        #[allow(dead_code)]
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub struct $name;

        impl<C> $crate::parser::Parse<C> for $name {
            fn parse(parser: &mut $crate::parser::Parser<'_, C>) -> $crate::parser::Result<Self> {
                parser.step(|cursor| match cursor.symbol() {
                    Some((symbol, rest)) if symbol.as_str() == $atom => Ok((Self, rest)),
                    _ => Err(cursor.error(format!("expected {}", $atom)))
                })
            }
        }

        impl $crate::parser::Peek for $name {
            fn peek(cursor: $crate::parser::Cursor<'_>) -> bool {
                cursor.peek_symbol(|atom| atom.as_ref() == $atom)
            }
        }

        impl<C> $crate::printer::Print<C> for $name {
            fn print<P: $crate::printer::Printer<C>>(&self, printer: &mut P) -> Result<(), P::Error> {
                printer.symbol($atom)
            }
        }
    }
}

pub use make_keyword;

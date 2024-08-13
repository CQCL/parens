#[macro_export]
macro_rules! make_keyword {
    ($name:ident) => { make_keyword!($name => stringify!($name)); };
    ($name:ident => $atom:expr) => {
        #[allow(non_camel_case_types)]
        #[allow(dead_code)]
        #[derive(Debug, Clone)]
        pub struct $name;

        impl $crate::parser::Parse for $name {
            fn parse(parser: &mut $crate::parser::Parser<'_>) -> $crate::parser::Result<Self> {
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

        impl $crate::printer::Print for $name {
            fn print<P: $crate::printer::Printer>(&self, printer: &mut P) -> Result<(), P::Error> {
                printer.symbol($atom)
            }
        }
    }
}

pub use make_keyword;

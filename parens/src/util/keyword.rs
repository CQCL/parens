#[macro_export]
macro_rules! make_keyword {
    ($name:ident) => { make_keyword!($name => stringify!($name)); };
    ($name:ident => $atom:expr) => {
        #[allow(non_camel_case_types)]
        #[allow(dead_code)]
        #[derive(Debug, Clone)]
        pub struct $name($crate::parser::Span);

        impl $crate::parser::Parse for $name {
            fn parse(parser: &mut $crate::parser::Parser<'_>) -> $crate::parser::Result<Self> {
                let atom = parser.atom()?;

                if atom.as_str() != $atom {
                    return Err(parser.error(format!("expected {}", $atom)));
                }

                Ok(Self(parser.span()))
            }
        }

        impl $crate::parser::Peek for $name {
            fn peek(cursor: $crate::parser::Cursor<'_>) -> bool {
                cursor.peek_atom(|atom| atom.as_ref() == $atom)
            }
        }

        impl $crate::printer::Print for $name {
            fn print<P: $crate::printer::Printer>(&self, printer: &mut P) -> Result<(), P::Error> {
                printer.print($atom)
            }
        }
    }
}

pub use make_keyword;

use crate::parser::{Parse, Parser};
use crate::printer::{Print, Printer};
use proptest::arbitrary::Arbitrary;
use smol_str::SmolStr;

/// An s-expression represented as a recursive enum.
///
/// This type is mainly provided for testing and debugging purposes.
/// To parse or print a user-defined type from and into s-expressions,
/// we do not recommend using this type. Instead, implement the [`Parse`] and [`Print`]
/// traits for your type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    /// A list of values enclosed in parentheses `(` and `)`.
    List(Vec<Value>),
    /// A sequence of values enclosed in square brackets `[` and `]`.
    Seq(Vec<Value>),
    /// A map of values enclosed in curly braces `{` and `}`.
    Map(Vec<Value>),
    /// A string value.
    String(SmolStr),
    /// A symbol.
    Symbol(SmolStr),
    /// An integer value.
    Int(i64),
}

impl<C> Parse<C> for Value {
    fn parse(parser: &mut Parser<'_, C>) -> crate::parser::Result<Self> {
        if parser.cursor().peek_list(|_| true) {
            parser.list(|parser| Ok(Value::List(parser.parse()?)))
        } else if parser.cursor().peek_seq(|_| true) {
            parser.seq(|parser| Ok(Value::Seq(parser.parse()?)))
        } else if parser.cursor().peek_map(|_| true) {
            parser.map(|parser| Ok(Value::Map(parser.parse()?)))
        } else if parser.cursor().peek_string(|_| true) {
            Ok(Value::String(parser.string().cloned()?))
        } else if parser.cursor().peek_int(|_| true) {
            Ok(Value::Int(parser.int()?))
        } else {
            Ok(Value::Symbol(parser.symbol().cloned()?))
        }
    }
}

impl<C> Print<C> for Value {
    fn print<P: Printer<C>>(&self, printer: &mut P) -> Result<(), P::Error> {
        match self {
            Value::List(items) => printer.list(|printer| printer.print(items)),
            Value::Seq(items) => printer.seq(|printer| printer.print(items)),
            Value::Map(items) => printer.map(|printer| printer.print(items)),
            Value::String(string) => printer.string(string),
            Value::Symbol(symbol) => printer.symbol(symbol),
            Value::Int(int) => printer.int(*int),
        }
    }
}

impl Arbitrary for Value {
    type Parameters = ();
    type Strategy = proptest::strategy::BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        use proptest::prelude::*;

        let leaf = prop_oneof![
            any::<String>().prop_map(|s| Value::String(s.into())),
            any::<String>().prop_map(|s| Value::Symbol(s.into())),
            any::<i64>().prop_map(Value::Int),
        ];

        leaf.prop_recursive(8, 256, 10, |inner| {
            proptest::prop_oneof![
                proptest::collection::vec(inner.clone(), 0..10).prop_map(Value::List),
                proptest::collection::vec(inner.clone(), 0..10).prop_map(Value::Map),
                proptest::collection::vec(inner, 0..10).prop_map(Value::Seq)
            ]
        })
        .boxed()
    }
}

#[cfg(test)]
mod test {
    use crate::{from_str, to_string, to_string_pretty, util::Value};
    use proptest::prelude::*;

    proptest! {
        /// Ensure that we can round-trip arbitrary values with simple printing.
        #[test]
        fn print_then_parse(values: Vec<Value>) {
            let sexp = to_string(&values, ());
            let parsed: Vec<Value> = from_str(&sexp, ()).unwrap();
            assert_eq!(values, parsed);
        }

        /// Ensure that we can round-trip arbitrary values with pretty printing.
        #[test]
        fn pretty_print_then_parse(values: Vec<Value>, width in 0..120usize) {
            let sexp = to_string_pretty(&values, width, ());
            let parsed: Vec<Value> = from_str(&sexp, ()).unwrap();
            assert_eq!(values, parsed);
        }
    }
}

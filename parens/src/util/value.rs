use crate::parser::{Parse, Parser};
use crate::printer::{Print, Printer};
use proptest::arbitrary::Arbitrary;
use smol_str::SmolStr;

/// An s-expression represented as a recursive enum.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    List(Vec<Value>),
    Seq(Vec<Value>),
    Map(Vec<Value>),
    String(SmolStr),
    Symbol(SmolStr),
    Int(i64),
}

impl Parse for Value {
    fn parse(parser: &mut Parser<'_>) -> crate::parser::Result<Self> {
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

impl Print for Value {
    fn print<P: Printer>(&self, printer: &mut P) -> Result<(), P::Error> {
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
        #[test]
        fn print_then_parse(values: Vec<Value>) {
            let sexp = to_string(&values);
            let parsed: Vec<Value> = from_str(&sexp).unwrap();
            assert_eq!(values, parsed);
        }

        #[test]
        fn pretty_print_then_parse(values: Vec<Value>, width in 0..120usize) {
            let sexp = to_string_pretty(&values, width);
            println!("`{}`", sexp);
            let parsed: Vec<Value> = from_str(&sexp).unwrap();
            assert_eq!(values, parsed);
        }
    }
}

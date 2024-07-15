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
    Atom(SmolStr),
}

impl Parse for Value {
    fn parse(parser: &mut Parser<'_>) -> crate::parser::Result<Self> {
        if parser.cursor().peek_list(|_| true) {
            parser.list(|parser| Ok(Value::List(parser.parse()?)))
        } else if parser.cursor().peek_seq(|_| true) {
            parser.seq(|parser| Ok(Value::Seq(parser.parse()?)))
        } else if parser.cursor().peek_map(|_| true) {
            parser.map(|parser| Ok(Value::Map(parser.parse()?)))
        } else {
            Ok(Value::Atom(parser.parse()?))
        }
    }
}

impl Print for Value {
    fn print<P: Printer>(&self, printer: &mut P) -> Result<(), P::Error> {
        match self {
            Value::List(items) => printer.list(|printer| printer.print(items)),
            Value::Seq(items) => printer.seq(|printer| printer.print(items)),
            Value::Map(items) => printer.map(|printer| printer.print(items)),
            Value::Atom(atom) => printer.atom(atom),
        }
    }
}

impl From<SmolStr> for Value {
    fn from(value: SmolStr) -> Self {
        Self::Atom(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::Atom(value.into())
    }
}

impl Arbitrary for Value {
    type Parameters = ();
    type Strategy = proptest::strategy::BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        use proptest::prelude::*;

        let leaf = any::<String>().prop_map(Value::from);
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
            let parsed: Vec<Value> = from_str(&sexp).unwrap();
            assert_eq!(values, parsed);
        }
    }
}

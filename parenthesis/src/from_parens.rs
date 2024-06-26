//! Types that can be constructed from s-expressions.
use smol_str::SmolStr;
use std::{error::Error, fmt::Display};
use thiserror::Error;

use crate::{Symbol, Value};

/// Input stream that emits s-expression tokens.
pub trait InputStream: Sized {
    /// Span that identifies the location of a token.
    type Span;

    /// Advance to the next token and return it.
    fn next(&mut self) -> Option<TokenTree<Self>>;

    /// Return the next token without advancing.
    fn peek(&self) -> Option<TokenTree<Self>>;

    /// The span of the last token returned by [`InputStream::next`].
    fn span(&self) -> Self::Span;

    /// The span of the parent stream, if any.
    ///
    /// For the root stream of some input, this returns the span of the entire input.
    fn parent_span(&self) -> Self::Span;

    /// Return whether there is no next token.
    fn is_end(&self) -> bool {
        self.peek().is_none()
    }
}

impl InputStream for &[Value] {
    type Span = ();

    #[inline]
    fn next(&mut self) -> Option<TokenTree<Self>> {
        let (value, rest) = self.split_first()?;
        *self = rest;
        Some(value_to_token(value))
    }

    #[inline]
    fn peek(&self) -> Option<TokenTree<Self>> {
        let value = self.first()?;
        Some(value_to_token(value))
    }

    #[inline]
    fn span(&self) -> Self::Span {}

    #[inline]
    fn parent_span(&self) -> Self::Span {}

    #[inline]
    fn is_end(&self) -> bool {
        self.is_empty()
    }
}

fn value_to_token(value: &Value) -> TokenTree<&[Value]> {
    match value {
        Value::List(list) => TokenTree::List(list),
        Value::String(string) => TokenTree::String(string.clone()),
        Value::Symbol(symbol) => TokenTree::Symbol(symbol.clone()),
        Value::Bool(bool) => TokenTree::Bool(*bool),
        Value::Int(int) => TokenTree::Int(*int),
        Value::Float(float) => TokenTree::Float(float.into_inner()),
    }
}

/// Types that can be constructed from s-expressions.
pub trait FromParens<I>: Sized
where
    I: InputStream,
{
    /// Parse a value by consuming tokens from an input stream.
    fn from_parens(stream: &mut I) -> Result<Self, ParseError<I::Span>>;
}

impl<I: InputStream> FromParens<I> for SmolStr {
    #[inline]
    fn from_parens(stream: &mut I) -> Result<Self, ParseError<I::Span>> {
        let Some(TokenTree::String(string)) = stream.next() else {
            return Err(ParseError::new("expected string", stream.span()));
        };

        Ok(string)
    }
}

impl<I: InputStream> FromParens<I> for String {
    #[inline]
    fn from_parens(stream: &mut I) -> Result<Self, ParseError<I::Span>> {
        let Some(TokenTree::String(string)) = stream.next() else {
            return Err(ParseError::new("expected string", stream.span()));
        };

        Ok(string.into())
    }
}

impl<I: InputStream> FromParens<I> for Symbol {
    #[inline]
    fn from_parens(stream: &mut I) -> Result<Self, ParseError<I::Span>> {
        let Some(TokenTree::Symbol(symbol)) = stream.next() else {
            return Err(ParseError::new("expected symbol", stream.span()));
        };

        Ok(symbol)
    }
}

impl<I: InputStream> FromParens<I> for Value {
    fn from_parens(stream: &mut I) -> Result<Self, ParseError<I::Span>> {
        let Some(token_tree) = stream.next() else {
            return Err(ParseError::new("expected value", stream.span()));
        };

        let value = match token_tree {
            TokenTree::List(mut list) => Value::List(FromParens::from_parens(&mut list)?),
            TokenTree::String(string) => Value::from(string),
            TokenTree::Symbol(symbol) => Value::from(symbol),
            TokenTree::Bool(bool) => Value::from(bool),
            TokenTree::Int(int) => Value::from(int),
            TokenTree::Float(float) => Value::from(float),
        };

        Ok(value)
    }
}

impl<I: InputStream> FromParens<I> for i64 {
    fn from_parens(stream: &mut I) -> Result<Self, ParseError<I::Span>> {
        let Some(TokenTree::Int(int)) = stream.next() else {
            return Err(ParseError::new("expected int", stream.span()));
        };

        Ok(int)
    }
}

impl<I: InputStream> FromParens<I> for f64 {
    fn from_parens(stream: &mut I) -> Result<Self, ParseError<I::Span>> {
        let Some(TokenTree::Float(float)) = stream.next() else {
            return Err(ParseError::new("expected float", stream.span()));
        };

        Ok(float)
    }
}

impl<I: InputStream, V> FromParens<I> for Vec<V>
where
    V: FromParens<I>,
{
    fn from_parens(stream: &mut I) -> Result<Self, ParseError<I::Span>> {
        let mut values = Vec::new();

        while !stream.is_end() {
            values.push(V::from_parens(stream)?);
        }

        Ok(values)
    }
}

/// Error while parsing a value.
#[derive(Debug, Error)]
pub enum ParseError<S> {
    /// Parse error together with a span.
    #[error("{message}")]
    Error {
        /// Error message.
        message: String,
        /// Span that indicates where the error occured.
        span: S,
    },
    /// Custom errors
    #[error(transparent)]
    Other(#[from] Box<dyn Error + 'static>),
}

impl<S> ParseError<S> {
    /// Construct a new [`ParseError`] given a message and span.
    pub fn new(message: impl Display, span: S) -> Self {
        Self::Error {
            message: format!("{}", message),
            span,
        }
    }
}

/// Individual token returned by an [`InputStream`].
#[derive(Debug, Clone)]
pub enum TokenTree<L> {
    /// A list with a nested [`InputStream`].
    List(L),
    /// A string.
    String(SmolStr),
    /// A symbol.
    Symbol(Symbol),
    /// A boolean.
    Bool(bool),
    /// An integer.
    Int(i64),
    /// A float.
    Float(f64),
}

#[cfg(feature = "macros")]
#[cfg_attr(docsrs, doc(cfg(feature = "macros")))]
pub use parenthesis_macros::FromParens;

#[cfg(test)]
mod test {
    use crate::{FromParens, Value};
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn parse_values_from_values(values: Vec<Value>) {
            let result = Vec::<Value>::from_parens(&mut values.as_slice()).unwrap();
            assert_eq!(values, result);
        }

        #[test]
        fn parse_value_from_values(value: Value) {
            let values = [value; 1];
            let result = Value::from_parens(&mut values.as_slice()).unwrap();
            assert_eq!(values[0], result);
        }
    }
}

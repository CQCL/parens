//! Parse values from s-expressions.
use crate::lexer::lex;
use delegate::delegate;
use smol_str::SmolStr;
use std::fmt::Display;
use std::ops::Range;
use std::rc::Rc;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub(crate) enum Token {
    List(usize),
    Seq(usize),
    Map(usize),
    Atom(SmolStr),
}

/// A lexed representation of an s-expression string that can be parsed.
#[derive(Debug, Clone)]
pub struct ParseBuffer<'a> {
    pub(crate) source: &'a str,
    pub(crate) tokens: Vec<Token>,
    pub(crate) spans: Vec<Span>,
}

impl<'a> ParseBuffer<'a> {
    pub fn new(str: &'a str) -> Result<Self> {
        lex(str).map_err(|err| ParseError::new(&err, err.span()))
    }

    #[inline]
    pub fn cursor(&'a self) -> Cursor<'a> {
        Cursor {
            buffer: self,
            index: 0,
            end_index: self.tokens.len(),
            parent: None,
        }
    }

    #[inline]
    pub fn parser(&'a self) -> Parser<'a> {
        Parser::new(self.cursor())
    }
}

/// A parser that is stepping through a [`ParseBuffer`].
#[derive(Clone)]
pub struct Parser<'a> {
    cursor: Cursor<'a>,
}

impl<'a> Parser<'a> {
    #[inline]
    pub fn new(cursor: Cursor<'a>) -> Self {
        Self { cursor }
    }

    #[inline]
    pub fn parse<T>(&mut self) -> Result<T>
    where
        T: Parse,
    {
        T::parse(self)
    }

    #[inline]
    pub fn peek<T>(&self) -> bool
    where
        T: Peek,
    {
        T::peek(self.cursor)
    }

    pub fn atom(&mut self) -> Result<&'a SmolStr> {
        self.step(|cursor| cursor.atom().ok_or_else(|| cursor.error("expected atom")))
    }

    pub fn list<T, F>(&mut self, f: F) -> Result<T>
    where
        F: FnOnce(&mut Self) -> Result<T>,
    {
        self.step(|cursor| {
            let (inner, after) = cursor.list().ok_or_else(|| cursor.error("expected list"))?;
            let mut inner = Parser::new(inner);
            let result = f(&mut inner)?;

            if !inner.is_empty() {
                return Err(inner.error("expected end of list"));
            }

            Ok((result, after))
        })
    }

    pub fn seq<T, F>(&mut self, f: F) -> Result<T>
    where
        F: FnOnce(&mut Self) -> Result<T>,
    {
        self.step(|cursor| {
            let (inner, after) = cursor.seq().ok_or_else(|| cursor.error("expected seq"))?;
            let mut inner = Parser::new(inner);
            let result = f(&mut inner)?;

            if !inner.is_empty() {
                return Err(inner.error("expected end of seq"));
            }

            Ok((result, after))
        })
    }

    pub fn map<T, F>(&mut self, f: F) -> Result<T>
    where
        F: FnOnce(&mut Self) -> Result<T>,
    {
        self.step(|cursor| {
            let (inner, after) = cursor.map().ok_or_else(|| cursor.error("expected map"))?;
            let mut inner = Parser::new(inner);
            let result = f(&mut inner)?;

            if !inner.is_empty() {
                return Err(inner.error("expected end of map"));
            }

            Ok((result, after))
        })
    }

    #[inline]
    pub fn step<T, F>(&mut self, f: F) -> Result<T>
    where
        F: FnOnce(Cursor<'a>) -> Result<(T, Cursor<'a>)>,
    {
        let (result, cursor) = f(self.cursor)?;
        self.cursor = cursor;
        Ok(result)
    }

    #[inline]
    pub fn cursor(&self) -> Cursor<'a> {
        self.cursor
    }

    delegate! {
        to self.cursor {
            pub fn is_empty(&self) -> bool;
            pub fn span(&self) -> Span;
            pub fn parent_span(&self) -> Span;
            pub fn error(&self, message: impl Display) -> ParseError;
        }
    }
}

/// A position into a [`ParseBuffer`].
#[derive(Debug, Clone, Copy)]
pub struct Cursor<'a> {
    buffer: &'a ParseBuffer<'a>,
    index: usize,
    end_index: usize,
    parent: Option<usize>,
}

impl<'a> Cursor<'a> {
    pub fn parse<T>(self) -> Result<(T, Self)>
    where
        T: Parse,
    {
        let mut parser = Parser::new(self);
        let result = parser.parse()?;
        let cursor = parser.cursor();
        Ok((result, cursor))
    }

    #[inline]
    pub fn peek<T>(self) -> bool
    where
        T: Peek,
    {
        T::peek(self)
    }

    pub fn error(self, message: impl Display) -> ParseError {
        ParseError::new(message, self.span())
    }

    pub fn advance(self) -> Option<Self> {
        let size = match self.get()? {
            Token::List(size) => size + 1,
            Token::Seq(size) => size + 1,
            Token::Map(size) => size + 1,
            Token::Atom(_) => 1,
        };
        Some(Cursor {
            buffer: self.buffer,
            index: self.index + size,
            end_index: self.end_index,
            parent: self.parent,
        })
    }

    pub fn atom(self) -> Option<(&'a SmolStr, Self)> {
        let Token::Atom(atom) = self.get()? else {
            return None;
        };

        let after = Cursor {
            buffer: self.buffer,
            index: self.index + 1,
            end_index: self.end_index,
            parent: self.parent,
        };

        Some((atom, after))
    }

    pub fn list(self) -> Option<(Self, Self)> {
        match self.get()? {
            Token::List(size) => Some(self.split(1, *size)),
            _ => None,
        }
    }

    pub fn seq(self) -> Option<(Self, Self)> {
        match self.get()? {
            Token::Seq(size) => Some(self.split(1, *size)),
            _ => None,
        }
    }

    pub fn map(self) -> Option<(Self, Self)> {
        match self.get()? {
            Token::Map(size) => Some(self.split(1, *size)),
            _ => None,
        }
    }

    fn split(self, skip: usize, size: usize) -> (Self, Self) {
        let left = Cursor {
            buffer: self.buffer,
            index: self.index + skip,
            end_index: self.index + size + skip,
            parent: Some(self.index),
        };

        let right = Cursor {
            buffer: self.buffer,
            index: self.index + skip + size,
            end_index: self.end_index,
            parent: self.parent,
        };

        (left, right)
    }

    pub fn peek_atom(self, f: impl FnOnce(&SmolStr) -> bool) -> bool {
        match self.atom() {
            Some((atom, _)) => f(atom),
            None => false,
        }
    }

    pub fn peek_list(self, f: impl FnOnce(Self) -> bool) -> bool {
        match self.list() {
            Some((inner, _)) => f(inner),
            None => false,
        }
    }

    pub fn peek_seq(self, f: impl FnOnce(Self) -> bool) -> bool {
        match self.seq() {
            Some((inner, _)) => f(inner),
            None => false,
        }
    }

    pub fn peek_map(self, f: impl FnOnce(Self) -> bool) -> bool {
        match self.map() {
            Some((inner, _)) => f(inner),
            None => false,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.index >= self.end_index
    }

    #[inline]
    fn get(&self) -> Option<&'a Token> {
        if self.is_empty() {
            None
        } else {
            Some(&self.buffer.tokens[self.index])
        }
    }

    #[inline]
    pub fn span(&self) -> Span {
        if self.is_empty() {
            let offset = self.parent_span().end;
            offset..offset
        } else {
            self.buffer.spans[self.index].clone()
        }
    }

    #[inline]
    pub fn parent_span(&self) -> Span {
        match self.parent {
            Some(parent) => self.buffer.spans[parent].clone(),
            None => 0..self.buffer.source.len(),
        }
    }
}

/// Trait for types that can be parsed from an s-expression.
pub trait Parse: Sized {
    fn parse(parser: &mut Parser<'_>) -> Result<Self>;
}

impl<V: Parse> Parse for Vec<V> {
    fn parse(parser: &mut Parser<'_>) -> Result<Self> {
        let mut values = Vec::new();
        while !parser.is_empty() {
            values.push(parser.parse()?);
        }
        Ok(values)
    }
}

impl<V: Parse> Parse for Box<V> {
    fn parse(parser: &mut Parser<'_>) -> Result<Self> {
        Ok(Box::new(parser.parse()?))
    }
}

impl<V: Parse> Parse for Rc<V> {
    fn parse(parser: &mut Parser<'_>) -> Result<Self> {
        Ok(Rc::new(parser.parse()?))
    }
}

impl<V: Parse> Parse for Arc<V> {
    fn parse(parser: &mut Parser<'_>) -> Result<Self> {
        Ok(Arc::new(parser.parse()?))
    }
}

/// Implement [`Parse`] by using [`FromStr`] to convert an atom into a value.
///
/// [`FromStr`]: [`std::str::FromStr`]
#[macro_export]
macro_rules! impl_parse_by_from_str {
    ($($ident:ty),*) => {
        $(impl $crate::parser::Parse for $ident {
            fn parse(parser: &mut Parser<'_>) -> $crate::parser::Result<Self> {
                let atom = parser.atom()?;
                <$ident as ::std::str::FromStr>::from_str(atom.as_ref())
                    .map_err(|err| parser.error(err))
            }
        })*
    };
}

pub use impl_parse_by_from_str;

impl_parse_by_from_str!(SmolStr, String);
impl_parse_by_from_str!(u8, u16, u32, u64, u128);
impl_parse_by_from_str!(i8, i16, i32, i64, i128);
impl_parse_by_from_str!(f32, f64);
impl_parse_by_from_str!(bool);

pub trait Peek: Sized {
    fn peek(cursor: Cursor<'_>) -> bool;
}

/// Implement [`Peek`] by using [`FromStr`] to convert an atom into a value.
///
/// [`FromStr`]: [`std::str::FromStr`]
#[macro_export]
macro_rules! impl_peek_by_from_str {
    ($($ident:ty),*) => {
        $(impl $crate::parser::Peek for $ident {
            fn peek(cursor: Cursor<'_>) -> bool {
                match cursor.atom() {
                    Some((atom, _)) => <$ident as ::std::str::FromStr>::from_str(atom.as_ref()).is_ok(),
                    None => false,
                }
            }
        })*
    };
}

pub use impl_peek_by_from_str;

impl_peek_by_from_str!(SmolStr, String);
impl_peek_by_from_str!(u8, u16, u32, u64, u128);
impl_peek_by_from_str!(i8, i16, i32, i64, i128);
impl_peek_by_from_str!(f32, f64);
impl_peek_by_from_str!(bool);

/// A parse error.
#[derive(Debug, thiserror::Error)]
#[error("{message}")]
pub struct ParseError {
    message: String,
    span: Span,
}

impl ParseError {
    pub fn new(message: impl Display, span: Span) -> Self {
        ParseError {
            message: message.to_string(),
            span,
        }
    }

    pub fn span(&self) -> Span {
        self.span.clone()
    }
}

/// Shorthand for a result specialised to parse errors.
pub type Result<T, E = ParseError> = std::result::Result<T, E>;

/// Span within a string.
pub type Span = Range<usize>;

/// Parse a `T` from an s-expression string.
pub fn from_str<T: Parse>(source: &str) -> Result<T> {
    let buffer = ParseBuffer::new(source)?;
    let mut parser = buffer.parser();
    T::parse(&mut parser)
}

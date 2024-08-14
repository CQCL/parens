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
    Symbol(SmolStr),
    String(SmolStr),
    Int(i64),
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
    pub fn parser<C>(&'a self, context: C) -> Parser<'a, C> {
        Parser::new(self.cursor(), context)
    }
}

/// A parser that is stepping through a [`ParseBuffer`].
#[derive(Clone)]
pub struct Parser<'a, C> {
    cursor: Cursor<'a>,
    context: C,
}

impl<'a, C> Parser<'a, C> {
    #[inline]
    pub fn new(cursor: Cursor<'a>, context: C) -> Self {
        Self { cursor, context }
    }

    #[inline]
    pub fn parse<T>(&mut self) -> Result<T>
    where
        T: Parse<C>,
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

    pub fn symbol(&mut self) -> Result<&'a SmolStr> {
        self.step(|cursor| {
            cursor
                .symbol()
                .ok_or_else(|| cursor.error("expected symbol"))
        })
    }

    pub fn int(&mut self) -> Result<i64> {
        self.step(|cursor| cursor.int().ok_or_else(|| cursor.error("expected integer")))
    }

    pub fn string(&mut self) -> Result<&'a SmolStr> {
        self.step(|cursor| {
            cursor
                .string()
                .ok_or_else(|| cursor.error("expected string"))
        })
    }

    pub fn list<T, F>(&mut self, f: F) -> Result<T>
    where
        F: FnOnce(&mut Self) -> Result<T>,
    {
        let (inner, after) = self
            .cursor
            .list()
            .ok_or_else(|| self.error("expected list"))?;
        self.cursor = inner;
        let result = f(self)?;

        if !self.cursor.is_empty() {
            return Err(inner.error("expected end of list"));
        }

        self.cursor = after;
        Ok(result)
    }

    pub fn seq<T, F>(&mut self, f: F) -> Result<T>
    where
        F: FnOnce(&mut Self) -> Result<T>,
    {
        let (inner, after) = self
            .cursor
            .seq()
            .ok_or_else(|| self.error("expected seq"))?;
        self.cursor = inner;
        let result = f(self)?;

        if !self.cursor.is_empty() {
            return Err(inner.error("expected end of seq"));
        }

        self.cursor = after;
        Ok(result)
    }

    pub fn map<T, F>(&mut self, f: F) -> Result<T>
    where
        F: FnOnce(&mut Self) -> Result<T>,
    {
        let (inner, after) = self
            .cursor
            .map()
            .ok_or_else(|| self.error("expected map"))?;
        self.cursor = inner;
        let result = f(self)?;

        if !self.cursor.is_empty() {
            return Err(inner.error("expected end of map"));
        }

        self.cursor = after;
        Ok(result)
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

    #[inline]
    pub fn context(&self) -> &C {
        &self.context
    }

    #[inline]
    pub fn context_mut(&mut self) -> &mut C {
        &mut self.context
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
    pub fn parse<C, T>(self, context: C) -> Result<(T, Self)>
    where
        T: Parse<C>,
    {
        let mut parser = Parser::new(self, context);
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

    fn advance(self, size: usize) -> Self {
        Self {
            buffer: self.buffer,
            index: self.index + size,
            end_index: self.end_index,
            parent: self.parent,
        }
    }

    pub fn symbol(self) -> Option<(&'a SmolStr, Self)> {
        match self.get()? {
            Token::Symbol(atom) => Some((atom, self.advance(1))),
            _ => None,
        }
    }

    pub fn int(self) -> Option<(i64, Self)> {
        match self.get()? {
            Token::Int(int) => Some((*int, self.advance(1))),
            _ => None,
        }
    }

    pub fn string(self) -> Option<(&'a SmolStr, Self)> {
        match self.get()? {
            Token::String(string) => Some((string, self.advance(1))),
            _ => None,
        }
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

    pub fn peek_symbol(self, f: impl FnOnce(&SmolStr) -> bool) -> bool {
        match self.symbol() {
            Some((symbol, _)) => f(symbol),
            None => false,
        }
    }

    pub fn peek_string(self, f: impl FnOnce(&SmolStr) -> bool) -> bool {
        match self.string() {
            Some((string, _)) => f(string),
            None => false,
        }
    }

    pub fn peek_int(self, f: impl FnOnce(i64) -> bool) -> bool {
        match self.int() {
            Some((int, _)) => f(int),
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
pub trait Parse<C = ()>: Sized {
    fn parse(parser: &mut Parser<'_, C>) -> Result<Self>;
}

impl<V: Parse<C>, C> Parse<C> for Vec<V> {
    fn parse(parser: &mut Parser<'_, C>) -> Result<Self> {
        let mut values = Vec::new();
        while !parser.is_empty() {
            values.push(parser.parse()?);
        }
        Ok(values)
    }
}

impl<V: Parse<C>, C> Parse<C> for Box<V> {
    fn parse(parser: &mut Parser<'_, C>) -> Result<Self> {
        Ok(Box::new(parser.parse()?))
    }
}

impl<V: Parse<C>, C> Parse<C> for Rc<V> {
    fn parse(parser: &mut Parser<'_, C>) -> Result<Self> {
        Ok(Rc::new(parser.parse()?))
    }
}

impl<V: Parse<C>, C> Parse<C> for Arc<V> {
    fn parse(parser: &mut Parser<'_, C>) -> Result<Self> {
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

// impl_parse_by_from_str!(SmolStr, String);
// impl_parse_by_from_str!(u8, u16, u32, u64, u128);
// impl_parse_by_from_str!(i8, i16, i32, i64, i128);
// impl_parse_by_from_str!(f32, f64);
// impl_parse_by_from_str!(bool);

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

// impl_peek_by_from_str!(SmolStr, String);
// impl_peek_by_from_str!(u8, u16, u32, u64, u128);
// impl_peek_by_from_str!(i8, i16, i32, i64, i128);
// impl_peek_by_from_str!(f32, f64);
// impl_peek_by_from_str!(bool);

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
    from_str_with_ctx(source, ())
}

/// Parse a `T` from an s-expression string, in the given context.
pub fn from_str_with_ctx<T: Parse<C>, C>(source: &str, context: C) -> Result<T> {
    let buffer = ParseBuffer::new(source)?;
    let mut parser = buffer.parser(context);
    T::parse(&mut parser)
}

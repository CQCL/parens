//! Parse values from s-expressions.
//!
//! An s-expression string is first turned into a tree of tokens in a [`ParseBuffer`],
//! checking for balanced parentheses and quoting. This tree can then be parsed into
//! a value of any type that implements the [`Parse`] trait by using a [`Parser`].
//!
//! Parsers can be given a context that is passed down to all parse functions.
//! This can be used to parse values whose text representation may only be determined given
//! additional context, for instance when using string interning.
use crate::lexer::lex;
use delegate::delegate;
use smol_str::SmolStr;
use std::fmt::Display;
use std::ops::Range;
use std::rc::Rc;
use std::sync::Arc;

/// The tokens that make up a [`ParseBuffer`].
/// The tokens are a tree representation of an s-expression string,
/// stored as a flat array for efficiency. The `List`, `Seq` and `Map` tokens
/// carry the offset of the first token after the end of the list, which
/// is used to naviate the tree.
#[derive(Debug, Clone)]
pub(crate) enum Token {
    List(usize),
    Seq(usize),
    Map(usize),
    Symbol(SmolStr),
    String(SmolStr),
    Int(i64),
}

/// An s-expression string that has been turned into a tree of tokens.
#[derive(Debug, Clone)]
pub struct ParseBuffer<'a> {
    pub(crate) source: &'a str,
    pub(crate) tokens: Vec<Token>,
    pub(crate) spans: Vec<Span>,
}

impl<'a> ParseBuffer<'a> {
    /// Turns a string into a parse buffer.
    /// Returns an error if the string is not a valid s-expression.
    #[inline]
    pub fn new(str: &'a str) -> Result<Self> {
        lex(str).map_err(|err| ParseError::new(&err, err.span()))
    }

    /// Creates a cursor that steps through the top level tokens of the buffer.
    #[inline]
    pub fn cursor(&'a self) -> Cursor<'a> {
        Cursor {
            buffer: self,
            index: 0,
            end_index: self.tokens.len(),
            parent: None,
        }
    }

    /// Creates a parser that steps through the top level tokens of the buffer with a given context.
    #[inline]
    pub fn parser<C>(&'a self, context: C) -> Parser<'a, C> {
        Parser::new(self.cursor(), context)
    }
}

/// A parser that is stepping through a [`ParseBuffer`].
///
/// If a parse function fails, the parser will return an error. At that point, the parser
/// is in an undefined state and should not be used further. In particular, the parser should
/// not be used for backtracking.
///
/// The parser carries a context of type `C` that is passed down to all parse functions.
#[derive(Clone)]
pub struct Parser<'a, C> {
    cursor: Cursor<'a>,
    context: C,
}

impl<'a, C> Parser<'a, C> {
    /// Creates a new parser from a cursor and a context.
    #[inline]
    pub fn new(cursor: Cursor<'a>, context: C) -> Self {
        Self { cursor, context }
    }

    /// Attempts to parse a value of type `T`.
    #[inline]
    pub fn parse<T: Parse<C>>(&mut self) -> Result<T> {
        T::parse(self)
    }

    /// Peeks at the next tokens to determine if a value of type `T` should be parsed.
    /// A successful peek does not guarantee that a subsequent parse will succeed.
    #[inline]
    pub fn peek<T: Peek>(&self) -> bool {
        T::peek(self.cursor)
    }

    /// Parse a string.
    pub fn symbol(&mut self) -> Result<&'a SmolStr> {
        self.step(|cursor| {
            cursor
                .symbol()
                .ok_or_else(|| cursor.error("expected symbol"))
        })
    }

    /// Parse an integer.
    pub fn int(&mut self) -> Result<i64> {
        self.step(|cursor| cursor.int().ok_or_else(|| cursor.error("expected integer")))
    }

    /// Parse a string.
    pub fn string(&mut self) -> Result<&'a SmolStr> {
        self.step(|cursor| {
            cursor
                .string()
                .ok_or_else(|| cursor.error("expected string"))
        })
    }

    /// Parse a list, using the provided function to parse the list contents.
    ///
    /// The function is expected to parse the list contents until the end.
    /// If any unparsed tokens remain within the list, an error is returned.
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

    /// Parse a sequence, using the provided function to parse the list contents.
    ///
    /// The function is expected to parse the sequence contents until the end.
    /// If any unparsed tokens remain within the sequence, an error is returned.
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

    /// Parse a map, using the provided function to parse the list contents.
    ///
    /// The function is expected to parse the sequence contents until the end.
    /// If any unparsed tokens remain within the map, an error is returned.
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

    /// Parse a value using the provided function that explicitly returns the new cursor.
    #[inline]
    pub fn step<T, F>(&mut self, f: F) -> Result<T>
    where
        F: FnOnce(Cursor<'a>) -> Result<(T, Cursor<'a>)>,
    {
        let (result, cursor) = f(self.cursor)?;
        self.cursor = cursor;
        Ok(result)
    }

    /// The current cursor position.
    #[inline]
    pub fn cursor(&self) -> Cursor<'a> {
        self.cursor
    }

    /// Reference to the parser's context.
    #[inline]
    pub fn context(&self) -> &C {
        &self.context
    }

    /// Mutable reference to the parser's context.
    #[inline]
    pub fn context_mut(&mut self) -> &mut C {
        &mut self.context
    }

    delegate! {
        to self.cursor {
            /// Returns whether there are no remaining tokens.
            pub fn is_empty(&self) -> bool;
            pub fn span(&self) -> Span;
            pub fn parent_span(&self) -> Span;
            /// Creates a [`ParseError`] with the current span.
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
    /// Parse a value of type `T` from the cursor's position.
    pub fn parse<C, T>(self, context: C) -> Result<(T, Self)>
    where
        T: Parse<C>,
    {
        let mut parser = Parser::new(self, context);
        let result = parser.parse()?;
        let cursor = parser.cursor();
        Ok((result, cursor))
    }

    /// Peeks at the next tokens to determine if a value of type `T` should be parsed.
    #[inline]
    pub fn peek<T: Peek>(self) -> bool {
        T::peek(self)
    }

    /// Creates a [`ParseError`] with the current span.
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

    /// Returns the current token if it is a symbol, together with a cursor advanced beyond it.
    pub fn symbol(self) -> Option<(&'a SmolStr, Self)> {
        match self.get()? {
            Token::Symbol(atom) => Some((atom, self.advance(1))),
            _ => None,
        }
    }

    /// Returns the current token if it is an integer, together with a cursor advanced beyond it.
    pub fn int(self) -> Option<(i64, Self)> {
        match self.get()? {
            Token::Int(int) => Some((*int, self.advance(1))),
            _ => None,
        }
    }

    /// Returns the current token if it is a string, together with a cursor advanced beyond it.
    pub fn string(self) -> Option<(&'a SmolStr, Self)> {
        match self.get()? {
            Token::String(string) => Some((string, self.advance(1))),
            _ => None,
        }
    }

    /// If the current token is a list, returns a cursor into the list's contents
    /// and a cursor advanced beyond the list.
    pub fn list(self) -> Option<(Self, Self)> {
        match self.get()? {
            Token::List(size) => Some(self.split(1, *size)),
            _ => None,
        }
    }

    /// If the current token is a sequence, returns a cursor into the sequence's contents
    /// and a cursor advanced beyond the sequence.
    pub fn seq(self) -> Option<(Self, Self)> {
        match self.get()? {
            Token::Seq(size) => Some(self.split(1, *size)),
            _ => None,
        }
    }

    /// If the current token is a map, returns a cursor into the map's contents
    /// and a cursor advanced beyond the map.
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

    /// Checks whether the current token is a symbol that satisfies the provided predicate.
    pub fn peek_symbol(self, f: impl FnOnce(&SmolStr) -> bool) -> bool {
        match self.symbol() {
            Some((symbol, _)) => f(symbol),
            None => false,
        }
    }

    /// Checks whether the current token is an integer that satisfies the provided predicate.
    pub fn peek_string(self, f: impl FnOnce(&SmolStr) -> bool) -> bool {
        match self.string() {
            Some((string, _)) => f(string),
            None => false,
        }
    }

    /// Checks whether the current token is an integer that satisfies the provided predicate.
    pub fn peek_int(self, f: impl FnOnce(i64) -> bool) -> bool {
        match self.int() {
            Some((int, _)) => f(int),
            None => false,
        }
    }

    /// Checks whether the current token is a list that satisfies the provided predicate.
    pub fn peek_list(self, f: impl FnOnce(Self) -> bool) -> bool {
        match self.list() {
            Some((inner, _)) => f(inner),
            None => false,
        }
    }

    /// Checks whether the current token is a sequence that satisfies the provided predicate.
    pub fn peek_seq(self, f: impl FnOnce(Self) -> bool) -> bool {
        match self.seq() {
            Some((inner, _)) => f(inner),
            None => false,
        }
    }

    /// Checks whether the current token is a map that satisfies the provided predicate.
    pub fn peek_map(self, f: impl FnOnce(Self) -> bool) -> bool {
        match self.map() {
            Some((inner, _)) => f(inner),
            None => false,
        }
    }

    /// Returns whether there are no remaining tokens.
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
///
/// The type parameter `C` is the type of the context that is passed to the parser.
/// Some types may only be parsed in a specific context. If the type does not require
/// any context, we encourage the trait implementation to be polymorphic over `C`
/// to allow mixing types that require context with those that do not.
pub trait Parse<C>: Sized {
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

#[cfg(feature = "tinyvec")]
use tinyvec::{Array, TinyVec};

#[cfg(feature = "tinyvec")]
impl<V, C, const N: usize> Parse<C> for TinyVec<[V; N]>
where
    [V; N]: Array<Item = V>,
    V: Parse<C>,
{
    fn parse(parser: &mut Parser<'_, C>) -> Result<Self> {
        let mut values = Self::new();
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

/// Trait for types whose presence can be detected by peeking at the next tokens.
///
/// Peeking is used to decide which type to parse without relying on backtracking.
/// A successful peek does not guarantee that the type can be parsed successfully.
pub trait Peek: Sized {
    fn peek(cursor: Cursor<'_>) -> bool;
}

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

/// Parse a `T` from an s-expression string, in the given context.
pub fn from_str<T: Parse<C>, C>(source: &str, context: C) -> Result<T> {
    let buffer = ParseBuffer::new(source)?;
    let mut parser = buffer.parser(context);
    T::parse(&mut parser)
}

//! The lexer that is used to turn an s-expression string into a token tree.
use logos::Logos;

use crate::{
    escape::unescape,
    parser::{ParseBuffer, Span, Token},
};

#[derive(Debug, Clone, PartialEq, Logos)]
pub(crate) enum LexerToken {
    #[token("(")]
    OpenList,
    #[token("[")]
    OpenSeq,
    #[token("{")]
    OpenMap,
    #[token(")")]
    CloseList,
    #[token("]")]
    CloseSeq,
    #[token("}")]
    CloseMap,
    /// A symbol that is not quoted with `|`.
    /// The regex appears scary, but it's just a negation of the characters that are not allowed
    /// in a symbol together with rules that avoid confusion with numbers.
    #[regex(r#"[^ +\-0-9\t\n\f\(\)\[\]\{\}"\|\\;][^ \t\n\f\(\)\[\]\{\}"\|\\;]*"#)]
    #[regex(r#"[+\-][^ 0-9\t\n\f\(\)\[\]\{\}"\|\\;][^ \t\n\f\(\)\[\]\{\}"\|\\;]*"#)]
    #[token("+")]
    #[token("-")]
    VerbatimSymbol,
    /// A symbol that is quoted with `|`.
    #[regex(r#"\|([^|\\]|\\[|\\tnr]|\\u\{[a-fA-F0-9]+\})*\|"#)]
    EscapedSymbol,
    /// A string that is quoted with `"`.
    #[regex(r#""([^"\\]|\\[\\tnr"]|\\u\{[a-fA-F0-9]+\})*""#)]
    String,
    /// A 64-bit signed integer. It is a parse error if the integer is out of bounds.
    #[regex(r#"[+-]?([0-9]+)"#, |lex| lex.slice().parse().ok())]
    Int(i64),
    /// Whitespace, including comments.
    #[regex(r"([ \t\n\f]+|;[^\n]*\n)")]
    WhiteSpace,
}

/// Errors that can occur during lexing.
/// These errors are ultimately turned into a [`ParseError`] in the public API.
///
/// [`ParseError`]: `cate::parser::ParseError`
#[derive(Debug, thiserror::Error)]
pub enum LexError {
    #[error("unexpected end of file")]
    Eof(Span),
    #[error("unexpected {0}")]
    UnexpectedClose(char, Span),
    #[error("syntax error")]
    Syntax(Span),
}

impl LexError {
    pub fn span(&self) -> Span {
        match self {
            LexError::Eof(span) => span.clone(),
            LexError::UnexpectedClose(_, span) => span.clone(),
            LexError::Syntax(span) => span.clone(),
        }
    }
}

/// Lexes the input string into a token tree.
pub fn lex<'a>(str: &'a str) -> Result<ParseBuffer<'a>, LexError> {
    let mut lexer = LexerToken::lexer(str);

    // The tokens that are lexed.
    let mut tokens = Vec::new();

    // The spans of the tokens in the token list.
    let mut spans = Vec::new();

    // A stack of open tokens and their positions in the token list.
    // This is used to match open and close tokens.
    let mut open_stack: Vec<(usize, LexerToken)> = Vec::new();

    while let Some(token) = lexer.next() {
        let span = lexer.span();
        let token = token.map_err(|()| LexError::Syntax(span.clone()))?;

        match token {
            LexerToken::OpenList => {
                open_stack.push((tokens.len(), token));
                tokens.push(Token::List(usize::MAX));
                spans.push(span);
            }
            LexerToken::OpenSeq => {
                open_stack.push((tokens.len(), token));
                tokens.push(Token::Seq(usize::MAX));
                spans.push(span);
            }
            LexerToken::OpenMap => {
                open_stack.push((tokens.len(), token));
                tokens.push(Token::Map(usize::MAX));
                spans.push(span);
            }
            LexerToken::CloseList => {
                let Some((pos, LexerToken::OpenList)) = open_stack.pop() else {
                    return Err(LexError::UnexpectedClose(')', span.clone()));
                };

                tokens[pos] = Token::List(tokens.len() - pos - 1);
                spans[pos].end = span.end;
            }
            LexerToken::CloseSeq => {
                let Some((pos, LexerToken::OpenSeq)) = open_stack.pop() else {
                    return Err(LexError::UnexpectedClose(']', span.clone()));
                };

                tokens[pos] = Token::Seq(tokens.len() - pos - 1);
                spans[pos].end = span.end;
            }
            LexerToken::CloseMap => {
                let Some((pos, LexerToken::OpenMap)) = open_stack.pop() else {
                    return Err(LexError::UnexpectedClose('}', span.clone()));
                };

                tokens[pos] = Token::Map(tokens.len() - pos - 1);
                spans[pos].end = span.end;
            }
            LexerToken::VerbatimSymbol => {
                tokens.push(Token::Symbol(lexer.slice().into()));
                spans.push(span);
            }
            LexerToken::EscapedSymbol => {
                let inner = lexer.slice()[1..lexer.slice().len() - 1].into();
                let unescaped = unescape(inner).ok_or(LexError::Syntax(span.clone()))?;
                tokens.push(Token::Symbol(unescaped.into()));
                spans.push(span);
            }
            LexerToken::Int(int) => {
                tokens.push(Token::Int(int));
                spans.push(span);
            }
            LexerToken::String => {
                let inner = lexer.slice()[1..lexer.slice().len() - 1].into();
                let unescaped = unescape(inner).ok_or(LexError::Syntax(span.clone()))?;
                tokens.push(Token::String(unescaped.into()));
                spans.push(span);
            }
            LexerToken::WhiteSpace => {}
        }
    }

    if !open_stack.is_empty() {
        return Err(LexError::Eof(str.len()..str.len()));
    }

    Ok(ParseBuffer {
        source: str,
        tokens,
        spans,
    })
}

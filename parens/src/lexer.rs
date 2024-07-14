use logos::Logos;

use crate::{
    escape::unescape,
    parser::{ParseBuffer, Span, Token},
};

#[derive(Debug, Clone, PartialEq, Logos)]
#[logos(skip r"([ \t\n\f]+|;[^\n]\n)+")]
enum LexerToken {
    #[token("(")]
    OpenList,
    #[token(")")]
    CloseList,
    #[regex(r#"[^ \t\n\f\(\)\[\]\{\}"\\;]+"#)]
    BareAtom,
    #[regex(r#""([^"\\]|\\["\\tnr]|u\{[a-fA-F0-9]+\})*""#)]
    EscapedAtom,
}

#[derive(Debug, thiserror::Error)]
pub enum LexError {
    #[error("unexpected end of file")]
    Eof(Span),
    #[error("unexpected )")]
    UnexpectedClose(Span),
    #[error("syntax error")]
    Syntax(Span),
}

impl LexError {
    pub fn span(&self) -> Span {
        match self {
            LexError::Eof(span) => span.clone(),
            LexError::UnexpectedClose(span) => span.clone(),
            LexError::Syntax(span) => span.clone(),
        }
    }
}

pub fn lex<'a>(str: &'a str) -> Result<ParseBuffer<'a>, LexError> {
    let mut lexer = LexerToken::lexer(str);
    let mut tokens = Vec::new();
    let mut spans = Vec::new();
    let mut open_stack = Vec::new();

    while let Some(token) = lexer.next() {
        let span = lexer.span();
        let token = token.map_err(|()| LexError::Syntax(span.clone()))?;

        match token {
            LexerToken::OpenList => {
                open_stack.push(tokens.len());
                tokens.push(Token::List(usize::MAX));
                spans.push(span);
            }
            LexerToken::CloseList => {
                let Some(pos) = open_stack.pop() else {
                    return Err(LexError::UnexpectedClose(span.clone()));
                };

                tokens[pos] = Token::List(tokens.len() - pos - 1);
                spans[pos].end = span.end;
            }
            LexerToken::BareAtom => {
                tokens.push(Token::Atom(lexer.slice().into()));
                spans.push(span);
            }
            LexerToken::EscapedAtom => {
                let inner = lexer.slice()[1..lexer.slice().len() - 1].into();
                let unescaped = unescape(inner).ok_or(LexError::Syntax(span.clone()))?;
                tokens.push(Token::Atom(unescaped.into()));
                spans.push(span);
            }
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

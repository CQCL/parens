use logos::Logos;

use crate::lexer::LexerToken;

/// Lexer token for an escaped string or symbol.
#[derive(Debug, Clone, Logos)]
enum EscapedToken {
    #[token(r#"\n"#, |_| '\n')]
    #[token(r#"\r"#, |_| '\r')]
    #[token(r#"\t"#, |_| '\t')]
    #[token(r#"\""#, |_| '"')]
    #[token(r#"\|"#, |_| '|')]
    #[token(r#"\\"#, |_| '\\')]
    Escaped(char),

    #[regex(r#"\\u\{[a-fA-F0-9]+\}"#, |lex| parse_unicode(lex.slice()))]
    Unicode(char),

    #[regex(r#"[^\\]"#)]
    Literal,
}

/// Parses a unicode escape sequence of the form `\u{HEX}` where `HEX` is a
/// hexadecimal number representing a unicode codepoint.
fn parse_unicode(str: &str) -> Option<char> {
    // Skip the '\u{' prefix and '}' suffix
    let hex = str.get(3..str.len() - 1)?;
    let code = u32::from_str_radix(hex, 16).ok()?;
    char::from_u32(code)
}

/// Replaces escape sequences with their corresponding characters.
pub fn unescape(str: &str) -> Option<String> {
    let mut lexer = EscapedToken::lexer(str);
    let mut output = String::with_capacity(str.len());

    while let Some(token) = lexer.next() {
        let token = token.ok()?;

        match token {
            EscapedToken::Escaped(c) => output.push(c),
            EscapedToken::Unicode(c) => output.push(c),
            EscapedToken::Literal => output.push_str(lexer.slice()),
        }
    }

    Some(output)
}

pub fn escape_symbol(str: &str) -> String {
    // Check if the symbol needs to be escaped at all
    let mut lexer = LexerToken::lexer(str);
    if let [Some(Ok(LexerToken::BareSymbol)), None] = [lexer.next(), lexer.next()] {
        return str.to_string();
    }

    let mut output = String::with_capacity(str.len() + 2);
    output.push('|');

    for c in str.chars() {
        match c {
            '\n' => output.push_str(r#"\n"#),
            '\r' => output.push_str(r#"\r"#),
            '\t' => output.push_str(r#"\t"#),
            '|' => output.push_str(r#"\|"#),
            '\\' => output.push_str(r#"\\"#),
            c => output.push(c),
        }
    }

    output.push('|');
    output
}

pub fn escape_string(str: &str) -> String {
    let mut output = String::with_capacity(str.len() + 2);
    output.push('"');

    for c in str.chars() {
        match c {
            '\n' => output.push_str(r#"\n"#),
            '\r' => output.push_str(r#"\r"#),
            '\t' => output.push_str(r#"\t"#),
            '"' => output.push_str(r#"\""#),
            '\\' => output.push_str(r#"\\"#),
            c => output.push(c),
        }
    }

    output.push('"');
    output
}

#[cfg(test)]
mod test {
    use super::{escape_symbol, unescape};
    use rstest::rstest;

    #[rstest]
    #[case("string", "string")]
    #[case("\n", r#"|\n|"#)]
    #[case(r"\", r#"|\\|"#)]
    #[case("", r#"||"#)]
    #[case("hello world", r#"|hello world|"#)]
    #[case("hello ", r#"|hello |"#)]
    #[case(" world", r#"| world|"#)]
    #[case("[", r#"|[|"#)]
    fn test_escape_symbol(#[case] string: &str, #[case] expected: &str) {
        assert_eq!(expected, escape_symbol(string));
    }

    #[rstest]
    #[case(r#"\""#, r#"""#)]
    #[case(r"\|", "|")]
    #[case(r"\u{1F60A}", "\u{1F60A}")]
    fn test_unescape(#[case] escaped: &str, #[case] expected: &str) {
        assert_eq!(expected, unescape(escaped).unwrap());
    }
}

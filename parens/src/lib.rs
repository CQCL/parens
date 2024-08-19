//! S-expressions as a data format.
//!
//! # Syntax
//!
//! This crate implements s-expressions with the following syntax:
//!
//! - **Lists** are sequences of values, delimited on the outside by `(` and `)`
//!   and separated by whitespace.
//!
//! - **Seqs** are sequences of values, delimited on the outside by `[` and `]`
//!   and separated by whitespace.
//!
//! - **Maps** are sequences of values, delimited on the outside by `{` and `}`
//!   and separated by whitespace.
//!
//! - **Symbols** represent identifiers. Symbols can occur verbatim without delimiters
//!   or enclosed between `|` characters. The `|` characters are optional whenever the
//!   symbol satisfies the following rules:
//!     - It does not include any of the characters `(`, `)`, `[`, `]`, `{`, `}`, `|`,
//!       `\n`, `\r`, `\t`, `\` or the space ` `.
//!     - It does not start with a digit (`0` through `9`).
//!     - If it starts with a `+` or `-` and has more than one character, the second character is not a digit.
//!
//!   Within quoted symbols, the following escaping rules apply:
//!    - `\"` and `\\` are used to escape `"` and `\`.
//!    - `\n`, `\r` and `\t` stand for the newline, carriage return and tab characters.
//!    - `\u{HEX}` stands in for any unicode character where `HEX` is a UTF-8 codepoint in hexadecimal notation.
//!
//! - **Strings** are sequences of characters enclosed within double quotes `"`.
//!   Within strings, the same escaping rules as for symbols apply.
//!
//! - **Integers** are signed 64-bit integers, encoded in decimal notation with an optional sign.
//!
//! - **Comments** begin with a `;` and extend to the end of the line.
//!
//! [sexplib]: https://github.com/janestreet/sexplib

pub(crate) mod escape;
pub(crate) mod lexer;
pub mod parser;
pub mod printer;
pub mod util;

pub use parser::from_str;
pub use printer::{to_string, to_string_pretty};

//! S-expressions as a data format.
//!
//! # Syntax
//!
//! This crate implements a minimalist version of s-expressions,
//! as inspired by OCaml's [sexplib].
//! The syntax of the s-expressions is as follows:
//!
//! - **Lists** are sequences of values, delimited on the outside by `(` and `)`
//!   and separated by whitespace.
//!
//! - **Atoms** are strings, either appearing verbatim without delimiters or
//!   enclosed within double quotes. The double quotes are optional whenever
//!   the string does not include any of the characters `(`, `)`, `"`, `\n`,
//!   `\r`, `\t` or the space ` `. Within quoted strings, the following escaping rules apply:
//!
//!    - `\"` and `\\` are used to escape `"` and `\`.
//!    - `\n`, `\r` and `\t` stand for the newline, carriage return and tab characters.
//!    - `\u{HEX}` stands in for any unicode character where `HEX` is a UTF-8 codepoint in hexadecimal notation.
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

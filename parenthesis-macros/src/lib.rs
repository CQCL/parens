//! Procedural macros for converting from s-expressions to Rust types and back.
//! See the `parenthesis` crate for more details on s-expressions and on how
//! to use the derive macros.
use syn::{parse_macro_input, DeriveInput};

pub(crate) mod common;
mod from_parens;
mod to_parens;

/// Derive the [`FromParens`] trait.
#[proc_macro_derive(FromParens, attributes(sexpr))]
pub fn derive_input(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let derive_input = parse_macro_input!(input as DeriveInput);
    from_parens::derive_from_parens_impl(derive_input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

/// Derive the [`ToParens`] trait.
#[proc_macro_derive(ToParens, attributes(sexpr))]
pub fn derive_export(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let derive_input = parse_macro_input!(input as DeriveInput);
    to_parens::derive_to_parens_impl(derive_input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

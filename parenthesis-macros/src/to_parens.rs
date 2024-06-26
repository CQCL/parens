use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse_quote, GenericParam};
use syn::{spanned::Spanned, DataStruct, DeriveInput};

use crate::common::{parse_sexpr_attributes, FieldKind};

pub fn derive_to_parens_impl(derive_input: DeriveInput) -> syn::Result<TokenStream> {
    match &derive_input.data {
        syn::Data::Struct(data_struct) => derive_to_parens_struct(&derive_input, data_struct),
        syn::Data::Enum(_) => Err(syn::Error::new(
            derive_input.span(),
            "Can not derive ToParens for enums.",
        )),
        syn::Data::Union(_) => Err(syn::Error::new(
            derive_input.span(),
            "Can not derive ToParens for unions.",
        )),
    }
}

fn derive_to_parens_struct(
    derive_input: &DeriveInput,
    data_struct: &DataStruct,
) -> syn::Result<TokenStream> {
    let struct_ident = &derive_input.ident;

    let mut code_fields = Vec::new();

    for field in &data_struct.fields {
        let Some(field_ident) = &field.ident else {
            // TODO: Derive Export for tuple structs
            return Err(syn::Error::new_spanned(
                field,
                "Fields must be named to derive ToParens.",
            ));
        };

        let field_data = parse_sexpr_attributes(&field.attrs)?;
        let field_name = field_data
            .rename
            .unwrap_or_else(|| format!("{}", field_ident.to_token_stream()));

        match field_data.kind {
            FieldKind::Positional => {
                code_fields.push(quote! {
                    <_ as ::parenthesis::to_parens::ToParens<__O>>::to_parens(&self.#field_ident, output)?;
                });
            }
            FieldKind::NamedRequired => {
                code_fields.push(quote! {
                    output.list(|output| {
                        output.symbol(#field_name)?;
                        <_ as ::parenthesis::to_parens::ToParens<__O>>::to_parens(&self.#field_ident, output)
                    })?;
                });
            }
            FieldKind::NamedOptional => {
                code_fields.push(quote! {
                    if let Some(field_value) = &self.#field_ident {
                        output.list(|output| {
                            output.symbol(#field_name)?;
                            <_ as ::parenthesis::to_parens::ToParens<__O>>::to_parens(field_value, output)
                        })?;
                    }
                });
            }
            FieldKind::NamedRepeated => {
                code_fields.push(quote! {
                    for field_value in self.#field_ident.iter() {
                        output.list(|output| {
                            output.symbol(#field_name)?;
                            <_ as ::parenthesis::to_parens::ToParens<__O>>::to_parens(field_value, output)
                        })?;
                    }
                });
            }
        }
    }

    // Add an `Output` bound to every generic type argument and add an `__O`
    // type argument for the output stream.
    let mut modified_generics = derive_input.generics.clone();
    let where_clause = modified_generics.make_where_clause();

    for param in &derive_input.generics.params {
        if let GenericParam::Type(param) = param {
            let ident = &param.ident;
            where_clause
                .predicates
                .push(parse_quote!(#ident: ::parenthesis::to_parens::ToParens<__O>));
        }
    }

    let stream_param: GenericParam = parse_quote!(__O: ::parenthesis::to_parens::OutputStream);
    modified_generics.params.push(stream_param);
    let (impl_generics, _, where_clause) = modified_generics.split_for_impl();
    let (_, ty_generics, _) = derive_input.generics.split_for_impl();

    Ok(quote! {
        impl #impl_generics ::parenthesis::to_parens::ToParens<__O> for #struct_ident #ty_generics
        where #where_clause {
            fn to_parens(&self, output: &mut __O) -> std::result::Result<(), __O::Error> {
                #(#code_fields)*
                Ok(())
            }
        }
    })
}

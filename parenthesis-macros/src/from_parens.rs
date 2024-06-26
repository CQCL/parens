use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse_quote, spanned::Spanned, DataStruct, DeriveInput, GenericParam};

use crate::common::{parse_sexpr_attributes, FieldKind};

pub fn derive_from_parens_impl(derive_input: DeriveInput) -> syn::Result<TokenStream> {
    match &derive_input.data {
        syn::Data::Struct(data_struct) => derive_from_parens_struct(&derive_input, data_struct),
        syn::Data::Enum(_) => Err(syn::Error::new(
            derive_input.span(),
            "Can not derive FromParens for enums.",
        )),
        syn::Data::Union(_) => Err(syn::Error::new(
            derive_input.span(),
            "Can not derive FromParens for unions.",
        )),
    }
}

fn derive_from_parens_struct(
    derive_input: &DeriveInput,
    data_struct: &DataStruct,
) -> syn::Result<TokenStream> {
    let struct_ident = &derive_input.ident;

    // The code used to parse positional fields
    let mut code_positional = Vec::new();

    // The setup code for named fields
    let mut code_field_setup = Vec::new();

    // The code that checks if required fields have been set
    let mut code_field_required = Vec::new();

    // The match branch for a named field
    let mut code_named_match = Vec::new();

    let mut constr_fields = Vec::new();

    // Whether we have seen a named field so far.
    // This is used to guarantee that positional fields must come before named ones.
    let mut seen_named = false;

    for field in &data_struct.fields {
        let Some(field_ident) = &field.ident else {
            return Err(syn::Error::new_spanned(
                field,
                "Fields must be named to derive Input.",
            ));
        };

        let field_data = parse_sexpr_attributes(&field.attrs)?;

        let field_name = field_data
            .rename
            .unwrap_or_else(|| format!("{}", field_ident.to_token_stream()));

        let field_ident_var = syn::Ident::new(
            &format!("var_{}", field_ident.to_token_stream()),
            field_ident.span(),
        );

        match field_data.kind {
            FieldKind::Positional => {
                if seen_named {
                    return Err(syn::Error::new_spanned(
                        field,
                        "Positional fields must come before named fields.",
                    ));
                }

                code_positional.push(quote! {
                    let #field_ident_var = <_ as ::parenthesis::from_parens::FromParens<__I>>::from_parens(stream)?;
                });
            }
            FieldKind::NamedRequired => {
                seen_named = true;

                code_field_setup.push(quote! {
                    let mut #field_ident_var = None;
                });

                let missing_field_message = format!("Missing required field `{}`.", field_name);

                code_field_required.push(quote! {
                    let Some(#field_ident_var) = #field_ident_var else {
                        return Err(::parenthesis::from_parens::ParseError::new(
                            #missing_field_message,
                            stream.parent_span()
                        ));
                    };
                });

                let duplicate_field_message = format!("Duplicate field `{}`.", field_name);

                code_named_match.push(quote! {
                    #field_name => {
                        if #field_ident_var.is_some() {
                            return Err(::parenthesis::from_parens::ParseError::new(
                                #duplicate_field_message,
                                inner_stream.parent_span()
                            ));
                        }

                        let value = <_ as ::parenthesis::from_parens::FromParens<__I>>::from_parens(&mut inner_stream)?;
                        #field_ident_var = Some(value);
                    },
                });
            }
            FieldKind::NamedOptional => {
                seen_named = true;

                code_field_setup.push(quote! {
                    let mut #field_ident_var = None;
                });

                let duplicate_field_message = format!("Duplicate field `{}`.", field_name);

                code_named_match.push(quote! {
                    #field_name => {
                        if #field_ident_var.is_some() {
                            return Err(::parenthesis::from_parens::ParseError::new(
                                #duplicate_field_message,
                                inner_stream.parent_span()
                            ));
                        }

                        let value = <_ as ::parenthesis::from_parens::FromParens<__I>>::from_parens(&mut inner_stream)?;
                        #field_ident_var = Some(value);
                    }
                });
            }
            FieldKind::NamedRepeated => {
                seen_named = true;

                code_field_setup.push(quote! {
                    let mut #field_ident_var = Vec::new();
                });

                code_named_match.push(quote! {
                    #field_name => {
                        let value = <_ as ::parenthesis::from_parens::FromParens<__I>>::from_parens(&mut inner_stream)?;
                        #field_ident_var.push(value);
                    }
                });
            }
        };

        constr_fields.push(quote! {
            #field_ident: #field_ident_var
        });
    }

    let code_named_match: TokenStream = code_named_match.into_iter().collect();
    let code_named = quote! {
        while let Some(token_tree) = stream.next() {
            let ::parenthesis::from_parens::TokenTree::List(mut inner_stream) = token_tree else {
                return Err(::parenthesis::from_parens::ParseError::new(
                    "expected field",
                    stream.span()
                ));
            };

            let Some(::parenthesis::from_parens::TokenTree::Symbol(head)) = inner_stream.next() else {
                return Err(::parenthesis::from_parens::ParseError::new(
                    "expected field name",
                    inner_stream.parent_span()
                ));
            };

            match head.as_ref() {
                #code_named_match
                unknown_name => {
                    return Err(::parenthesis::from_parens::ParseError::new(
                        format!("unknown field `{}`", unknown_name),
                        inner_stream.parent_span()
                    ));
                }
            };
        }
    };

    // Add an `Input` bound to every generic type argument and add an `__I`
    // type argument for the input stream.
    let mut modified_generics = derive_input.generics.clone();
    let where_clause = modified_generics.make_where_clause();

    for param in &derive_input.generics.params {
        if let GenericParam::Type(param) = param {
            let ident = &param.ident;
            where_clause
                .predicates
                .push(parse_quote!(#ident: ::parenthesis::from_parens::FromParens<__I>));
        }
    }

    let stream_param: GenericParam = parse_quote!(__I: ::parenthesis::from_parens::InputStream);
    modified_generics.params.push(stream_param);
    let (impl_generics, _, where_clause) = modified_generics.split_for_impl();
    let (_, ty_generics, _) = derive_input.generics.split_for_impl();

    Ok(quote! {
        #[automatically_derived]
        impl #impl_generics ::parenthesis::from_parens::FromParens<__I> for #struct_ident #ty_generics
        where #where_clause {
            fn from_parens(stream: &mut __I) -> ::std::result::Result<Self, ::parenthesis::from_parens::ParseError<__I::Span>>
            where
                Self: Sized {
                #(#code_positional)*
                #(#code_field_setup)*
                #code_named
                #(#code_field_required)*
                Ok(Self {
                    #(#constr_fields),*
                })
            }
        }
    })
}

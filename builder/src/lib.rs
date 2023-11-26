use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned, DeriveInput, Ident, Result};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match generate_builder(&input) {
        Ok(r) => r.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn generate_builder(input: &DeriveInput) -> Result<proc_macro2::TokenStream> {
    let struct_ident = &input.ident;
    let builder_ident = get_builder_struct_ident(input);

    let builder_struct = generate_builder_struct(input)?;

    let ret = quote! {
        impl #struct_ident {
            pub fn builder() -> #builder_ident {
                #builder_ident::new()
            }
        }

        #builder_struct
    };

    Ok(ret)
}

fn generate_builder_struct(input: &DeriveInput) -> Result<proc_macro2::TokenStream> {
    let builder_ident = get_builder_struct_ident(input);

    // 判断只能使用在 struct 上
    let fields = match input.data {
        syn::Data::Struct(ref data) => match data.fields {
            syn::Fields::Named(ref fields) => fields,
            _ => return Err(syn::Error::new(input.span(), "Only use on name fields")),
        },
        _ => return Err(syn::Error::new(input.span(), "Only use on struct")),
    };

    let mut fields_ident = Vec::new();
    let mut fields_ty = Vec::new();

    for field in fields.named.iter() {
        fields_ident.push(field.ident.as_ref().unwrap());
        fields_ty.push(&field.ty);
    }

    let ret = quote! {
        pub struct #builder_ident {
            #(#fields_ident: core::option::Option<#fields_ty>),*
        }

        impl #builder_ident {
            pub fn new() -> #builder_ident {
                #builder_ident {
                    #(#fields_ident: core::option::Option::None),*
                }
            }
        }
    };

    Ok(ret)
}

fn get_builder_struct_ident(input: &DeriveInput) -> Ident {
    let struct_name = input.ident.to_string();
    let builder_struct = format!("{}Builder", struct_name);
    Ident::new(&builder_struct, input.span())
}

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned, DeriveInput, Ident, Result, Type};

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

// 生成 builder struct 及对应的方法
fn generate_builder_struct(input: &DeriveInput) -> Result<proc_macro2::TokenStream> {
    // builder struct 名称
    let builder_ident = get_builder_struct_ident(input);

    // 判断只能使用在 struct 上
    let fields = match input.data {
        syn::Data::Struct(ref data) => match data.fields {
            syn::Fields::Named(ref fields) => fields,
            _ => return Err(syn::Error::new(input.span(), "Only use on name fields")),
        },
        _ => return Err(syn::Error::new(input.span(), "Only use on struct")),
    };

    // struct 字段名
    let mut fields_ident = Vec::new();
    // struct 字段类型
    let mut fields_ty = Vec::new();

    for field in fields.named.iter() {
        fields_ident.push(field.ident.as_ref().unwrap());
        fields_ty.push(&field.ty);
    }

    let setter_fns = generate_setter_fn(&fields_ident, &fields_ty)?;
    let builder_fn = validate_all_field(input, &fields_ident)?;

    let ret = quote! {
        pub struct #builder_ident {
            #(#fields_ident: std::option::Option<#fields_ty>),*
        }

        impl #builder_ident {
            pub fn new() -> #builder_ident {
                #builder_ident {
                    #(#fields_ident: std::option::Option::None),*
                }
            }

            #(#setter_fns)*

            #builder_fn
        }
    };

    Ok(ret)
}

// 生成 setter 方法
fn generate_setter_fn<'a>(
    fields_ident: &'a [&Ident],
    fields_ty: &'a [&Type],
) -> Result<Vec<proc_macro2::TokenStream>> {
    let mut setter_fns = Vec::new();
    for (ident, ty) in fields_ident.iter().zip(fields_ty) {
        let setter_fn = quote! {
            pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = std::option::Option::Some(#ident);
                self
            }
        };

        setter_fns.push(setter_fn);
    }

    Ok(setter_fns)
}

// 检测是否所有的字赋值
fn validate_all_field(
    input: &DeriveInput,
    fields_ident: &[&Ident],
) -> Result<proc_macro2::TokenStream> {
    let struct_ident = &input.ident;

    let mut validations = Vec::new();

    for ident in fields_ident.iter() {
        let ident_error_msg = format!("{} show be set", ident);
        validations.push(quote! {
            if self.#ident.is_none() {
                return std::result::Result::Err(std::boxed::Box::<dyn std::error::Error>::from(#ident_error_msg));
            }
        });
    }

    let ret = quote! {
        pub fn build(&mut self) -> Result<#struct_ident, Box<dyn std::error::Error>> {
            #(#validations)*

            let ret = #struct_ident {
                #(#fields_ident: self.#fields_ident.clone().unwrap()),*
            };

            std::result::Result::Ok(ret)
        }
    };

    Ok(ret)
}

// 获取 builder 的名称
fn get_builder_struct_ident(input: &DeriveInput) -> Ident {
    let struct_name = input.ident.to_string();
    let builder_struct = format!("{}Builder", struct_name);
    Ident::new(&builder_struct, input.span())
}

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, spanned::Spanned, AngleBracketedGenericArguments, DeriveInput,
    GenericArgument, Ident, Path, PathArguments, Result, Type, TypePath,
};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match generate_builder(&input) {
        Ok(r) => r.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

// struct 字段说明
struct InnerBuilderField<'a> {
    // 字段类型
    ty: &'a syn::Type,
    // 字段名
    ident: &'a syn::Ident,
    // 是否为 option
    is_optional: bool,
}

// 生成自定义的 builder
fn generate_builder(input: &DeriveInput) -> Result<proc_macro2::TokenStream> {
    let struct_ident = &input.ident;
    let builder_ident = get_builder_struct_ident(input);

    let builder_struct = generate_builder_token(input)?;

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
fn generate_builder_token(input: &DeriveInput) -> Result<proc_macro2::TokenStream> {
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

    // struct 原始字段, builder struct 字段
    let (_fields_ident, builder_fields) = fill_builder_field(fields);

    let builder_struct = generate_builder_struct(&builder_ident, &builder_fields)?;
    let builder_struct_new_fn = generate_builder_struct_new_fn(&builder_ident, &builder_fields)?;
    let builder_setter_fns = generate_builder_setter_fn(&builder_fields)?;
    let builder_validation = validate_all_field(&input.ident, &builder_fields)?;

    // 组装 Builder struct 类型
    let ret = quote! {
        #builder_struct

        impl #builder_ident {
            #builder_struct_new_fn

            #(#builder_setter_fns)*

            #builder_validation
        }
    };

    Ok(ret)
}

/// builder struct 的 new 方法
fn generate_builder_struct_new_fn(
    builder_ident: &syn::Ident,
    builder_fields: &[InnerBuilderField],
) -> Result<proc_macro2::TokenStream> {
    let fields = builder_fields
        .iter()
        .map(|field| {
            let ident = field.ident;

            return quote! {
                #ident
            };
        })
        .collect::<Vec<_>>();

    Ok(quote! {
        pub fn new() -> #builder_ident {
                #builder_ident {
                    #(#fields: std::option::Option::None),*
                }
            }
    })
}

/// 生成 builder struct 的 token stream
fn generate_builder_struct(
    builder_ident: &syn::Ident,
    builder_fields: &[InnerBuilderField],
) -> Result<proc_macro2::TokenStream> {
    let fields = builder_fields
        .iter()
        .map(|field| {
            let ident = field.ident;
            let ty = field.ty;

            return quote! {
                #ident: std::option::Option<#ty>
            };
        })
        .collect::<Vec<_>>();

    Ok(quote! {
        pub struct #builder_ident {
            #(#fields),*
        }
    })
}

/// 填充 builder struct 字段
fn fill_builder_field<'a>(
    fields: &'a syn::FieldsNamed,
) -> (Vec<&'a Ident>, Vec<InnerBuilderField<'a>>) {
    let mut fields_ident = Vec::new();
    let mut builder_fields = Vec::new();

    for field in fields.named.iter() {
        let ident = field.ident.as_ref().unwrap();
        let type_ref = &field.ty;
        fields_ident.push(ident);

        // 如果字段为 option 类型, 那泛型
        let builder_field = match get_optional_type(&field.ty) {
            Some(inner_type) => InnerBuilderField {
                ty: inner_type,
                ident,
                is_optional: true,
            },
            None => InnerBuilderField {
                ty: type_ref,
                ident,
                is_optional: false,
            },
        };

        builder_fields.push(builder_field);
    }

    (fields_ident, builder_fields)
}

// 生成 builder struct 的 setter 方法
fn generate_builder_setter_fn(
    builder_fields: &[InnerBuilderField],
) -> Result<Vec<proc_macro2::TokenStream>> {
    let mut setter_fns = Vec::new();
    for field in builder_fields.iter() {
        let ident = field.ident;
        let ty = field.ty;

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

// 检测 builder struct 里需要赋值字段是赋值
fn validate_all_field(
    struct_ident: &Ident,
    inner_fields: &[InnerBuilderField],
) -> Result<proc_macro2::TokenStream> {
    let mut builder_struct_ret = Vec::new();
    let mut validations = Vec::new();

    for field in inner_fields.iter() {
        let builder_field_ident = field.ident;
        let is_optional = field.is_optional;

        let ident_error_msg = format!("{} show be set", builder_field_ident);
        validations.push(quote! {
            if !#is_optional && self.#builder_field_ident.is_none() {
                return std::result::Result::Err(std::boxed::Box::<dyn std::error::Error>::from(#ident_error_msg));
            }
        });

        let struct_field_ret = if is_optional {
            quote! { #builder_field_ident: self.#builder_field_ident.clone(),}
        } else {
            quote! { #builder_field_ident: self.#builder_field_ident.clone().unwrap(),}
        };

        builder_struct_ret.push(struct_field_ret);
    }

    let ret = quote! {
        pub fn build(&mut self) -> Result<#struct_ident, Box<dyn std::error::Error>> {
            #(#validations)*

            let ret = #struct_ident {
                #(#builder_struct_ret)*
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

/// 查看当前字段是否为 Option 字段,
fn get_optional_type(ty: &syn::Type) -> Option<&syn::Type> {
    let mut ret = None;

    if let Type::Path(TypePath {
        path: Path { segments, .. },
        ..
    }) = ty
    {
        if let Some(s) = segments.last() {
            if s.ident.to_string() == "Option" {
                if let PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                    ref args,
                    ..
                }) = s.arguments
                {
                    if let Some(GenericArgument::Type(ref inner_type)) = args.first() {
                        ret = Some(inner_type)
                    }
                }
            }
        }
    }

    ret
}

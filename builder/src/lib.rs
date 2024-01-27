use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, spanned::Spanned, AngleBracketedGenericArguments, DeriveInput, Expr,
    GenericArgument, Ident, Lit, PathArguments, Result, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match generate_builder(&input) {
        Ok(r) => r.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

// struct 字段说明
struct InnerBuilderField<'a> {
    /// 字段类型
    ty: &'a Type,
    /// 字段名
    ident: &'a Ident,
    /// 是否为 option
    optional: bool,
    /// 字段 vec 的自定义名称和里面类型
    vec_ident: Option<(String, &'a Type)>,
}

impl<'a> InnerBuilderField<'a> {
    pub fn new(ty: &'a Type, ident: &'a Ident, optional: bool) -> Self {
        Self {
            ty,
            ident,
            optional,
            vec_ident: None,
        }
    }
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

    //  builder struct 字段
    let builder_fields = fill_builder_field(fields)?;

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
    builder_ident: &Ident,
    builder_fields: &[InnerBuilderField],
) -> Result<proc_macro2::TokenStream> {
    let fields = builder_fields
        .iter()
        .map(|field| {
            let ident = field.ident;

            match field.vec_ident {
                Some(_) => {
                    quote! {
                        #ident: std::vec::Vec::new()
                    }
                }
                None => {
                    quote! {
                        #ident: std::option::Option::None
                    }
                }
            }
        })
        .collect::<Vec<_>>();

    Ok(quote! {
        pub fn new() -> #builder_ident {
                #builder_ident {
                    #(#fields),*
                }
            }
    })
}

/// 生成 builder struct 的 token stream
fn generate_builder_struct(
    builder_ident: &Ident,
    builder_fields: &[InnerBuilderField],
) -> Result<proc_macro2::TokenStream> {
    let fields = builder_fields
        .iter()
        .map(|field| {
            let ident = field.ident;
            let ty = field.ty;

            match field.vec_ident {
                Some((_, ref vec_inner_type)) => {
                    quote! {
                        #ident: std::vec::Vec<#vec_inner_type>
                    }
                }
                None => {
                    quote! {
                            #ident: std::option::Option<#ty>
                    }
                }
            }
        })
        .collect::<Vec<_>>();

    Ok(quote! {
        pub struct #builder_ident {
            #(#fields),*
        }
    })
}

/// 填充 builder struct 字段
fn fill_builder_field<'a>(fields: &'a syn::FieldsNamed) -> syn::Result<Vec<InnerBuilderField<'a>>> {
    let mut fields_ident = Vec::new();
    let mut builder_fields = Vec::new();

    for field in fields.named.iter() {
        let ident = field.ident.as_ref().unwrap();
        let type_ref = &field.ty;
        fields_ident.push(ident);

        // 如果字段为 option 类型, 那取里面的泛型
        let mut builder_field = match check_optional_type(&field.ty) {
            Some(inner_type) => InnerBuilderField::new(inner_type, ident, true),
            None => InnerBuilderField::new(type_ref, ident, false),
        };

        // 查看字段是否为 vec 类型和有 builder 注解
        builder_field.vec_ident = check_vec_type(field)?;
        builder_fields.push(builder_field);
    }

    Ok(builder_fields)
}

// 生成 builder struct 的 setter 方法
fn generate_builder_setter_fn(
    builder_fields: &[InnerBuilderField],
) -> Result<Vec<proc_macro2::TokenStream>> {
    let mut setter_fns = Vec::new();
    for field in builder_fields.iter() {
        let ident = field.ident;
        let ty = field.ty;

        // 如果字段为 vec 并且有 builder 注释
        let setter_fn = match field.vec_ident {
            Some((ref each_name, vec_inner_type)) => {
                let vec_each_ident = Ident::new(&each_name, ident.span());

                quote! {
                    pub fn #vec_each_ident(&mut self, #vec_each_ident: #vec_inner_type) -> &mut Self {
                        self.#ident.push(#vec_each_ident);
                        self
                    }
                }
            }
            None => {
                quote! {
                    pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = std::option::Option::Some(#ident);
                        self
                    }
                }
            }
        };

        setter_fns.push(setter_fn);
    }

    Ok(setter_fns)
}

/// 检测 builder struct 里需要赋值字段是已经被赋值了
fn validate_all_field(
    struct_ident: &Ident,
    inner_fields: &[InnerBuilderField],
) -> Result<proc_macro2::TokenStream> {
    let mut builder_struct_ret = Vec::new();
    let mut validations = Vec::new();

    for field in inner_fields.iter() {
        let builder_field_ident = field.ident;
        let is_optional = field.optional;

        let ident_error_msg = format!("{} show be set", builder_field_ident);

        if field.vec_ident.is_none() {
            validations.push(quote! {
                if !#is_optional && self.#builder_field_ident.is_none() {
                    return std::result::Result::Err(std::boxed::Box::<dyn std::error::Error>::from(#ident_error_msg));
                }
            });
        }

        let struct_field_ret = if is_optional || field.vec_ident.is_some() {
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

/// 获取 builder 的名称
fn get_builder_struct_ident(input: &DeriveInput) -> Ident {
    let struct_name = input.ident.to_string();
    let builder_struct = format!("{}Builder", struct_name);
    Ident::new(&builder_struct, input.span())
}

/// 查看当前字段是否为 Option 字段,
fn check_optional_type(ty: &Type) -> Option<&Type> {
    let mut ret = None;

    if let Type::Path(TypePath { path, .. }) = ty {
        if let Some(s) = path.segments.last() {
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

/// 查看当前字段是否为 vec 类型
fn check_vec_type(field: &syn::Field) -> syn::Result<Option<(String, &Type)>> {
    for attr in field.attrs.iter() {
        if attr.path().is_ident("builder") {
            if let Type::Path(TypePath { ref path, .. }) = field.ty {
                // 如果字段为 Vec 类型
                match path.segments.last() {
                    Some(s) if s.ident.to_string() == "Vec" => {
                        // 查看是否有 builder attribute
                        let kv = attr.parse_args::<syn::MetaNameValue>()?;
                        if kv.path.is_ident("each") {
                            if let Expr::Lit(syn::ExprLit {
                                lit: Lit::Str(ref lit_str),
                                ..
                            }) = kv.value
                            {
                                // 取出 vec 类型
                                if let PathArguments::AngleBracketed(
                                    AngleBracketedGenericArguments { ref args, .. },
                                ) = s.arguments
                                {
                                    if let Some(GenericArgument::Type(vec_type)) = args.first() {
                                        return Ok(Some((lit_str.value(), vec_type)));
                                    }
                                }
                            }
                        } else {
                            return Err(syn::Error::new_spanned(
                                &attr.meta,
                                r#"expected `builder(each = "...")`"#,
                            ));
                        }
                    }
                    _ => (),
                }
            }

            return Err(syn::Error::new(
                field.span(),
                "builder attribute only for vec type",
            ));
        }
    }

    Ok(None)
}

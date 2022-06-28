use proc_macro::TokenStream;
use syn::spanned::Spanned;
use quote::quote;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = syn::parse_macro_input!(input as syn::DeriveInput);
    match do_expand(&st) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

type StructFields = syn::punctuated::Punctuated<syn::Field, syn::Token![,]>;

fn get_fields_from_derive_input(st: &syn::DeriveInput) -> syn::Result<&StructFields> {
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed {
            ref named,
            .. }),
        .. }) = st.data {
        return Ok(named);
    }
    Err(syn::Error::new_spanned(st, "Must Define On Struct, Not Enum".to_string()))
}

fn generate_builder_struct_fields_def(fields: &StructFields) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    let quotes: syn::Result<Vec<_>> = fields.iter().map(|f| {
        let ident = f.ident.as_ref().unwrap();
        let ty = &f.ty;
        Ok(if let Some(inner_type) = get_generic_inner_type(ty, "Option") {
            quote! {
                #ident: std::option::Option<#inner_type>
            }
        } else if get_user_specified_ident_for_vec(&f)?.is_some() {
            quote! {
                #ident: #ty
            }
        } else {
            quote! {
                #ident: std::option::Option<#ty>
            }
        })
    }).collect();
    Ok(quotes?)
}

fn generate_builder_struct_factory_init_clauses(fields: &StructFields) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    let init_clauses: syn::Result<Vec<_>> = fields.iter().map(|f| {
        let ident = f.ident.as_ref().unwrap();
        Ok(if get_user_specified_ident_for_vec(f)?.is_some() {
            quote! {
                #ident: std::vec::Vec::new()
            }
        } else {
            quote! {
                #ident: std::option::Option::None
            }
        })

    })
        .collect();
    Ok(init_clauses?)
}

// test 3
fn generate_struct_fields_setters_def(fields: &StructFields) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    let idents: Vec<_> = fields.iter().map(|f| (f.ident.as_ref().unwrap(), &f.ty)).collect();
    let mut setters = vec![];
    for (idx, (ident, ty)) in idents.iter().enumerate() {
        if let Some(inner_type) = get_generic_inner_type(ty, "Option") {
            setters.push(quote! {
                fn #ident(&mut self, #ident: #inner_type) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            });
        } else if let Some(ref user_specified_ident) = get_user_specified_ident_for_vec(&fields[idx])? {
            let inner_type = get_generic_inner_type(ty, "Vec").ok_or(syn::Error::new(fields[idx].span(), "`each` field must be a Vec type"))?;
            setters.push(quote! {
                fn #user_specified_ident(&mut self, #user_specified_ident: #inner_type) -> &mut Self {
                    self.#ident.push(#user_specified_ident.clone());
                    self
                }
            });

            if user_specified_ident != *ident {
                setters.push(quote! {
                    fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = #ident.clone();
                        self
                    }
                });
            }
        } else {
            setters.push(quote! {
                fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            });
        };
    }
    Ok(setters)
}

// test 4 5
fn generate_build_function_for_builder(struct_ident: &syn::Ident, fields: &StructFields) -> syn::Result<proc_macro2::TokenStream> {
    let field_idents: Vec<_> = fields.iter().map(|f| (f.ident.as_ref().unwrap(), &f.ty)).collect();
    let mut result_fields = vec![];
    let mut err_handles = vec![];
    for (idx, (field_ident, ty)) in field_idents.iter().enumerate() {
        if get_generic_inner_type(ty, "Option").is_none()
        && get_user_specified_ident_for_vec(&fields[idx])?.is_none() {
            let err_handle = quote! {
                if self.#field_ident.is_none() {
                    return Err(format!("{} must be set", stringify!(#field_ident)).into());
                }
            };
            err_handles.push(err_handle);
        }
        let result_field = if get_user_specified_ident_for_vec(&fields[idx])?.is_some() {
            quote! {
                #field_ident: self.#field_ident.clone()
            }
        } else if get_generic_inner_type(ty, "Option").is_some() {
            quote! {
                #field_ident: self.#field_ident.clone()
            }
        } else {
            quote! {
                #field_ident: self.#field_ident.clone().unwrap()
            }
        };
        result_fields.push(result_field);
    }

    let ret = quote! {
        pub fn build(&mut self) -> std::result::Result<#struct_ident, std::boxed::Box<dyn std::error::Error>> {
            #(#err_handles)*
            Ok(#struct_ident {
                #(#result_fields),*
            })
        }
    };
    Ok(ret)
}

// test 6
fn get_generic_inner_type<'a>(ty: &'a syn::Type, outer_ident_name: &'a str) -> Option<&'a syn::Type> {
    if let syn::Type::Path(syn::TypePath{ path: syn::Path { segments, ..}, ..}) = ty {
        if segments.last().is_some() {
            let path_segment = segments.last().unwrap();
            if path_segment.ident == outer_ident_name {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments{ args, ..}) = &path_segment.arguments {
                    if args.first().is_some() {
                        if let Some(syn::GenericArgument::Type(inner_type)) = args.first() {
                            return Some(inner_type);
                        }
                    }
                }
            }
        }
    }
    None
}

// test 7
fn get_user_specified_ident_for_vec(field: &syn::Field) -> syn::Result<Option<syn::Ident>> {
    for attr in &field.attrs {
        if let Ok(syn::Meta::List(syn::MetaList{ ref path, ref nested, .. })) = attr.parse_meta() {
            if let Some(p) = path.segments.first() {
                if p.ident == "builder" {
                    if let Some(syn::NestedMeta::Meta(syn::Meta::NameValue(kv))) = nested.first() {
                        if kv.path.is_ident("each") {
                            if let syn::Lit::Str(ref ident_str) = kv.lit {
                                return Ok(Some(syn::Ident::new(ident_str.value().as_str(), attr.span())));
                            }
                        } else {
                            if let Ok(syn::Meta::List(list)) = attr.parse_meta() {
                                return Err(syn::Error::new_spanned(list, r#"expected `builder(each = "...")`"#));
                            }
                        }
                    }
                }
            }
        }
    }
    Ok(None)
}

fn do_expand(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    // eprintln!("{:#?}", st);
    let struct_name_literal = st.ident.to_string();
    let struct_ident = &st.ident;
    let builder_name_literal = struct_name_literal + "Builder";
    let builder_name_ident = syn::Ident::new(&builder_name_literal, st.span());

    let fields = get_fields_from_derive_input(st)?;
    let builder_struct_fields_def = generate_builder_struct_fields_def(fields)?;
    let builder_struct_factory_init_clauses = generate_builder_struct_factory_init_clauses(fields)?;
    let struct_fields_setters_def = generate_struct_fields_setters_def(fields)?;
    let build_function_for_builder = generate_build_function_for_builder(struct_ident, fields)?;

    let ret = quote!(
        pub struct #builder_name_ident {
            #(#builder_struct_fields_def),*
        }

        impl #builder_name_ident {
            #(#struct_fields_setters_def)*

            #build_function_for_builder
        }

        impl #struct_ident {
            pub fn builder() -> #builder_name_ident {
                #builder_name_ident {
                    #(#builder_struct_factory_init_clauses),*
                }
            }
        }
    );
    Ok(ret)
}

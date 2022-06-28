use proc_macro::TokenStream;
use std::collections::HashMap;
use quote::quote;
use syn::parse_quote;
use syn::visit::{self, Visit};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = syn::parse_macro_input!(input as syn::DeriveInput);
    match do_expand(&st) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

type StructFields = syn::punctuated::Punctuated<syn::Field, syn::Token![,]>;
fn get_fields_from_derive_input(st: &syn::DeriveInput) -> syn::Result<&StructFields> {
    if let syn::Data::Struct(syn::DataStruct {fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }), ..}) = st.data {
        return Ok(named)
    }
    Err(syn::Error::new_spanned(st, "Must Define On Struct, Not Enum".to_string()))
}

fn generate_debug_trait_core(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let fields = get_fields_from_derive_input(st)?;
    let struct_name_ident = &st.ident;
    let struct_name_literal = struct_name_ident.to_string();

    let mut fmt_body_stream = proc_macro2::TokenStream::new();
    fmt_body_stream.extend(quote!(
       fmt.debug_struct(#struct_name_literal)
    ));
    for field in fields {
        let field_name_ident = field.ident.as_ref().unwrap();
        let field_name_literal = field_name_ident.to_string();

        let mut fmt_str = "{:?}".to_string();
        if let Some(fmt) = get_custom_format_of_field(field)? {
            fmt_str = fmt;
        }
        fmt_body_stream.extend(quote! {
           .field(#field_name_literal, &format_args!(#fmt_str, self.#field_name_ident))
        });
    }
    fmt_body_stream.extend(quote! {
       .finish()
    });
    Ok(fmt_body_stream)
}

// test 2
fn generate_debug_trait(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let fmt_body_stream = generate_debug_trait_core(st)?;
    let struct_name_ident = &st.ident;

    let mut generics_param_to_modify = st.generics.clone();
    if let Some(hatch) = get_struct_escape_hatch(st) {
        generics_param_to_modify.make_where_clause();
        generics_param_to_modify.where_clause.as_mut().unwrap().predicates.push(syn::parse_str(hatch.as_str()).unwrap());
    } else {
        let fields = get_fields_from_derive_input(st)?;
        let mut field_type_names = vec![];
        let mut phantomdata_type_param_names = vec![];
        for field in fields {
            if let Some(s) = get_field_type_name(field)? {
                field_type_names.push(s);
            }
            if let Some(s) = get_phantomdata_generic_type_name(field)? {
                phantomdata_type_param_names.push(s);
            }
        }

        let associated_types_map = get_generic_associated_types(st);
        for g in generics_param_to_modify.params.iter_mut() {
            if let syn::GenericParam::Type(t) = g {
                let type_param_name = t.ident.to_string();
                if (phantomdata_type_param_names.contains(&type_param_name)
                    || associated_types_map.contains_key(&type_param_name))
                    && !field_type_names.contains(&type_param_name)
                {
                    continue;
                }
                t.bounds.push(parse_quote!(std::fmt::Debug));
            }
        }

        generics_param_to_modify.make_where_clause();
        for (_, associated_types) in associated_types_map {
            for associated_type in associated_types {
                generics_param_to_modify.where_clause.as_mut().unwrap().predicates.push(parse_quote!(
                #associated_type: std::fmt::Debug
            ));
            }
        }
    }

    let (impl_generics, type_generics, where_generics) = generics_param_to_modify.split_for_impl();

    let ret_stream = quote! {
        impl #impl_generics std::fmt::Debug for #struct_name_ident #type_generics #where_generics {
            fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                #fmt_body_stream
            }
        }
    };

    Ok(ret_stream)
}

// test 4
fn get_custom_format_of_field(field: &syn::Field) -> syn::Result<Option<String>> {
    for attr in &field.attrs {
        if let Ok(syn::Meta::NameValue(syn::MetaNameValue { ref path, ref lit, .. })) = attr.parse_meta() {
            if path.is_ident("debug") {
                if let syn::Lit::Str(ref ident_str) = lit {
                    return Ok(Some(ident_str.value()))
                }
            }
        }
    }
    Ok(None)
}

fn get_phantomdata_generic_type_name(field: &syn::Field) -> syn::Result<Option<String>> {
    if let syn::Type::Path(syn::TypePath{ path: syn::Path{ ref segments, ..}, ..}) = field.ty {
        if let Some(syn::PathSegment{ ref ident, ref arguments }) = segments.last() {
            if ident == "PhantomData" {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments{ args, .. }) = arguments {
                    if let Some(syn::GenericArgument::Type(syn::Type::Path(ref gp))) = args.first() {
                        if let Some(generic_ident) = gp.path.segments.first() {
                            return Ok(Some(generic_ident.ident.to_string()))
                        }
                    }
                }
            }
        }
    }
    Ok(None)
}

fn get_field_type_name(field: &syn::Field) -> syn::Result<Option<String>> {
    if let syn::Type::Path(syn::TypePath{ path: syn::Path{ ref segments, .. }, .. }) = field.ty {
        if let Some(syn::PathSegment{ ref ident, .. }) = segments.last() {
            return Ok(Some(ident.to_string()));
        }
    }
    Ok(None)
}

fn do_expand(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let mut ret = proc_macro2::TokenStream::new();
    ret.extend(generate_debug_trait(st)?);
    Ok(ret)
}

struct TypePathVisitor {
    generic_type_names: Vec<String>,
    associated_types: HashMap<String, Vec<syn::TypePath>>,
}

impl<'ast> Visit<'ast> for TypePathVisitor {
    fn visit_type_path(&mut self, node: &'ast syn::TypePath) {
        if node.path.segments.len() >= 2 {
            let generic_type_name = node.path.segments[0].ident.to_string();
            if self.generic_type_names.contains(&generic_type_name) {
                self.associated_types.entry(generic_type_name).or_insert(Vec::new()).push(node.clone());
            }
        }
        visit::visit_type_path(self, node);
    }
}

fn get_generic_associated_types(st: &syn::DeriveInput) -> HashMap<String, Vec<syn::TypePath>> {
    let origin_generic_param_names: Vec<String> = st.generics.params.iter().filter_map(|f| {
        if let syn::GenericParam::Type(t) = f {
            return Some(t.ident.to_string())
        }
        None
    })
        .collect();
    let mut visitor = TypePathVisitor {
        generic_type_names: origin_generic_param_names,
        associated_types: HashMap::new(),
    };

    visitor.visit_derive_input(st);
    visitor.associated_types
}

fn get_struct_escape_hatch(st: &syn::DeriveInput) -> Option<String> {
    if let Some(inert_attr) = st.attrs.last() {
        if let Ok(syn::Meta::List(syn::MetaList{ nested, .. })) = inert_attr.parse_meta() {
            if let Some(syn::NestedMeta::Meta(syn::Meta::NameValue(path_value))) = nested.last() {
                if path_value.path.is_ident("bound") {
                    if let syn::Lit::Str(ref lit) = path_value.lit {
                        return Some(lit.value());
                    }
                }
            }
        }
    }
    None
}

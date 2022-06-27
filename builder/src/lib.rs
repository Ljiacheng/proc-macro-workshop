use proc_macro::TokenStream;
use syn::spanned::Spanned;
use quote::quote;

#[proc_macro_derive(Builder)]
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

fn generate_builder_struct_fields_def(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let fields = get_fields_from_derive_input(st)?;
    let idents: Vec<_> = fields.iter().map(|f| f.ident.as_ref().unwrap()).collect();
    let types: Vec<_> = fields.iter().map(|f| &f.ty).collect();
    let ret = quote! {
        #( #idents: std::option::Option<#types>),*
    };
    Ok(ret)
}

fn generate_builder_struct_factory_init_clauses(st: &syn::DeriveInput) -> syn::Result<Vec<proc_macro2::TokenStream>> {
    let fields = get_fields_from_derive_input(st)?;
    let init_clauses: Vec<_> = fields.iter().map(|f| {
        let ident = f.ident.as_ref().unwrap();
        quote! {
            #ident: std::option::Option::None
        }
    })
        .collect();
    Ok(init_clauses)
}

fn do_expand(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    // eprintln!("{:#?}", st);
    let struct_name_literal = st.ident.to_string();
    let struct_ident = &st.ident;
    let builder_name_literal = struct_name_literal + "Builder";
    let builder_name_ident = syn::Ident::new(&builder_name_literal, st.span());

    let builder_struct_fields_def = generate_builder_struct_fields_def(st)?;
    let builder_struct_factory_init_clauses = generate_builder_struct_factory_init_clauses(st)?;

    let ret = quote!(
        pub struct #builder_name_ident {
            #builder_struct_fields_def
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

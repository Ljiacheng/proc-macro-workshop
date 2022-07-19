use proc_macro::TokenStream;
use quote::ToTokens;
use syn::ExprMatch;
use syn::spanned::Spanned;
use syn::visit_mut::VisitMut;

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let st = syn::parse_macro_input!(input as syn::Item);

    match do_expand(&st) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => {
            let mut t: proc_macro2::TokenStream = st.to_token_stream();
            t.extend(e.to_compile_error());
            t.into()
        },
    }
}

fn do_expand(st: &syn::Item) -> syn::Result<proc_macro2::TokenStream> {
    match st {
        syn::Item::Enum(e) => check_enum_order(e),
        _ => syn::Result::Err(syn::Error::new(proc_macro2::Span::call_site(), "expected enum or match expression")),
    }
}

fn check_enum_order(st: &syn::ItemEnum) -> syn::Result<proc_macro2::TokenStream> {
    let origin_order = st.variants.iter().map(|f| (f.ident.to_string(), f)).collect::<Vec<_>>();
    let mut sorted = origin_order.clone();
    sorted.sort_by(|a, b| a.0.cmp(&b.0));
    for (a, b) in origin_order.iter().zip(sorted) {
        if a.0 != b.0 {
            return syn::Result::Err(syn::Error::new_spanned(&b.1.ident, format!("{} should sort before {}", b.0, a.0)));
        }
    }

    Ok(st.to_token_stream())
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let mut st = syn::parse_macro_input!(input as syn::ItemFn);

    match do_match_expand(&mut st) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => {
            let mut t: proc_macro2::TokenStream = st.to_token_stream();
            t.extend(e.to_compile_error());
            t.into()
        },
    }
}

fn do_match_expand(st: &mut syn::ItemFn) -> syn::Result<proc_macro2::TokenStream> {
    let mut visitor = MatchVisitor{ err: None };
    visitor.visit_item_fn_mut(st);

    match visitor.err {
        Some(e) => syn::Result::Err(e),
        None => syn::Result::Ok(st.to_token_stream()),
    }
}

struct MatchVisitor{
    err: Option<syn::Error>
}

impl syn::visit_mut::VisitMut for MatchVisitor {
    fn visit_expr_match_mut(&mut self, i: &mut ExprMatch) {
        let mut target_idx: Option<usize> = None;
        for (idx, attr) in i.attrs.iter().enumerate() {
            if get_path_string(&attr.path) == "sorted" {
                target_idx = Some(idx);
            }
        }

        if target_idx.is_none() {
            syn::visit_mut::visit_expr_match_mut(self, i);
            return;
        }

        i.attrs.remove(target_idx.unwrap());
        let mut match_arm_names: Vec<(_, &dyn ToTokens)> = Vec::new();
        for arm in &i.arms {
            match &arm.pat {
                syn::Pat::Path(p) => match_arm_names.push((get_path_string(&p.path), &p.path)),
                syn::Pat::TupleStruct(p) => match_arm_names.push((get_path_string(&p.path), &p.path)),
                syn::Pat::Struct(p) => match_arm_names.push((get_path_string(&p.path), &p.path)),
                syn::Pat::Ident(i) => match_arm_names.push((i.ident.to_string(), &i.ident)),
                syn::Pat::Wild(w) => match_arm_names.push(("_".to_string(), &w.underscore_token)),
                _ => {
                    self.err = Some(syn::Error::new(arm.pat.span(), "unsupported by #[sorted]"));
                    return;
                }
            }
        }

        let mut sorted_names = match_arm_names.clone();
        sorted_names.sort_by(|a, b| a.0.cmp(&b.0));
        for (a, b) in match_arm_names.iter().zip(sorted_names) {
            if a.0 != b.0 {
                self.err = Some(syn::Error::new_spanned(b.1, format!("{} should sort before {}", b.0, a.0)));
                return;
            }
        }

        syn::visit_mut::visit_expr_match_mut(self, i);
        return;
    }
}

fn get_path_string(p: &syn::Path) -> String {
    let mut buf = vec![];
    for s in &p.segments {
        buf.push(s.ident.to_string());
    }
    return buf.join("::")
}

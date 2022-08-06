use proc_macro_error::abort;
use std::str::FromStr;
use proc_macro2::{Ident};
use syn::{Attribute, Token};
use syn::parse::Parse;

pub(crate) fn get_attr(s: &str, attrs: &mut Vec<Attribute>) -> Option<Attribute> {
    let mut index = 0;
    while index < attrs.len() {
        if let Some(ident) = attrs[index].path.get_ident() {
            if ident.to_string() == s {
                return Some(attrs.swap_remove(index))
            }
        }
        index += 1;
    }
    None
}

pub(crate) fn get_attr_equals_idents(s: &str, attrs: &mut Vec<Attribute>) -> Option<Vec<Ident>> {
    get_attr(s, attrs).map(|attr| {
        let tokens: Vec<_> = attr.tokens.clone().into_iter().collect();
        match tokens.get(0) {
            Some(proc_macro2::TokenTree::Punct(p)) if p.as_char() == '=' => {
                let mut idents = vec![];
                let mut expect_ident = true;
                for token in &tokens[1..] {
                    match token {
                        proc_macro2::TokenTree::Ident(ident) if expect_ident => {
                            expect_ident = false;
                            idents.push(ident.clone());
                        },
                        proc_macro2::TokenTree::Punct(p) if !expect_ident && p.as_char() == ',' => { expect_ident = true; }
                        _ => abort!(attr, "Expected a list of identifiers: #[{s} = Ident1, Ident2]")
                    }
                }
                idents
            }
            _ => abort!(attr, "Expected `= identifier` after attribute name"),
        }
    })
}

pub(crate) fn get_attr_equals_ident(s: &str, attrs: &mut Vec<Attribute>) -> Option<Ident> {
    get_attr_equals_idents(s, attrs).map(|mut vec| {
        if vec.len() > 1 {
            abort!(vec[1], "Expected only a single identifier");
        } else {
            vec.remove(0)
        }
    })
}

pub(crate) fn get_ast<T: FromStr + Parse>(variant: &mut syn::Variant, new_discriminant: Option<(Token![=], syn::Expr)>, type_message: &str) -> T
where <T as FromStr>::Err: std::fmt::Debug {
    let ast = match variant.discriminant.take() {
        Some((
            _,
            syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Str(lit_str),
                ..
            }),
        )) => {
            let text = lit_str.value();
            text.parse().unwrap()
        }
        Some((
            _,
            syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Char(lit_char),
                ..
            }),
        )) => {
            let text = lit_char.value().to_string();
            text.parse().unwrap()
        }
        Some((
            _,
            syn::Expr::Macro(syn::ExprMacro { mac, .. })
             )) if mac.path.segments.len() == 1 && mac.path.segments.first().unwrap().ident.to_string() == "p" => {
            syn::parse(mac.tokens.into()).unwrap()
        }
        Some((_, discriminant)) => {
            abort!(discriminant, type_message);
        }
        None => {
            abort!(variant, type_message);
        }
    };
    variant.discriminant = new_discriminant;
    ast
}

pub(crate) fn get_lexer_ident(meta: &syn::Meta) -> Option<syn::Ident> {
    match meta {
        syn::Meta::Path(path) => {
            match path.get_ident() {
                Some(i) => Some(i.clone()),
                None => None
            }
        }
        _ => None
    }
}

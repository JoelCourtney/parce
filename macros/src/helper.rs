use proc_macro_error::abort;
use std::str::FromStr;
use proc_macro2::{Ident, TokenTree};
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

pub(crate) fn get_attr_ident_argument(s: &str, attrs: &mut Vec<Attribute>) -> Option<Ident> {
    get_attr(s, attrs).map(|attr| {
        let mut tokens: Vec<TokenTree> = attr.tokens.clone().into_iter().collect();
        if tokens.len() != 1 {
            abort!(attr.tokens, "Expected only an identifier (without quotes)");
        }
        match tokens.remove(0) {
            TokenTree::Ident(ident) => ident,
            _ => abort!(attr.tokens, "Expected an identifier (without quotes)")
        }
    })

}

pub(crate) fn get_mode_list(attrs: &mut Vec<Attribute>) -> Option<Vec<Ident>> {
    let attr = get_attr("mode", attrs)?;
    match attr.parse_meta() {
        Ok(syn::Meta::List(syn::MetaList { nested, .. })) => {
            let mut list = vec![];
            for nest in nested {
                match nest {
                    syn::NestedMeta::Meta(syn::Meta::Path(ref path)) => match path.get_ident() {
                        Some(id) => list.push(id.clone()),
                        _ => abort!(nest, "Modes must be a list of identifiers (without quotes)"),
                    },
                    _ => abort!(nest, "Modes must be a list of identifiers (without quotes)"),
                }
            }
            Some(list)
        }
        _ => abort!(attr, "Modes must be a list of identifiers. Omit the attribute if you only have one mode."),
    }
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
             )) => {
            syn::parse(mac.tokens.into()).unwrap()
        }
        _ => {
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

use quote::ToTokens;
use syn::Attribute;

pub(crate) struct ParceMacroError(pub Box<dyn ToTokens>, pub String);

pub(crate) fn has_attr(s: &str, attrs: &Vec<Attribute>) -> bool {
    for attr in attrs {
        if let Some(ident) = attr.path.get_ident() {
            if ident.to_string() == s {
                return true;
            }
        }
    }
    false
}

pub(crate) fn get_attr<'a>(s: &str, attrs: &'a Vec<Attribute>) -> Option<&'a Attribute> {
    for attr in attrs {
        if let Some(ident) = attr.path.get_ident() {
            if ident.to_string() == s {
                return Some(attr);
            }
        }
    }
    None
}

pub(crate) fn get_attr_mut<'a>(s: &str, attrs: &'a mut Vec<Attribute>) -> Option<&'a mut Attribute> {
    for attr in attrs {
        if let Some(ident) = attr.path.get_ident() {
            if ident.to_string() == s {
                return Some(attr);
            }
        }
    }
    None
}

pub(crate) fn get_ident_list(s: &str, attrs: &Vec<Attribute>) -> Option<Vec<String>> {
    let attr = get_attr(s, attrs)?;
    match attr.parse_meta() {
        Ok(syn::Meta::List(syn::MetaList {nested,..})) => {
            let mut list = vec![];
            for nest in nested {
                match nest {
                    syn::NestedMeta::Meta(syn::Meta::Path(path)) => {
                        match path.get_ident() {
                            Some(id) => list.push(id.to_string()),
                            _ => return None
                        }
                    }
                    _ => return None
                }
            }
            Some(list)
        },
        _ => return None
    }
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

pub(crate) fn get_lexer_path(meta: &syn::Meta) -> Option<syn::Path> {
    match meta {
        syn::Meta::Path(path) => Some(path.clone()),
        _ => None
    }
}

pub(crate) fn get_pattern(variant: &syn::Variant) -> Result<String, ParceMacroError> {
    match &variant.discriminant {
        Some((_, syn::Expr::Lit(syn::ExprLit {
            lit: syn::Lit::Str(lit_str), ..
        }))) => Ok(lit_str.value()),
        _ => {
            return Err(ParceMacroError(Box::new(variant.clone()), "discriminant must a str literal".to_string()));
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub(crate) enum RangeRuleMax {
    Infinite,
    Fixed,
    Some(usize)
}

lazy_static::lazy_static! {
    pub(crate) static ref COUNT_PARSER: regex::Regex = regex::Regex::new(r"\{(\d+),?(\d+)?\}").unwrap();
}

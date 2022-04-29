use std::collections::HashSet;
use crate::{helper, DISCRIMINANT_TAG};
use proc_macro2::{Ident, TokenStream};
use serde::{Deserialize, Serialize};
use std::str::FromStr;
use proc_macro_error::{abort, abort_call_site, emit_warning};
use quote::{format_ident, quote, ToTokens};
use crate::helper::{get_attr_equals_idents};

#[derive(Serialize, Deserialize, Debug)]
pub enum LexerAst {
    Literal(String),
    Ident(String),
    Group(Vec<LexerAst>),
    Star(Box<LexerAst>),
    Plus(Box<LexerAst>),
    Question(Box<LexerAst>),
    Dot,
    Or(Vec<LexerAst>),
}

impl FromStr for LexerAst {
    type Err = serde_json::Error;

    fn from_str(s: &str) -> serde_json::Result<Self> {
        Ok(if s.starts_with(DISCRIMINANT_TAG) {
            serde_json::from_str(&s[DISCRIMINANT_TAG.len()..])?
        } else {
            LexerAst::Literal(s.to_string())
        })
    }
}

#[derive(Debug)]
struct LexemeVariant {
    ident: Ident,
    skip: bool,
    frag: bool,
    modes: Vec<Ident>,
    set_mode: Option<Ident>,
    discriminant: LexerAst
}

pub fn process_lexer(_lexer_ident: Ident, mut ast: syn::DeriveInput) -> TokenStream {
    if let syn::Data::Enum(ref mut lexer_enum) = ast.data {
        let default_mode = helper::get_attr_equals_ident("default_mode", &mut ast.attrs);
        let mut populated_modes = HashSet::new();
        let mut set_modes = HashSet::new();
        let mut current_modes = vec![format_ident!("Default")];
        let variants: Vec<_> = lexer_enum.variants.iter_mut().map(|variant| {
            let ident = variant.ident.clone();
            let skip = helper::get_attr("skip", &mut variant.attrs).is_some();
            let frag = helper::get_attr("frag", &mut variant.attrs).is_some();
            let modes = if let Some(list) = get_attr_equals_idents("mode", &mut variant.attrs) {
                current_modes = list.clone();
                populated_modes.extend(list.clone());
                list
            } else {
                current_modes.clone()
            };
            let set_mode = helper::get_attr_equals_ident("set_mode", &mut variant.attrs);
            if let Some(ident) = &set_mode {
                set_modes.insert(ident.clone());
            }
            let new_discriminant = if let Some(attr) = helper::get_attr("discriminant", &mut variant.attrs) {
                if let Ok(syn::Meta::NameValue(name_value)) = attr.parse_meta() {
                    Some((name_value.eq_token, syn::Expr::Macro(syn::ExprMacro { attrs: vec![], mac: syn::Macro {
                        path: format_ident!("d").into(),
                        bang_token: Default::default(),
                        delimiter: syn::MacroDelimiter::Paren(Default::default()),
                        tokens: name_value.lit.to_token_stream()
                    } })))
                } else {
                    abort!(attr, "Discriminant must be of the form #[discriminant = <integer>]")
                }
            } else {
                None
            };
            let discriminant = helper::get_ast(variant, new_discriminant, "Lexeme pattern must be a literal char, literal str, or an invocation of the l!() macro");
            LexemeVariant {
                ident,
                skip,
                frag,
                modes,
                set_mode,
                discriminant
            }
        }).collect();

        let _default_mode = if let Some(def) = default_mode {
            set_modes.insert(def.clone());
            def
        } else {
            if !populated_modes.is_empty() || !set_modes.is_empty() {
                abort_call_site!("Default mode must be specified in multi-modal lexers");
            } else {
                let default = format_ident!("Default");
                populated_modes.insert(default.clone());
                set_modes.insert(default.clone());
                default
            }
        };

        let unset_modes: Vec<&Ident> = populated_modes.difference(&set_modes).collect();
        let unpopulated_modes: Vec<&Ident> = set_modes.difference(&populated_modes).collect();
        for mode in unset_modes {
            emit_warning!(mode, "Mode has members but is never activated.");
        }
        for mode in unpopulated_modes {
            emit_warning!(mode, "Mode is activated but has no members.");
        }

        dbg!(variants);

        quote!{ #ast }
    } else {
        abort_call_site!("Lexer macro must be applied to an enum of lexemes, even if you only need one variant");
    }
}

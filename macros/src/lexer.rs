use std::collections::HashSet;
use crate::helper;
use proc_macro2::{Ident, TokenStream};
use std::str::FromStr;
use proc_macro_error::{abort, abort_call_site, emit_warning};
use quote::{format_ident, quote, ToTokens};
use crate::helper::{get_attr_equals_idents};
use crate::tree::{ItemSource, SliceMatcher, Tree};

#[derive(Debug)]
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
    type Err = ();

    fn from_str(s: &str) -> Result<Self, ()> {
        Ok(LexerAst::Literal(s.to_string()))
    }
}

impl LexerAst {
    fn into_tree(self, success: Tree, failure: Tree) -> Tree {
        use LexerAst::*;

        match self {
            Literal(lit) => {
                Tree::Match {
                    length: lit.len(),
                    arms: vec![
                        (SliceMatcher::literal(lit), Box::new(success)),
                        (SliceMatcher::Else, Box::new(failure))
                    ]
                }
            },
            Group(asts) => {
                let mut result = success;
                for ast in asts.into_iter().rev() {
                    result = ast.into_tree(result, failure.clone());
                }
                result
            }
            Dot => Tree::Match {
                arms: vec![
                    (SliceMatcher::any(1), Box::new(success.deprioritize())),
                    (SliceMatcher::Else, Box::new(failure))
                ],
                length: 1
            },
            Question(ast) => {
                let matched_branch = ast.into_tree(success.clone().deprioritize(), failure);
                matched_branch.merge(success.deprioritize())
            }
            _ => todo!()
        }
    }
}

#[derive(Debug)]
#[allow(dead_code)]
struct TokenVariant {
    ident: Ident,
    skip: bool,
    frag: bool,
    modes: VariantModes,
    set_mode: Option<Ident>,
    discriminant: LexerAst
}

#[derive(Clone, Debug)]
enum VariantModes {
    Explicit(Vec<Ident>),
    Implicit
}

pub fn process_lexer(lexer_ident: Ident, mut ast: syn::DeriveInput) -> TokenStream {
    if let syn::Data::Enum(ref mut lexer_enum) = ast.data {
        let default_mode = helper::get_attr_equals_ident("default_mode", &mut ast.attrs);
        let mut populated_modes = HashSet::new();
        let mut set_modes = HashSet::new();
        let mut current_modes = VariantModes::Implicit;
        let mut variants: Vec<TokenVariant> = lexer_enum.variants.iter_mut().map(|variant| {
            let ident = variant.ident.clone();
            let skip = helper::get_attr("skip", &mut variant.attrs).is_some();
            let frag = helper::get_attr("frag", &mut variant.attrs).is_some();
            let modes = if let Some(list) = get_attr_equals_idents("mode", &mut variant.attrs) {
                current_modes = VariantModes::Explicit(list.clone());
                populated_modes.extend(list.clone());
                current_modes.clone()
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
            let discriminant = helper::get_ast(variant, new_discriminant, "Lexeme pattern must be a literal char, literal str, or an invocation of the p!() macro");
            TokenVariant {
                ident,
                skip,
                frag,
                modes,
                set_mode,
                discriminant
            }
        }).collect();

        let default_mode = if let Some(def) = default_mode {
            set_modes.insert(def.clone());
            def
        } else {
            if !populated_modes.is_empty() || !set_modes.is_empty() {
                abort_call_site!("Default mode must be specified in modal lexers");
            } else {
                let default = format_ident!("Default");
                populated_modes.insert(default.clone());
                set_modes.insert(default.clone());
                for variant in &mut variants {
                    variant.modes = VariantModes::Explicit(vec![default.clone()])
                }
                default
            }
        };

        for mode in populated_modes.difference(&set_modes) {
            emit_warning!(mode, "Mode has members but is never activated.");
        }
        for mode in set_modes.difference(&populated_modes) {
            emit_warning!(mode, "Mode is activated but has no members.");
        }

        let all_modes: Vec<_> = set_modes.union(&populated_modes).collect();

        if all_modes.len() > 1 {
            for variant in &variants {
                if let VariantModes::Implicit = variant.modes {
                    abort!(variant.ident, "You must explicitly declare the modes for all variants in modal lexers");
                }
            }
        }

        let tokens_ident = ast.ident.clone();

        let mut tree = Tree::Err;
        for variant in variants {
            let variant_ident = variant.ident;
            tree = tree.merge(variant.discriminant.into_tree(Tree::Ok { result: quote! {#tokens_ident::#variant_ident}, priority: 0}, Tree::Err));
        }
        tree.squash();
        tree.prune_err();

        let tree_for_slice = tree.to_tokens(quote! {0}, ItemSource::Slice);
        let tree_for_buffered_iter = tree.to_tokens(quote! {0}, ItemSource::BufferedIter);

        quote!{
            #ast

            enum #lexer_ident {
                #(
                    #all_modes,
                )*
            }

            impl Default for #lexer_ident {
                fn default() -> Self {
                    Self::#default_mode
                }
            }

            impl parce::prelude::Lexer for #lexer_ident {
                type Input = char;
                type Output = #tokens_ident;

                #[inline]
                fn lex_from_slice(&mut self, input: &[char]) -> Option<parce::Lexeme<Self::Output>> {
                    #tree_for_slice
                }

                #[inline]
                fn lex_from_buffered_iter<Iter: Iterator<Item=Self::Input>>(&mut self, input: &mut parce::iterator::BufferedIterator<Self::Input, Iter>) -> Option<parce::Lexeme<Self::Output>> {
                    #tree_for_buffered_iter
                }
            }
        }
    } else {
        abort_call_site!("Lexer macro must be applied to an enum of tokens, even if you only need one variant");
    }
}

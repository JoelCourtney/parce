//! Parce macros

// #![doc(test(attr(deny(warnings))))]

mod discriminant;
mod helper;
mod tree;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use std::str::FromStr;
use proc_macro2::Ident;
use proc_macro_error::{abort, abort_call_site, proc_macro_error};
use syn::{parse_macro_input};
use quote::{format_ident, quote, ToTokens};
use crate::helper::{get_attr_equals_ident, get_lexer_ident};
use crate::tree::{ItemSource, Tree, SliceMatcher};

/// Apply this macro to an enum of lexemes to create a lexer that generates them.
///
/// TODO
#[proc_macro_error]
#[proc_macro_attribute]
pub fn parce(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as syn::Meta);
    let ident = match get_lexer_ident(&args) {
        Some(id) => id,
        None => abort!(args, "Lexer or Parser is missing, e.g. #[parce(MyLexer)]")
    };
    let mut ast = parse_macro_input!(input as syn::DeriveInput);
    if let syn::Data::Enum(ref mut grammar) = ast.data {
        let mut variants: Vec<Variant> = grammar.variants.iter_mut().map(|variant| {
            let ident = variant.ident.clone();
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
            let discriminant = helper::get_ast(variant, new_discriminant, "Pattern must be a literal char, literal str, or an invocation of the p!() macro");
            Variant {
                ident,
                discriminant
            }
        }).collect();

        let sentences_ident = ast.ident.clone();

        let mut tree = Tree::Err;
        for variant in variants {
            let variant_ident = variant.ident;
            tree = tree.merge(variant.discriminant.into_tree(Tree::Ok { result: quote! {#sentences_ident::#variant_ident}, priority: 0}, Tree::Err));
        }
        tree.squash();
        tree.prune_err();

        let tree_for_slice = tree.to_tokens(quote! {0}, ItemSource::Slice);
        let tree_for_buffered_iter = tree.to_tokens(quote! {0}, ItemSource::BufferedIter);

        (quote!{
            #ast

            struct #ident;

            impl Default for #ident {
                fn default() -> Self {
                    Self
                }
            }

            impl parce::prelude::Parce for #ident {
                type Input = char;
                type Output = #sentences_ident;

                #[inline]
                fn process_slice(&mut self, input: &[char]) -> Option<parce::Sentence<Self::Output>> {
                    #tree_for_slice
                }

                #[inline]
                fn process_buffered_iter<Iter: Iterator<Item=Self::Input>>(&mut self, input: &mut parce::iterator::BufferedIterator<Self::Input, Iter>) -> Option<parce::Sentence<Self::Output>> {
                    #tree_for_buffered_iter
                }
            }
        }).into()
    } else {
        abort_call_site!("Lexer macro must be applied to an enum of tokens, even if you only need one variant")
    }
}

#[derive(Debug)]
enum Ast {
    Literal(String),
    Ident(String),
    Group(Vec<Ast>),
    Star(Box<Ast>),
    Plus(Box<Ast>),
    Question(Box<Ast>),
    Dot,
    Or(Vec<Ast>),
}

impl Ast {
    fn into_tree(self, success: Tree, failure: Tree) -> Tree {
        use Ast::*;

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

impl FromStr for Ast {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, ()> {
        Ok(Ast::Literal(s.to_string()))
    }
}

#[derive(Debug)]
#[allow(dead_code)]
struct Variant {
    ident: Ident,
    discriminant: Ast
}



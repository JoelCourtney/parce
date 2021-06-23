//! This crate contains the [macro@lexer] and [macro@parser] macros used by the Parce crate.
//! Documentation for these macros are placed on the re-exports in the main crate,
//! because they generate code that depends on the main crate, meaning doc-tests cannot
//! be run here.

use proc_macro::{TokenStream};
use syn::parse_macro_input;
use proc_macro_error::{proc_macro_error, abort};
use quote::quote;

mod lexer;
mod parser;
mod common;
mod discriminants;

use common::*;

#[proc_macro_error]
#[proc_macro_attribute]
pub fn lexer(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as syn::Meta);
    let lexer_ident = match get_lexer_ident(&args) {
        Some(id) => id,
        None => abort!(args, "lexer name must be specified")
    };
    let input = parse_macro_input!(input as syn::ItemEnum);
    match lexer::lexer(lexer_ident, input) {
        Ok(s) => s.into(),
        Err(ParceMacroError(who, message)) => abort!(who, message)
    }
}

#[proc_macro_error]
#[proc_macro_attribute]
pub fn parser(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as syn::Meta);
    let lexer_ident = match get_lexer_path(&args) {
        Some(id) => id,
        None => abort!(args, "lexer name must be specified")
    };
    let input = parse_macro_input!(input as syn::ItemEnum);
    match parser::parser(lexer_ident, input) {
        Ok(s) => s.into(),
        Err(ParceMacroError(who, message)) => abort!(who, message)
    }
}

/// No-op derive macro that declares the helper attributes used by the primary [macro@lexer] macro.
///
/// Its a little hacky, I know, but its simpler than manually removing all of these attributes
/// in the main macro.
#[proc_macro_derive(RemoveLexerAttributes, attributes(skip, frag, set_mode, mode, modes))]
pub fn lex_attributes(_input: TokenStream) -> TokenStream {
    (quote! {}).into()
}


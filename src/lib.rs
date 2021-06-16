use proc_macro::{TokenStream};
use syn::parse_macro_input;
use proc_macro_error::{proc_macro_error, abort};
use quote::quote;
use crate::lexer::LexerMacroError;

mod lexer;
mod parser;

#[proc_macro_error]
#[proc_macro_attribute]
pub fn lexer(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as syn::Meta);
    let lexer_ident = match args.clone() {
        syn::Meta::Path(path) => {
            match path.get_ident() {
                Some(id) => id.clone(),
                None => abort!(args, "lexer arg must be a single identifier")
            }
        }
        _ => abort!(args, "lexer arg must be a single identifier")
    };
    let input = parse_macro_input!(input as syn::ItemEnum);
    match lexer::lexer(lexer_ident, input.clone()) {
        Ok(s) => s.into(),
        Err(LexerMacroError(who, message)) => abort!(who, message)
    }
}

#[proc_macro_error]
#[proc_macro_attribute]
pub fn parser(_args: TokenStream, input: TokenStream) -> TokenStream {
    input
}

#[proc_macro_derive(RemoveLexerAttributes, attributes(skip, frag, set_mode, mode, modes))]
pub fn lex_attributes(_input: TokenStream) -> TokenStream {
    (quote! {}).into()
}
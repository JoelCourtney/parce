mod discriminant;
mod helper;
mod lexer;
mod parser;

use proc_macro::TokenStream;
use proc_macro_error::{abort, proc_macro_error};
use syn::{parse_macro_input};

use lexer::LexerAst;
use parser::ParserAst;

#[proc_macro_error]
#[proc_macro_attribute]
pub fn lexer(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as syn::Meta);
    let lexer_ident = match helper::get_lexer_ident(&args) {
        Some(id) => id,
        None => abort!(args, "lexer name must be specified")
    };
    let ast = parse_macro_input!(input as syn::DeriveInput);
    lexer::process_lexer(lexer_ident, ast).into()
}

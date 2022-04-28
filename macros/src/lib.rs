mod lexer;
mod parser;
mod discriminant;

use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;
use proc_macro_error::proc_macro_error;

use lexer::LexerAst;
use parser::ParserAst;

const DISCRIMINANT_TAG: &'static str = "###PARCE-DISCRIMINANT:";

#[proc_macro_error]
#[proc_macro]
pub fn l(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as LexerAst);
    let text = format!("{DISCRIMINANT_TAG}{}", serde_json::to_string(&ast).unwrap());
    println!("{text}");
    (quote! { #text }).into()
}

#[proc_macro_error]
#[proc_macro]
pub fn p(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as ParserAst);
    let text = format!("{DISCRIMINANT_TAG}{}", serde_json::to_string(&ast).unwrap());
    (quote! { #text }).into()
}
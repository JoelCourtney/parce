use proc_macro::TokenStream;
use syn::{parse_macro_input};
use quote::quote;
use syn::parse::Parser;

#[proc_macro_attribute]
pub fn lex(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(input as syn::ItemEnum);
    for variant in &mut input.variants {
        variant.discriminant = None;
    }

    let ident = input.ident.clone();

    (quote! {
        #[derive(gen_parser_macro::LexAttributes)]
        #input

        impl gen_parser::Lex for #ident {

            fn lex(s: &mut str) -> Result<gen_parser::Lexeme<#ident>, gen_parser::LexError> {
                use gen_parser::{Lexeme, LexError};
                todo!()
            }
        }
    }).into()
}

#[proc_macro_derive(LexAttributes, attributes(skip, frag))]
pub fn lex_attributes(_input: TokenStream) -> TokenStream {
    (quote! {}).into()
}

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
        #[derive(parce_macros::LexAttributes)]
        #input

        impl parce::Lex for #ident {

            fn lex(s: &mut str) -> Result<parce::Lexeme<#ident>, parce::LexError> {
                use parce::{Lexeme, LexError};
                todo!()
            }
        }
    }).into()
}

#[proc_macro_derive(LexAttributes, attributes(skip, frag))]
pub fn lex_attributes(_input: TokenStream) -> TokenStream {
    (quote! {}).into()
}

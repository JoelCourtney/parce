use proc_macro::{TokenStream};
use syn::{parse_macro_input, Attribute};
use quote::{quote, format_ident};
use proc_macro2::Ident;
use proc_macro_error::{proc_macro_error, emit_error};
use inflector::Inflector;
use proc_macro2::TokenStream as TokenStream2;

#[derive(Debug)]
struct VariantInfo {
    mode: String,
    ident: Ident,
    pattern: String,
    fragment: bool,
    skip: bool,
    set_mode: Option<String>,
}

#[proc_macro_error]
#[proc_macro_attribute]
pub fn lex(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(input as syn::ItemEnum);
    let mut modes = vec![String::from("default")];
    let mut variant_info = vec![];
    for variant in &mut input.variants {
        let mut info = VariantInfo {
            mode: "".to_string(),
            ident: variant.ident.clone(),
            pattern: match &variant.discriminant {
                Some((_, syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(lit_str), ..
                }))) => lit_str.value(),
                _ => {
                    emit_error!(variant, "discriminant must be a str literal");
                    "".to_string()
                }
            },
            fragment: has_attr("frag", &variant.attrs),
            skip: has_attr("skip", &variant.attrs),
            set_mode: match get_attr("set_mode", &variant.attrs) {
                Some(attr) => {
                    let meta = attr.parse_meta().unwrap();
                    match meta {
                        syn::Meta::NameValue(syn::MetaNameValue {lit: syn::Lit::Str(lit_str), ..}) => {
                            Some(lit_str.value())
                        }
                        _ => {
                            emit_error!(attr, "must be of the form $[mode = \"mode_name\"]");
                            None
                        }
                    }
                }
                None => None
            }
        };
        match get_attr("mode", &variant.attrs) {
            Some(attr) => {
                let meta = attr.parse_meta().unwrap();
                match meta {
                    syn::Meta::NameValue(syn::MetaNameValue {lit: syn::Lit::Str(lit_str), ..}) => {
                        modes.push(lit_str.value())
                    }
                    _ => emit_error!(attr, "must be of the form #[new_mode = \"mode_name\"]")
                }
            }
            None => {}
        }
        info.mode = modes.last().unwrap().clone();
        variant_info.push(info);
        variant.discriminant = None;
    }

    let ident = input.ident.clone();
    let lexer_ident = format_ident!("{}Lexer", input.ident.clone());
    let mode_idents: Vec<_> = modes.iter().map(|mode| format_ident!("{}", mode.to_class_case())).collect();

    let struct_name = format!("{}PatternMatchers", input.ident.to_string());
    let struct_ident = format_ident!("{}", struct_name);

    let mut pattern_matchers = vec![];
    let mut lexemes = vec![];
    for info in &variant_info {
        pattern_matchers.push(
            quote! {
                if let Some((data, length)) = todo!() {
                    if let Some((_, longest_length)) = longest {
                        if longest_length < length {
                            longest = Some((data, length))
                        }
                    } else {
                        longest = Some((data, length))
                    }
                }
            }
        );
        if !info.fragment {
            let lexeme_ident = info.ident.clone();
            lexemes.push(quote! {#ident::#lexeme_ident});
        }
    }


    (quote! {
        #[derive(parce_macros::LexAttributes, variants_struct::VariantsStruct, Debug)]
        #[struct_name = #struct_name]
        #[struct_derive(Default)]
        #input

        enum #lexer_ident {
            #(#mode_idents),*
        }

        impl Default for #lexer_ident {
            fn default() -> Self {
                #lexer_ident::Default
            }
        }

        impl parce::lexer::Lexer<#ident> for #lexer_ident {
            fn lex(&mut self, s: &str, start: usize) -> Result<parce::lexer::Lexeme<#ident>, parce::lexer::LexError> {
                use parce::lexer::{Lexeme, LexError};
                let mut longest: Option<(#ident, usize)> = None;

                let pattern_matchers: #struct_ident<i32> = #struct_ident {
                    // #(#pattern_matchers)*
                    ..Default::default()
                };

                for lexeme in &[#(#lexemes),*] {
                    dbg!(pattern_matchers.get_unchecked(&lexeme));
                }

                match longest {
                    Some((data, length)) => Ok(Lexeme {
                        data,
                        start,
                        length
                    }),
                    None => Err(LexError)
                }
            }
        }
    }).into()
}

#[proc_macro_derive(LexAttributes, attributes(skip, frag, set_mode, mode))]
pub fn lex_attributes(_input: TokenStream) -> TokenStream {
    (quote! {}).into()
}

fn has_attr(s: &str, attrs: &Vec<Attribute>) -> bool {
    for attr in attrs {
        if let Some(ident) = attr.path.get_ident() {
            if ident.to_string() == s {
                return true;
            }
        }
    }
    false
}

fn get_attr<'a>(s: &str, attrs: &'a Vec<Attribute>) -> Option<&'a Attribute> {
    for attr in attrs {
        if let Some(ident) = attr.path.get_ident() {
            if ident.to_string() == s {
                return Some(attr);
            }
        }
    }
    None
}

fn lexer_discriminant(s: &str) -> TokenStream2 {
    todo!()
}
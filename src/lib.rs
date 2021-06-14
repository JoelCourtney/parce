use proc_macro::{TokenStream};
use syn::{parse_macro_input, Attribute};
use quote::{quote, format_ident};
use proc_macro2::Ident;
use proc_macro_error::{proc_macro_error, emit_error, abort};
use inflector::Inflector;
use proc_macro2::TokenStream as TokenStream2;
use parce_core::LexError;

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

    let mut pattern_matchers = vec![];
    for info in &variant_info {
        let fn_ident = format_ident!("{}", info.ident.to_string().to_snake_case());
        let matcher = match gen_matchers(info.pattern.clone()) {
            Ok(m) => m,
            Err(_) => abort!(input, "it broke dude")
        };
        pattern_matchers.push(
            quote! {
                fn #fn_ident(s: &str, mut start: usize) -> Option<usize> {
                    #matcher
                }

            }
        );
        if !info.fragment {
            let lexeme_ident = info.ident.clone();
            pattern_matchers.push(
                quote! {
                    match #fn_ident(s, start) {
                        Some(len) => {
                            match longest {
                                Some((lexeme, longest_len)) if longest_len < len => {
                                    longest = Some((#ident::#lexeme_ident, len))
                                }
                                None => longest = Some((#ident::#lexeme_ident, len)),
                                _ => {}
                            }
                        }
                        _ => {}
                    }
                }
            )
        }
    }

    (quote! {
        #[derive(parce_macros::LexAttributes, Debug, Eq, PartialEq, Copy, Clone, Hash)]
        #input

        enum #lexer_ident {
            #(#mode_idents),*
        }

        impl Default for #lexer_ident {
            fn default() -> Self {
                #lexer_ident::Default
            }
        }

        impl parce::prelude::Lexer<#ident> for #lexer_ident {
            fn lex(&mut self, s: &str) -> Result<Vec<parce::prelude::Lexeme<#ident>>, parce::prelude::LexError> {
                use parce::prelude::{Lexeme, LexError};

                let mut result = vec![];
                let mut start = 0;
                while start < s.len() {
                    let mut longest: Option<(#ident, usize)> = None;

                    #(#pattern_matchers)*

                    match longest {
                        Some((data, len)) if len > 0 => {
                            result.push(
                                Lexeme {
                                    data,
                                    start,
                                    len
                                }
                            );
                            start += len;
                        }
                        _ => return Err(LexError)
                    }
                }
                if start == s.len() {
                    Ok(result)
                } else {
                    Err(LexError)
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

#[derive(Clone, Debug)]
enum LexDiscriminantRule {
    And(Vec<LexDiscriminantRule>),
    Or(Vec<LexDiscriminantRule>),
    Group(Vec<LexGroupRule>, bool),
    Literal(String),
    Lexeme(String),
    Dot,
    Star(Box<LexDiscriminantRule>),
    Plus(Box<LexDiscriminantRule>),
    Question(Box<LexDiscriminantRule>)
}

impl LexDiscriminantRule {
    fn to_matcher(&self) -> TokenStream2 {
        use LexDiscriminantRule::*;
        // Heres the deal
        // Given a rule, produce a matcher
        // the matcher is an expression, that produces Some(len)
        // where len is the length of the match if it finds a match
        // or none it it doesn't
        // match on the string s, starting at index start
        // start is mutable, you may need to mutate it, but
        // must be returned to its initial value before the matcher
        // exits. This is so that star/plus/question play nicely with
        // And/Or. Also: DO NOT USE CARRIERS
        // using ? produces return statements on intermediate failures
        // which we don't want
        match self {
            Literal(s) => {
                let len = s.len();
                quote! {
                    if s.len() >= start+#len && &s[start..start+#len] == #s {
                        Some(#len)
                    } else {
                        None
                    }
                }
            }
            Lexeme(l) => {
                let lexeme_fn_ident = format_ident!("{}", l.to_snake_case());
                quote! {
                    #lexeme_fn_ident(s, start)
                }
            }
            And(v) => {
                let matchers: Vec<TokenStream2> = v.iter().map(|rule| rule.to_matcher()).collect();
                quote! {
                    let old_start = start;
                    let result = loop {
                        #(
                            match {#matchers} {
                                Some(len) => start += len,
                                None => break None
                            }
                        )*
                        break Some(start - old_start);
                    };
                    start = old_start;
                    result
                }
            }
            Or(v) => {
                let matchers: Vec<_> = v.iter().map(|rule| rule.to_matcher()).collect();
                quote! {
                    let mut lengths = vec![];
                    #(
                        match {#matchers} {
                            Some(l) => lengths.push(l),
                            None => {}
                        }
                    )*
                    lengths.max()
                }
            }
            Group(g, negate) => {
                let mut chars = vec![];
                for rule in g {
                    match rule {
                        LexGroupRule::Single(c) => chars.push(c),
                        // LexGroupRule::Range(_c1, _c2) => {}
                    }
                }
                let bang = if *negate {
                    quote! {!}
                } else {
                    quote! {}
                };
                quote! {
                    if s.len() > start && #bang[#(#chars),*].contains(&s.chars().nth(start).unwrap()) {
                        Some(1)
                    } else {
                        None
                    }
                }
            }
            Star(r) => {
                let matcher = r.to_matcher();
                quote! {
                    let old_start = start;
                    while let Some(len) = {#matcher} {
                        start += len;
                    }
                    let diff = start - old_start;
                    start = old_start;
                    Some(diff)
                }
            }
            Plus(r) => {
                let matcher = r.to_matcher();
                quote! {
                    let old_start = start;
                    let result = loop {
                        if let Some(len) = {#matcher} {
                            start += len;
                        } else {
                            break None;
                        }
                        while let Some(len) = {#matcher} {
                            start += len;
                        }
                        break Some(start - old_start)
                    };
                    start = old_start;
                    result
                }
            }
            Question(r) => {
                let matcher = r.to_matcher();
                quote! {
                    if let Some(len) = {#matcher} {
                        Some(len)
                    } else {
                        None
                    }
                }
            }
            Dot => {
                quote! {
                    if s.len() > start {
                        Some(1)
                    } else {
                        None
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
enum LexGroupRule {
    Single(char),
    // Range(char, char)
}

fn gen_matchers(s: String) -> Result<TokenStream2, LexError> {
    let rule = parse_matcher(s)?;
    Ok(rule.to_matcher())
}

fn parse_matcher(s: String) -> Result<LexDiscriminantRule, LexError> {
    let chars: Vec<char> = s.chars().collect();

    // Search for |
    let mut i = 0;
    let mut in_string = false;
    let mut group_depth = 0;
    let mut splits = vec![-1_i32];
    while i < s.len() {
        match chars[i] {
            '(' | '[' if !in_string => group_depth += 1,
            ')' | ']' if !in_string => group_depth -= 1,
            '\'' if !in_string || chars[i-1] != '\\' => in_string = !in_string,
            '|' if !in_string && group_depth == 0 => splits.push(i as i32),
            _ => {}
        }
        i += 1;
    }
    if splits.len() > 1 {
        let mut options = vec![];
        for j in 0..splits.len()-1 {
            options.push(parse_matcher(s[splits[j+1] as usize..splits[j+1] as usize].to_string())?);
        }
        return Ok(LexDiscriminantRule::Or(options));
    }

    // Generate other lexemes. If just one return it, otherwise return And.
    let mut result = vec![];
    i = 0;
    while i < s.len() {
        match chars[i] {
            '\'' => {
                let mut j = i + 1;
                while j < s.len() && chars[j] != '\'' && chars[j-1] != '\\' {
                    j += 1;
                }
                if j != s.len() {
                    result.push(LexDiscriminantRule::Literal(s[i+1..j].to_string()));
                    i = j;
                } else {
                    return Err(LexError);
                }
            }
            '[' => {
                let mut j = i + 1;
                while j < s.len() && chars[j] != ']' && chars[j-1] != '\\' {
                    j += 1;
                }
                if j != s.len() {
                    let (rules, negate) = parse_group(s[i+1..j].to_string())?;
                    result.push(LexDiscriminantRule::Group(rules, negate));
                    i = j;
                }
            }
            c if c.is_alphabetic() => {
                let mut j = i + 1;
                while j < s.len() && chars[j].is_alphanumeric() {
                    j += 1;
                }
                result.push(LexDiscriminantRule::Lexeme(s[i..j].to_string()));
                i = j - 1;
            }
            '.' => result.push(LexDiscriminantRule::Dot),
            '*' => match result.pop() {
                Some(l) => result.push(LexDiscriminantRule::Star(Box::new(l))),
                None => return Err(LexError)
            },
            '+' => match result.pop() {
                Some(l) => result.push(LexDiscriminantRule::Plus(Box::new(l))),
                None => return Err(LexError)
            },
            '?' => match result.pop() {
                Some(l) => result.push(LexDiscriminantRule::Question(Box::new(l))),
                None => return Err(LexError)
            },
            c if c.is_whitespace() => {}
            _ => return Err(LexError)
        }
        i += 1;
    }
    if result.len() == 0 {
        Err(LexError)
    } else if result.len() == 1 {
        Ok(result.remove(0))
    } else {
        Ok(LexDiscriminantRule::And(result))
    }
}

fn parse_group(s: String) -> Result<(Vec<LexGroupRule>, bool), LexError> {
    if s.len() == 0 {
        return Err(LexError)
    }
    let chars: Vec<char> = s.chars().collect();

    let negate = chars[0] == '^';
    let mut result = vec![];
    let mut i = 0;
    while i < s.len() {
        match chars[i] {
            c => result.push(LexGroupRule::Single(c))
        }
        i += 1;
    }
    Ok((result, negate))
}
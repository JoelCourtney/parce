use proc_macro2::TokenStream as TokenStream2;
use syn::Attribute;
use quote::{quote, format_ident, ToTokens};
use proc_macro2::Ident;
use inflector::Inflector;
use check_keyword::CheckKeyword;

pub struct LexerPatternParseError(pub Box<dyn ToTokens>, pub String);

#[derive(Debug)]
struct VariantInfo {
    mode: String,
    ident: Ident,
    pattern: String,
    fragment: bool,
    skip: bool,
    set_mode: Option<String>,
}

pub fn lexer(lexer_ident: Ident, mut input: syn::ItemEnum) -> Result<TokenStream2, LexerPatternParseError> {
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
                    return Err(LexerPatternParseError(Box::new(variant.clone()), "discriminant must a str literal".to_string()));
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
                            return Err(LexerPatternParseError(Box::new(attr.clone()), "set_mode must be of the form $[set_mode = \"mode_name\"]".to_string()));
                        }
                    }
                }
                None => None
            }
        };
        match get_attr("new_mode", &variant.attrs) {
            Some(attr) => {
                let meta = attr.parse_meta().unwrap();
                match meta {
                    syn::Meta::NameValue(syn::MetaNameValue {lit: syn::Lit::Str(lit_str), ..}) => {
                        modes.push(lit_str.value())
                    }
                    _ => {
                        // emit_error!(attr, "must be of the form #[new_mode = \"mode_name\"]")
                        return Err(LexerPatternParseError(Box::new(attr.clone()), "new_mode must be of the form #[new_mode = \"mode_name\"]".to_string()));
                    }
                }
            }
            None => {}
        }
        info.mode = modes.last().unwrap().clone();
        variant_info.push(info);
        variant.discriminant = None;
    }

    let ident = input.ident.clone();
    let mode_idents: Vec<_> = modes.iter().map(|mode| format_ident!("{}", mode.to_class_case().into_safe())).collect();

    let mut pattern_matchers = vec![];
    let mut statics: Vec<TokenStream2> = vec![];
    let mut non_fragment_checks = vec![];
    let mut no_skip = vec![];
    for info in &variant_info {
        let lexeme_ident = info.ident.clone();
        let fn_ident = format_ident!("{}", info.ident.to_string().to_snake_case().into_safe());
        let (matcher, stat) = match gen_matchers(info.pattern.clone()) {
            Ok((m, s)) => (m, s),
            Err(e) => {
                return Err(e);
            }
        };
        pattern_matchers.push(
            quote! {
                fn #fn_ident(s: &str, mut start: usize) -> Option<usize> {
                    #matcher
                }

            }
        );
        statics.push(stat);
        if !info.fragment {
            non_fragment_checks.push(
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
        if !info.skip {
            no_skip.push(quote! {#ident::#lexeme_ident});
        }
    }

    Ok(quote! {
        #[derive(parce_macros::RemoveLexerAttributes, Debug, Eq, PartialEq, Copy, Clone, Hash)]
        #input

        #[derive(Debug)]
        enum #lexer_ident {
            #(#mode_idents),*
        }

        impl Default for #lexer_ident {
            fn default() -> Self {
                #lexer_ident::Default
            }
        }

        impl std::fmt::Display for #lexer_ident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    #(#lexer_ident::#mode_idents => write!(f, #modes)),*
                }
            }
        }

        impl parce::prelude::Lexer<#ident> for #lexer_ident {
            fn lex(&mut self, s: &str) -> Result<Vec<parce::prelude::Lexeme<#ident>>, parce::prelude::LexerError> {
                use parce::prelude::{Lexeme, LexerError};

                #(#pattern_matchers)*
                lazy_static! {
                    #(#statics)*
                }

                let mut result = vec![];
                let mut start = 0;
                while start < s.len() {
                    let mut longest: Option<(#ident, usize)> = None;

                    #(#non_fragment_checks)*

                    match longest {
                        Some((data, len)) if len > 0 => {
                            match data {
                                #(#no_skip)|* => result.push(
                                    Lexeme {
                                        data,
                                        start,
                                        len
                                    }
                                ),
                                _ => {}
                            }
                            start += len;
                        }
                        _ => return Err(LexerError {
                            full: s.to_string(),
                            start,
                            len: s.len() - start,
                            message: "no possible lexemes matched this input".to_string(),
                            mode: "default".to_string()
                        })
                    }
                }
                Ok(result)
            }
        }
    })
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

#[derive(Clone, Debug, Hash)]
enum LexDiscriminantRule {
    And(Vec<LexDiscriminantRule>),
    Or(Vec<LexDiscriminantRule>),
    Class(String),
    Literal(String),
    Lexeme(String),
    Dot,
    Star(Box<LexDiscriminantRule>),
    Plus(Box<LexDiscriminantRule>),
    Question(Box<LexDiscriminantRule>)
}

impl LexDiscriminantRule {
    fn to_matcher(&self) -> (TokenStream2, TokenStream2) {
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
                (quote! {
                    if s.len() >= start+#len && &s[start..start+#len] == #s {
                        Some(#len)
                    } else {
                        None
                    }
                }, quote! {})
            }
            Lexeme(l) => {
                let lexeme_fn_ident = format_ident!("{}", l.to_snake_case().into_safe());
                (quote! {
                    #lexeme_fn_ident(s, start)
                }, quote! {})
            }
            And(v) => {
                let matchers_and_statics: Vec<(TokenStream2, TokenStream2)> = v.iter().map(|rule| rule.to_matcher()).collect();
                let mut matchers = Vec::with_capacity(matchers_and_statics.len());
                let mut statics = Vec::with_capacity(matchers_and_statics.len());
                for (m, s) in matchers_and_statics {
                    matchers.push(m);
                    statics.push(s);
                }
                (quote! {
                    let old_start = start;
                    let result = loop {
                        #(
                            match {#matchers} {
                                Some(len) => start += len,
                                None => {
                                    break None;
                                }
                            }
                        )*
                        break Some(start - old_start);
                    };
                    start = old_start;
                    result
                }, quote! { #(#statics)* })
            }
            Or(v) => {
                let matchers_and_statics: Vec<(TokenStream2, TokenStream2)> = v.iter().map(|rule| rule.to_matcher()).collect();
                let mut matchers = Vec::with_capacity(matchers_and_statics.len());
                let mut statics = Vec::with_capacity(matchers_and_statics.len());
                for (m, s) in matchers_and_statics {
                    matchers.push(m);
                    statics.push(s);
                }
                (quote! {
                    let mut lengths = vec![];
                    #(
                        match {#matchers} {
                            Some(l) => lengths.push(l),
                            None => {}
                        }
                    )*
                    lengths.max()
                }, quote! { #(#statics)* })
            }
            Class(s) => {
                use std::collections::hash_map::DefaultHasher;
                use std::hash::{Hash, Hasher};

                let mut hasher = DefaultHasher::new();
                s.hash(&mut hasher);

                let static_ident = format_ident!("class_static_{}", hasher.finish());
                (quote! {
                    if s.len() > start && #static_ident.is_match(&s[start..start+1]) {
                        Some(1)
                    } else {
                        None
                    }
                }, quote! {
                    static ref #static_ident: regex::Regex = regex::Regex::new(#s)
                        .expect(&format!("{} is not a valid regex class", #s));
                })
            }
            Star(r) => {
                let (matcher, stat) = r.to_matcher();
                (quote! {
                    let old_start = start;
                    while let Some(len) = {#matcher} {
                        start += len;
                    }
                    let diff = start - old_start;
                    start = old_start;
                    Some(diff)
                }, stat)
            }
            Plus(r) => {
                let (matcher, stat) = r.to_matcher();
                (quote! {
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
                }, stat)
            }
            Question(r) => {
                let (matcher, stat) = r.to_matcher();
                (quote! {
                    if let Some(len) = {#matcher} {
                        Some(len)
                    } else {
                        None
                    }
                }, stat)
            }
            Dot => {
                (quote! {
                    if s.len() > start {
                        Some(1)
                    } else {
                        None
                    }
                }, quote! {})
            }
        }
    }
}

fn gen_matchers(s: String) -> Result<(TokenStream2, TokenStream2), LexerPatternParseError> {
    let rule = parse_rule(s)?;
    Ok(rule.to_matcher())
}

fn parse_rule(s: String) -> Result<LexDiscriminantRule, LexerPatternParseError> {
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
            options.push(parse_rule(s[splits[j+1] as usize..splits[j+1] as usize].to_string())?);
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
                    return Err(LexerPatternParseError(Box::new(s), "reached end of pattern before string was closed".to_string()));
                }
            }
            '[' => {
                let mut j = i + 1;
                let mut class_depth = 1;
                while j < s.len() {
                    match chars[j] {
                        '[' if chars[j-1] != '\\' => class_depth += 1,
                        ']' if chars[j-1] != '\\' => {
                            class_depth -= 1;
                            if class_depth == 0 {
                                break;
                            }
                        }
                        _ => {}
                    }
                    j += 1;
                }
                if j != s.len() {
                    result.push(LexDiscriminantRule::Class(s[i..j+1].to_string()));
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
                None => return Err(LexerPatternParseError(Box::new(s), "* was applied to nothing".to_string()))
            },
            '+' => match result.pop() {
                Some(l) => result.push(LexDiscriminantRule::Plus(Box::new(l))),
                None => return Err(LexerPatternParseError(Box::new(s), "+ was applied to nothing".to_string()))
            },
            '?' => match result.pop() {
                Some(l) => result.push(LexDiscriminantRule::Question(Box::new(l))),
                None => return Err(LexerPatternParseError(Box::new(s), "? was applied to nothing".to_string()))
            },
            c if c.is_whitespace() => {}
            c => return Err(LexerPatternParseError(Box::new(s), format!("{} is not a valid beginning to any lexer pattern", c)))
        }
        i += 1;
    }
    if result.len() == 0 {
        Err(LexerPatternParseError(Box::new(s), "this shouldn't be possible".to_string()))
    } else if result.len() == 1 {
        Ok(result.remove(0))
    } else {
        Ok(LexDiscriminantRule::And(result))
    }
}

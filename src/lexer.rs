use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, format_ident};
use proc_macro2::Ident;
use inflector::Inflector;
use check_keyword::CheckKeyword;
use std::collections::HashMap;
use crate::common::*;


#[derive(Debug)]
struct VariantInfo {
    modes: Vec<String>,
    ident: Ident,
    pattern: String,
    fragment: bool,
    skip: bool,
    set_mode: Option<String>
}

pub(crate) fn lexer(lexer_ident: Ident, mut input: syn::ItemEnum) -> Result<TokenStream2, ParceMacroError> {

    let modes = if let Some(idents) = get_ident_list("modes", &input.attrs) {
        if idents.len() < 2 {
            return Err(ParceMacroError(Box::new(input.clone()),"specify at least two modes, or delete the attribute for single-mode".to_string()))
        }
        idents
    } else {
        vec![String::from("Default")]
    };

    let mut variant_info = vec![];
    let mut current_modes = vec![modes.first().unwrap().clone()];
    for variant in &mut input.variants {
        let info = VariantInfo {
            modes: match get_ident_list("mode", &variant.attrs) {
                Some(m) => {
                    current_modes = m.clone();
                    for mode in &m {
                        if !modes.contains(mode) {
                            return Err(ParceMacroError(Box::new(variant.clone()), format!("mode {} was not declared", mode)));
                        }
                    }
                    m
                }
                None => current_modes.clone()
            },
            ident: variant.ident.clone(),
            pattern: get_pattern(&variant)?,
            fragment: has_attr("frag", &variant.attrs),
            skip: {
                let skip = get_attr_mut("skip", &mut variant.attrs);
                if let Some(attr) = skip {
                    attr.path.segments.first_mut().unwrap().ident = format_ident!("allow");
                    attr.tokens = quote! {(dead_code)};
                    true
                } else {
                    false
                }
            },
            set_mode: match get_ident_list("set_mode", &variant.attrs) {
                Some(mut list) => {
                    if list.len() != 1 {
                        return Err(ParceMacroError(Box::new(variant.clone()), "set_mode must have exactly one mode".to_string()));
                    }
                    let result = list.pop().unwrap();
                    if !modes.contains(&result) {
                        return Err(ParceMacroError(Box::new(variant.clone()), format!("mode {} was not declared", result)));
                    }
                    Some(result)
                }
                None => None
            }
        };
        variant_info.push(info);
        variant.discriminant = None;
    }

    let ident = input.ident.clone();
    let mode_idents: Vec<_> = modes.iter().map(|mode| format_ident!("{}", mode.to_class_case().into_safe())).collect();

    let mut pattern_matchers = vec![];
    let mut statics: Vec<TokenStream2> = vec![];
    let mut no_skip = vec![];
    let mut mode_setters = vec![];
    let mut mode_checks = HashMap::<String, TokenStream2>::new();
    for mode in &modes {
        mode_checks.insert(mode.clone(), quote! {});
    }
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
                fn #fn_ident(s: &str, mut start: usize) -> TinyVec<[usize;2]> {
                    #matcher
                }
            }
        );
        statics.push(stat);
        if !info.fragment {
            for mode in &info.modes {
                let acc = mode_checks[mode].clone();
                mode_checks.insert(mode.clone(), quote! {
                    #acc
                    for length in #fn_ident(s, start) {
                        match longest {
                            Some((lexeme, longest_len)) if longest_len < length => {
                                longest = Some((#ident::#lexeme_ident, length))
                            }
                            None => longest = Some((#ident::#lexeme_ident, length)),
                            _ => {}
                        }
                    }
                });
            }
        }
        if !info.skip {
            no_skip.push(quote! {#ident::#lexeme_ident});
        }
        if let Some(mode) = &info.set_mode {
            let mode_ident = format_ident!("{}", mode);
            mode_setters.push(
                quote! {
                    #ident::#lexeme_ident => *self = #lexer_ident::#mode_ident,
                }
            )
        }
    }

    let mut non_fragment_checks = vec![];
    for (key, value) in mode_checks {
        let mode_ident = format_ident!("{}", key);
        non_fragment_checks.push(quote! {
            #lexer_ident::#mode_ident => {
                #value
            }
        });
    }

    let default_mode = format_ident!("{}", modes.first().unwrap().clone());

    Ok(quote! {
        #[derive(parce_macros::RemoveLexerAttributes, Debug, Eq, PartialEq, Copy, Clone, Hash)]
        #[allow(dead_code)]
        #input

        #[derive(Debug, Eq, PartialEq, Copy, Clone)]
        enum #lexer_ident {
            #(#mode_idents),*
        }

        impl Default for #lexer_ident {
            fn default() -> Self {
                #lexer_ident::#default_mode
            }
        }

        impl std::fmt::Display for #lexer_ident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    #(#lexer_ident::#mode_idents => write!(f, #modes)),*
                }
            }
        }

        impl parce::reexports::Lexer for #lexer_ident {
            type Lexemes = #ident;

            fn lex(&mut self, s: &str) -> Result<Vec<parce::reexports::Lexeme<#ident>>, parce::ParceError> {
                use parce::reexports::*;
                use parce::{ParceError, ParcePhase};

                fn dedup_tiny(tiny: &mut TinyVec<[usize; 2]>) {
                    if let Some(mut i) = tiny.len().checked_sub(1) {
                        while i > 0 {
                            if tiny[i] == tiny[i - 1] {
                                tiny.remove(i);
                            }
                            i -= 1;
                        }
                    }
                }

                #(#pattern_matchers)*
                lazy_static! {
                    #(#statics)*
                }

                let mut result = vec![];
                let mut start = 0;
                while start < s.len() {
                    let mut longest: Option<(#ident, usize)> = None;

                    match self {
                        #(#non_fragment_checks)*
                    }

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
                            match data {
                                #(#mode_setters)*
                                _ => {}
                            }
                            start += len;
                        }
                        _ => return Err(ParceError {
                            input: s.to_string(),
                            start,
                            phase: ParcePhase::Lexer(self.to_string())
                        })
                    }
                }
                Ok(result)
            }
        }
    })
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
    Question(Box<LexDiscriminantRule>),
    Range(Box<LexDiscriminantRule>, usize, RangeRuleMax)
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
                        tiny_vec!([usize;2] => #len)
                    } else {
                        tiny_vec!([usize;2])
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

                // pay attention now
                let mut brute_force_it = quote! {
                    results.push(start - old_start);
                };
                for matcher in matchers.iter().rev() {
                    brute_force_it = quote! {
                        let lengths = {#matcher};
                        let old_start_2 = start;
                        for length in lengths {
                            start += length;
                            {#brute_force_it}
                            start = old_start_2;
                        }
                    }
                }
                (quote! {
                    let old_start = start;
                    let mut results = tiny_vec!([usize;2]);
                    {#brute_force_it}
                    results.sort_unstable();
                    dedup_tiny(&mut results);
                    start = old_start;
                    results
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
                    let mut results = tiny_vec!([usize;2]);
                    #(
                        results.extend({#matchers});
                    )*
                    results.sort_unstable();
                    dedup_tiny(&mut results);
                    results
                }, quote! { #(#statics)* })
            }
            Class(s) => {
                use std::collections::hash_map::DefaultHasher;
                use std::hash::{Hash, Hasher};

                let mut hasher = DefaultHasher::new();
                s.hash(&mut hasher);

                let static_ident = format_ident!("CLASS_STATIC_{}", hasher.finish());
                (quote! {
                    if s.len() > start && #static_ident.is_match(&s[start..start+1]) {
                        tiny_vec!([usize;2] => 1)
                    } else {
                        tiny_vec!([usize;2])
                    }
                }, quote! {
                    static ref #static_ident: regex::Regex = regex::Regex::new(#s)
                        .expect(&format!("{} is not a valid regex class", #s));
                })
            }
            Star(r) => {
                let (matcher, stat) = r.to_matcher();
                let infinite = match_infinite(matcher);
                (quote! {
                    let old_start = start;
                    let mut results = tiny_vec!([usize;2] => 0);
                    let mut i = 0;
                    #infinite;
                    start = old_start;
                    results
                }, stat)
            }
            Plus(r) => {
                let (matcher, stat) = r.to_matcher();
                let infinite = match_infinite(matcher);
                (quote! {
                    let old_start = start;
                    let mut results = tiny_vec!([usize;2]);
                    let mut i: i32 = -1;
                    #infinite
                    start = old_start;
                    results
                }, stat)
            }
            Range(r, min, max) => {
                let (matcher, stat) = r.to_matcher();
                let mut require_min = quote! {results.push(start - old_start)};
                for _ in 0..*min {
                    require_min = quote! {
                        let lengths = {#matcher};
                        for length in lengths {
                            let old_start_2 = start;
                            start += length;
                            {#require_min}
                            start = old_start_2;
                        }
                    }
                }
                let rest = match max {
                    RangeRuleMax::Some(max) => {
                        let mut allow_max = quote! {};
                        for _ in 0..(*max-*min) {
                            allow_max = quote! {
                                let lengths = {#matcher};
                                for length in lengths {
                                    let old_start_2 = start;
                                    start += length;
                                    results.push(start - old_start);
                                    {#allow_max}
                                    start = old_start_2;
                                }
                            }
                        }
                        quote! {
                            let results_length = results.len();
                            for i in 0..results_length {
                                start = old_start + results[i];
                                #allow_max
                            }
                            results.sort_unstable();
                            dedup_tiny(&mut results);
                        }
                    }
                    RangeRuleMax::Fixed => quote! {},
                    RangeRuleMax::Infinite => {
                        let infinite = match_infinite(matcher);
                        quote! {
                            if !results.is_empty() {
                                start += results[0];
                                let mut i = 0;
                                #infinite
                            }
                        }
                    },
                };
                (quote! {
                    let old_start = start;
                    let mut results = tiny_vec!([usize;2]);
                    #require_min
                    results.sort_unstable();
                    dedup_tiny(&mut results);
                    #rest
                    start = old_start;
                    results
                }, stat)
            }
            Question(r) => {
                let (matcher, stat) = r.to_matcher();
                (quote! {
                    let mut results = {#matcher};
                    results.insert(0, 0);
                    dedup_tiny(&mut results);
                    results
                }, stat)
            }
            Dot => {
                (quote! {
                    if s.len() > start {
                        tiny_vec!([usize;2] => 1)
                    } else {
                        tiny_vec!([usize;2])
                    }
                }, quote! {})
            }
        }
    }
}

fn match_infinite(matcher: TokenStream2) -> TokenStream2 {
    quote! {
        loop {
            let mut lengths = {#matcher};
            if !lengths.is_empty() {
                for length in &mut lengths {
                    *length += start - old_start;
                }
                results.extend(lengths);
                &results[(i+1) as usize ..].sort_unstable();
                dedup_tiny(&mut results);
            }
            i += 1;
            if i as usize == results.len() {
                break;
            }
            start = old_start + results[i as usize];
        }
    }
}

fn gen_matchers(s: String) -> Result<(TokenStream2, TokenStream2), ParceMacroError> {
    let rule = parse_rule(s)?;
    Ok(rule.to_matcher())
}


fn parse_rule(s: String) -> Result<LexDiscriminantRule, ParceMacroError> {
    let chars: Vec<char> = s.chars().collect();

    // Search for |
    let mut i = 0;
    let mut in_string = false;
    let mut group_depth = 0;
    let mut class_depth = 0;
    let mut splits = vec![-1_i32];
    while i < s.len() {
        match chars[i] {
            '(' if !in_string && class_depth == 0 => group_depth += 1,
            ')' if !in_string && class_depth == 0 => group_depth -= 1,
            '[' if !in_string && (i == 0 || chars[i-1] != '\\' || class_depth == 0) => class_depth += 1,
            ']' if !in_string && chars[i-1] != '\\' => class_depth -= 1,
            '\'' if (!in_string && class_depth == 0) || (in_string && chars[i-1] != '\\') => {
                in_string = !in_string;
            }
            '|' if !in_string && class_depth == 0 && group_depth == 0 => {
                splits.push(i as i32);
            }
            _ => {}
        }
        i += 1;
    }
    if splits.len() > 1 {
        splits.push(s.len() as i32);
        let mut options = vec![];
        for j in 0..splits.len()-1 {
            options.push(parse_rule(s[(splits[j]+1) as usize..splits[j+1] as usize].to_string())?);
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
                    return Err(ParceMacroError(Box::new(s), "reached end of pattern before string was closed".to_string()));
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
            '(' => {
                let mut j = i + 1;
                let mut class_depth: u32 = 0;
                let mut group_depth: u32 = 1;
                let mut in_string = false;
                while j < s.len() {
                    match chars[j] {
                        '(' if !in_string && class_depth == 0 => group_depth += 1,
                        ')' if !in_string && class_depth == 0 => {
                            group_depth -= 1;
                            if group_depth == 0 {
                                break;
                            }
                        }
                        '[' if !in_string && (chars[j-1] != '\\' || class_depth == 0) => class_depth += 1,
                        ']' if !in_string && chars[j-1] != '\\' => class_depth -= 1,
                        '\'' if (!in_string && class_depth == 0) || (in_string && chars[j-1] != '\\') => {
                            in_string = !in_string;
                        }
                        _ => {}
                    }
                    j += 1;
                }
                if j != s.len() {
                    result.push(parse_rule(s[i+1..j].to_string())?);
                    i = j;
                } else {
                    return Err(ParceMacroError(Box::new(s), "reached end of string before () group was closed".to_string()));
                }
            }
            '{' => {
                match result.pop() {
                    Some(prev) => {
                        let mut j = i + 1;
                        while j < s.len() {
                            match chars[j] {
                                '}' => break,
                                _ => j += 1
                            }
                        }
                        if j != s.len() {
                            let captures = COUNT_PARSER.captures(&s[i..j + 1]);
                            match captures {
                                Some(cap) => {
                                    result.push(
                                        LexDiscriminantRule::Range(
                                            Box::new(prev),
                                            cap[1].parse().unwrap(),
                                            match cap.get(2) {
                                                Some(s) => RangeRuleMax::Some(s.as_str().parse().unwrap()),
                                                None => {
                                                    if s[i..j+1].contains(',') {
                                                        RangeRuleMax::Infinite
                                                    } else {
                                                        RangeRuleMax::Fixed
                                                    }
                                                }
                                            }
                                        )
                                    );
                                }
                                None => return Err(ParceMacroError(Box::new(s), "invalid counter operator".to_string()))
                            }
                            i = j;
                        }
                    }
                    None => return Err(ParceMacroError(Box::new(s), "{} was applied to nothing".to_string()))
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
                None => return Err(ParceMacroError(Box::new(s), "* was applied to nothing".to_string()))
            },
            '+' => match result.pop() {
                Some(l) => result.push(LexDiscriminantRule::Plus(Box::new(l))),
                None => return Err(ParceMacroError(Box::new(s), "+ was applied to nothing".to_string()))
            },
            '?' => match result.pop() {
                Some(l) => result.push(LexDiscriminantRule::Question(Box::new(l))),
                None => return Err(ParceMacroError(Box::new(s), "? was applied to nothing".to_string()))
            },
            c if c.is_whitespace() => {}
            c => return Err(ParceMacroError(Box::new(s), format!("{} is not a valid beginning to any lexer pattern", c)))
        }
        i += 1;
    }
    if result.len() == 0 {
        Err(ParceMacroError(Box::new(s), "this shouldn't be possible".to_string()))
    } else if result.len() == 1 {
        Ok(result.remove(0))
    } else {
        Ok(LexDiscriminantRule::And(result))
    }
}

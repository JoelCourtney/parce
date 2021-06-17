use proc_macro2::TokenStream as TokenStream2;
use proc_macro2::Ident;
use super::ParceMacroError;
use crate::common::*;
use quote::{quote, format_ident};
use syn::Path;

struct VariantInfo {
    ident: Ident,
    pattern: ParseDiscriminantRule,
    fields: VariantFields
}

enum VariantFields {
    Unit,
    Unnamed(Vec<syn::Type>),
    Named(Vec<(Ident, syn::Type)>)
}

impl VariantFields {
    fn search_named(&self, name: &String) -> Result<syn::Type, ParceMacroError> {
        return match self {
            VariantFields::Named(v) => {
                for (id, ty) in v {
                    if id.to_string() == name.to_string() {
                        return Ok(ty.clone())
                    }
                }
                Err(ParceMacroError(Box::new(name.clone()), format!("field {} was not found in variant", name)))
            }
            _ => Err(ParceMacroError(Box::new(name.clone()), format!("variant does not have named fields")))
        }
    }
}

pub(crate) fn parser(lexer: syn::Path, mut input: syn::ItemEnum) -> Result<TokenStream2, ParceMacroError> {
    let mut variants = vec![];
    let mut other_rules: Vec<String> = vec![];
    for variant in &mut input.variants {
        variants.push(
            VariantInfo {
                pattern: parse_rule(get_pattern(&variant)?)?,
                ident: variant.ident.clone(),
                fields: match variant.fields.clone() {
                    syn::Fields::Unnamed(syn::FieldsUnnamed {unnamed, ..}) => {
                        VariantFields::Unnamed(unnamed.iter().map(|field| field.ty.clone()).collect())
                    }
                    syn::Fields::Named(syn::FieldsNamed {named, ..}) => {
                        VariantFields::Named(named.iter().map(|field| (field.ident.clone().unwrap(), field.ty.clone())).collect())
                    }
                    syn::Fields::Unit => VariantFields::Unit
                }
            }
        );
        variant.discriminant = None;
    }
    other_rules.sort_unstable();
    other_rules.dedup();

    let enum_ident = input.ident.clone();
    let num_productions = variants.len();
    let num_prod_index = syn::Index::from(num_productions);
    let other_rule_paths: Vec<syn::Path> = other_rules.iter().map(|rule| syn::parse_str(rule).unwrap()).collect();

    let mut route_matchers = vec![];
    let mut next_route = num_productions;
    for (i,variant) in variants.into_iter().enumerate() {
        let MatcherOutput {
            main_route,
            extra_routes,
            ..
        } = variant.pattern.to_matchers(&enum_ident, &lexer, &variant, 0, next_route, EndBehavior::Last)?;

        let iu32 = syn::Index::from(i);
        route_matchers.push(quote! {
            #iu32 => match state {
                #main_route
                other => panic!("state {} out of bounds", other)
            }
        });

        route_matchers.extend(
            extra_routes.iter().map(|extra_route| {
                let next_u32 = syn::Index::from(next_route);
                let result = quote! {
                    #next_u32 => match state {
                        #extra_route
                        other => panic!("state {} out of bounds", other)
                    }
                };
                next_route += 1;
                result
            })
        );
    }

    Ok(quote! {
        #[derive(Debug, Eq, PartialEq)]
        #[allow(dead_code)]
        #input

        impl Parser for #enum_ident {
            type Lexemes = <#lexer as Lexer>::Lexemes;
            const PRODUCTIONS: u32 = #num_prod_index;

            fn default_lexer() -> Box<dyn Lexer<Lexemes = <#lexer as Lexer>::Lexemes>> {
                Box::new(#lexer::default())
            }
            fn commands(rule: Rule, route: u32, state: u32, lexeme: Lexeme<<#lexer as Lexer>::Lexemes>) -> parce::reexports::ArrayVec<[AutomatonCommand; 2]> {
                use parce::reexports::*;
                use AutomatonCommand::*;

                if rule == Rule::of::<#enum_ident>() {
                    match route {
                        #(#route_matchers)*
                        other => panic!("route {} out of bounds", other)
                    }
                } #(else if rule == Rule::of::<#other_rule_paths>() {
                    <#other_rule_paths as Parser>::commands(rule, route, state, lexeme)
                })* else {
                    panic!("shouldn't be able to get here")
                }
            }
            fn assemble(_auto: *mut Automaton) -> Self {
                Self::Thing
            }
        }
    })
}

#[derive(Debug)]
enum ParseDiscriminantRule {
    /// if match and last, Victory(Die)
    /// if match and cyclic, Victory(Reset(#))
    /// if match, Advance
    /// if not match, die
    Lexeme(String),

    /// recruit.
    /// if last on Victory die
    /// if
    Rule(String),
    BareUnnamedField(usize),
    AssignUnnamedField(usize, Box<ParseDiscriminantRule>),
    BareNamedField(String),
    AssignNamedField(String, Box<ParseDiscriminantRule>),
    And(Vec<ParseDiscriminantRule>),
    Or(Vec<ParseDiscriminantRule>),
    Dot,

    ///
    Star(Box<ParseDiscriminantRule>),
    Plus(Box<ParseDiscriminantRule>),
    Question(Box<ParseDiscriminantRule>),
    Range(Box<ParseDiscriminantRule>, usize, RangeRuleMax),
}

struct MatcherOutput {
    main_route: TokenStream2,
    main_states: usize,
    extra_routes: Vec<TokenStream2>,
}

enum EndBehavior {
    Last,
    NotLast,
    Reset(u32)
}

impl ParseDiscriminantRule {
    // pay attention now
    fn to_matchers(
        &self,
        grammar: &Ident,
        lexer: &Path,
        info: &VariantInfo,
        first_state: usize,
        next_route: usize,
        end_behavior: EndBehavior
    ) -> Result<MatcherOutput, ParceMacroError> {
        use ParseDiscriminantRule::*;
        use EndBehavior::*;

        let first_u32 = syn::Index::from(first_state);
        let next_u32 = syn::Index::from(next_route);

        Ok(match self {
            Lexeme(name) => {
                let ident = format_ident!("{}", name);
                let success = match end_behavior {
                    Last => quote! { Victory, Die },
                    NotLast => quote! { Advance },
                    Reset(n) => quote! { Victory, Reset(#n) }
                };
                MatcherOutput {
                    main_route: quote! {
                        #first_u32 => if lexeme == <#lexer as Lexer>::Lexemes::#ident {
                            array_vec!([AutomatonCommand;2] => #success)
                        } else {
                            array_vec!([AutomatonCommand;2] => Die)
                        },
                    },
                    main_states: 1,
                    extra_routes: vec![]
                }
            }
            Rule(name) => {
                let r = format_ident!("{}", name);
                let on_victory = match end_behavior {
                    Last => quote! { Continuation::Die },
                    NotLast => quote! { Continuation::Advance },
                    Reset(n) => quote! { Continuation::Reset(#n) }
                };
                MatcherOutput {
                    main_route: quote! {
                        #first_u32 => array_vec!([AutomatonCommand;2] => Recruit {
                            rule: Rule::of::<#r>(),
                            production: 0_u32,
                            how_many: <#r as Parser>::PRODUCTIONS,
                            on_victory: #on_victory
                        }, Die),
                    },
                    main_states: 1,
                    extra_routes: vec![]
                }
            }
            BareUnnamedField(n) => {
                let r = match info.fields {
                    VariantFields::Unnamed(ref v) => {
                        match v.get(*n) {
                            Some(t) => t,
                            None => return Err(ParceMacroError(Box::new(info.ident.clone()), format!("production has fewer than {} fields", n)))
                        }
                    }
                    _ => return Err(ParceMacroError(Box::new(info.ident.clone()), "variant does not have unnamed fields".to_string()))
                };
                let on_victory = match end_behavior {
                    Last => quote! { Continuation::Die },
                    NotLast => quote! { Continuation::Advance },
                    Reset(n) => quote! { Continuation::Reset(#n) }
                };
                MatcherOutput {
                    main_route: quote! {
                        #first_u32 => array_vec!([AutomatonCommand;2] => Recruit {
                            rule: Rule::of::<#r>(),
                            production: 0_u32,
                            how_many: <#r as Parser>::PRODUCTIONS,
                            on_victory: #on_victory
                        }, Die),
                    },
                    main_states: 1,
                    extra_routes: vec![]
                }
            }
            BareNamedField(id) => {
                let ty = info.fields.search_named(id)?;
                let on_victory = match end_behavior {
                    Last => quote! { Continuation::Die },
                    NotLast => quote! { Continuation::Advance },
                    Reset(n) => quote! { Continuation::Reset(#n) }
                };
                MatcherOutput {
                    main_route: quote! {
                        #first_u32 => array_vec!([AutomatonCommand;2] => Recruit {
                            rule: Rule::of::<#ty>(),
                            production: 0_u32,
                            how_many: <#ty as Parser>::PRODUCTIONS,
                            on_victory: #on_victory
                        }, Die),
                    },
                    main_states: 1,
                    extra_routes: vec![]
                }
            }
            // AssignUnnamedField(n, rule) => {
            //
            // }
            And(rules) => {
                let mut next_route = next_route;
                let mut extra_routes = vec![];
                let mut state = first_state;
                let mut main_route = quote! {};
                for rule in &rules[..rules.len()-1] {
                    let output = rule.to_matchers(grammar, lexer, info, state, next_route, EndBehavior::NotLast)?;
                    next_route += output.extra_routes.len();
                    state += output.main_states;
                    extra_routes.extend(output.extra_routes);
                    let next_matcher = output.main_route;
                    main_route = quote! {
                        #main_route
                        #next_matcher
                    };
                }
                let output = rules.last().unwrap().to_matchers(grammar, lexer, info, state, next_route, end_behavior)?;
                extra_routes.extend(output.extra_routes);
                state += output.main_states;
                let next_matcher = output.main_route;
                main_route = quote! {
                    #main_route
                    #next_matcher
                };
                MatcherOutput {
                    main_route,
                    main_states: state - first_state,
                    extra_routes
                }
            }
            Or(rules) => {
                let on_victory = match end_behavior {
                    Last => quote! { Continuation::Die },
                    NotLast => quote! { Continuation::Advance },
                    Reset(n) => quote! { Continuation::Reset(#n) }
                };
                let rules_len = syn::Index::from(rules.len());
                let mut main_routes = vec![];
                let mut extra_routes = vec![];
                let mut next_extra_route = next_route + rules.len();
                for rule in rules {
                    let output = rule.to_matchers(grammar, lexer, info, 0, next_extra_route, EndBehavior::Last)?;
                    next_extra_route += extra_routes.len();
                    main_routes.push(output.main_route);
                    extra_routes.extend(output.extra_routes);
                }
                main_routes.extend(extra_routes);
                MatcherOutput {
                    main_route: quote! {
                        #first_u32 => array_vec!([AutomatonCommand;2] => Recruit {
                            rule: Rule::of::<#grammar>(),
                            route: #next_u32,
                            how_many: #rules_len,
                            on_victory: #on_victory
                        }, Die),
                    },
                    main_states: 1,
                    extra_routes: main_routes
                }
            }
            Star(rule) => {
                let matcher_output = rule.to_matchers(grammar, lexer, info, 0, next_route + 1, EndBehavior::Reset(0))?;
                let now = match end_behavior {
                    Last => quote! { RecruitNow::VictoryDie },
                    NotLast => quote! { RecruitNow::Advance },
                    Reset(n) => quote! { RecruitNow::VictoryReset(#n) }
                };
                let on_victory = match end_behavior {
                    Last => quote! { Continuation::Die },
                    NotLast => quote! { Continuation::Advance },
                    Reset(n) => quote! { Continuation::Reset(#n) }
                };
                let mut extra_routes = vec![matcher_output.main_route];
                extra_routes.extend(matcher_output.extra_routes);
                MatcherOutput {
                    main_route: quote! {
                        #first_u32 => array_vec!([AutomatonCommand;2] => Recruit {
                            rule: Rule::of::<#grammar>(),
                            route: #next_u32,
                            how_many: 1,
                            now: #now,
                            on_victory: #on_victory
                        }, #now),
                    },
                    main_states: 1,
                    extra_routes
                }
            }
            Dot => {
                let success = match end_behavior {
                    Last => quote! { Victory, Die },
                    NotLast => quote! { Advance },
                    Reset(n) => quote! { Victory, Reset(#n) }
                };
                MatcherOutput {
                    main_route: quote! {
                        #first_u32 => array_vec!([AutomatonCommand;2] => #success)
                    },
                    main_states: 1,
                    extra_routes: vec![]
                }
            }
            _ => todo!()
        })
    }
}

fn parse_rule(s: String) -> Result<ParseDiscriminantRule, ParceMacroError> {
    let chars: Vec<char> = s.chars().collect();

    let mut i = 0;
    let mut splits = vec![-1];
    let mut group_depth = 0;
    while i < s.len() {
        match chars[i] {
            '(' => group_depth += 1,
            ')' => group_depth -= 1,
            '|' if group_depth == 0 => {
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
        return Ok(ParseDiscriminantRule::Or(options));
    }

    let mut result = vec![];
    i = 0;
    while i < s.len() {
        match chars[i] {
            '#' => {
                let mut j = i + 1;
                while j < s.len() && (chars[j].is_alphanumeric() || chars[j] == ':') {
                    j += 1;
                }
                result.push(ParseDiscriminantRule::Rule(s[i+1..j].to_string()));
                i = j - 1;
            }
            '(' => {
                let mut j = i + 1;
                let mut group_depth: u32 = 1;
                while j < s.len() {
                    match chars[j] {
                        '(' => group_depth += 1,
                        ')' => {
                            group_depth -= 1;
                            if group_depth == 0 {
                                break;
                            }
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
            c if c.is_alphabetic() => {
                let mut j = i + 1;
                while j < s.len() && chars[j].is_alphanumeric() {
                    j += 1;
                }
                let name = s[i..j].to_string();
                if c.is_uppercase() {
                    result.push(ParseDiscriminantRule::Lexeme(name));
                    i = j - 1;
                } else {
                    if j < s.len() && chars[j] == '=' {
                        let mut k = j + 1;
                        let mut group_depth: u32 = 0;
                        while k < s.len() {
                            match chars[k] {
                                '(' => group_depth += 1,
                                ')' => group_depth -= 1,
                                c if c.is_whitespace() && group_depth == 0 => break,
                                _ => {}
                            }
                            k += 1;
                        }
                        result.push(ParseDiscriminantRule::AssignNamedField(name, Box::new(parse_rule(s[j+1..k].to_string())?)));
                        i = k - 1;
                    } else {
                        result.push(ParseDiscriminantRule::BareNamedField(name));
                        i = j - 1;
                    }
                }
            }
            c if c.is_numeric() => {
                let mut j = i + 1;
                while j < s.len() && chars[j].is_numeric() {
                    j += 1;
                }
                let name = match s[i..j].parse() {
                    Ok(n) => n,
                    Err(_) => panic!("how even")
                };
                if j < s.len() && chars[j] == '=' {
                    let mut k = j + 1;
                    let mut group_depth: u32 = 0;
                    while k < s.len() {
                        match chars[k] {
                            '(' => group_depth += 1,
                            ')' => group_depth -= 1,
                            c if c.is_whitespace() && group_depth == 0 => break,
                            _ => {}
                        }
                        k += 1;
                    }
                    result.push(ParseDiscriminantRule::AssignUnnamedField(name, Box::new(parse_rule(s[j+1..k].to_string())?)));
                    i = k - 1;
                } else {
                    result.push(ParseDiscriminantRule::BareUnnamedField(name));
                    i = j - 1;
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
                                        ParseDiscriminantRule::Range(
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
            '.' => result.push(ParseDiscriminantRule::Dot),
            '*' => match result.pop() {
                Some(l) => result.push(ParseDiscriminantRule::Star(Box::new(l))),
                None => return Err(ParceMacroError(Box::new(s), "* was applied to nothing".to_string()))
            },
            '+' => match result.pop() {
                Some(l) => result.push(ParseDiscriminantRule::Plus(Box::new(l))),
                None => return Err(ParceMacroError(Box::new(s), "+ was applied to nothing".to_string()))
            },
            '?' => match result.pop() {
                Some(l) => result.push(ParseDiscriminantRule::Question(Box::new(l))),
                None => return Err(ParceMacroError(Box::new(s), "? was applied to nothing".to_string()))
            },
            c if c.is_whitespace() => {}
            c => return Err(ParceMacroError(Box::new(s), format!("{} is not a valid beginning to any parser pattern", c)))
        }
        i += 1;
    }
    if result.len() == 0 {
        Err(ParceMacroError(Box::new(s), "this shouldn't be possible".to_string()))
    } else if result.len() == 1 {
        Ok(result.remove(0))
    } else {
        Ok(ParseDiscriminantRule::And(result))
    }
}

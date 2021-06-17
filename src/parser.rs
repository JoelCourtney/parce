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
    let mut route_assemblers = vec![];
    let mut next_route = num_productions;
    for (i,variant) in variants.into_iter().enumerate() {
        let MatcherOutput {
            main_route,
            extra_routes,
            assembler,
            produced,
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
            extra_routes.iter().map(|(extra_route, cycle)| {
                let next_u32 = syn::Index::from(next_route);
                let modulus = match cycle {
                    Some(n) => {
                        let index = syn::Index::from(*n);
                        quote! { % #index }
                    },
                    None => quote! {}
                };
                let result = quote! {
                    #next_u32 => match state #modulus {
                        #extra_route
                        other => panic!("state {} out of bounds", other)
                    }
                };
                next_route += 1;
                result
            })
        );

        route_assemblers.push({
            let ident = variant.ident.clone();
            // TODO check fields are correct
            match ident.fields {
                VariantFields::Unit => quote! {
                    { #assembler }
                    Self::#ident
                },
                VariantFields::Unnamed(fields) => quote! {
                    let (#(#produced),*) = { #assembler };
                    Self::#ident(#(#produced),*)
                }
                VariantFields::Named(_) => quote! {
                    #iu32 => {
                        let (#(#produced),*) = { #assembler };
                        Self::ident { #(#produced,*) }
                    }
                }
            }
        });
    }

    Ok(quote! {
        #[derive(Debug, Eq, PartialEq)]
        #[allow(dead_code)]
        #input

        impl parce::reexports::Parser for #enum_ident {
            type Lexemes = <#lexer as Lexer>::Lexemes;
            const PRODUCTIONS: u32 = #num_prod_index;

            fn default_lexer() -> Box<dyn Lexer<Lexemes = <#lexer as Lexer>::Lexemes>> {
                Box::new(#lexer::default())
            }
            fn commands(rule: parce::reexports::Rule, route: u32, state: u32, lexeme: parce::reexports::Lexeme<<#lexer as Lexer>::Lexemes>) -> parce::reexports::ArrayVec<[parce::reexports::AutomatonCommand; 2]> {
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
            fn assemble(auto: parce::reexports::Rawtomaton, lexemes: &Vec<parce::reexports::Lexeme<Self::Lexemes>>, text: &str) -> (usize, Self) {
                use parce::reexports::*;

                unsafe {
                    let rule = (**auto).rule;
                    if rule == Rule::of::<#enum_ident>() {
                        let mut consumed = 0;
                        let mut recruits = 0;
                        Ok((match (**auto).route {
                            #(#route_assemblers)*
                            other => panic!("route {} out of bounds, shouldn't be possible", other)
                        }, consumed))
                    } #(else if rule == Rule::of::<#other_rule_paths>() {
                        <#other_rule_paths as Parser>::assemble(auto, lexemes, text)
                    })* else {
                        panic!("shouldn't be able to get here")
                    }
                }
            }
        }
    })
}

#[derive(Debug)]
enum ParseDiscriminantRule {
    Lexeme(String),
    Rule(String),
    BareUnnamedField(usize),
    AssignUnnamedField(usize, Box<ParseDiscriminantRule>),
    BareNamedField(String),
    AssignNamedField(String, Box<ParseDiscriminantRule>),
    And(Vec<ParseDiscriminantRule>),
    Or(Vec<ParseDiscriminantRule>),
    Dot,
    Star(Box<ParseDiscriminantRule>),
    Plus(Box<ParseDiscriminantRule>),
    Question(Box<ParseDiscriminantRule>),
    Range(Box<ParseDiscriminantRule>, usize, RangeRuleMax),
}

struct MatcherOutput {
    main_route: TokenStream2,
    main_states: usize,
    extra_routes: Vec<(TokenStream2, Option<usize>)>,
    assembler: TokenStream2,
    produced: Vec<Ident>,
}

#[derive(Debug, Eq, PartialEq)]
enum EndBehavior {
    Last,
    NotLast,
    Reset
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
        end_behavior: EndBehavior,
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
                    Reset => quote! { Victory }
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
                    extra_routes: vec![],
                    assembler: quote! { consumed += 1; },
                    produced: vec![],
                }
            }
            Rule(name) => {
                let r = format_ident!("{}", name);
                let on_victory = match end_behavior {
                    Last => quote! { Continuation::PassDie },
                    NotLast => quote! { Continuation::Advance },
                    Reset => quote! { Continuation::PassAdvance }
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
                    extra_routes: vec![],
                    assembler: quote! {
                        let (more_consumed, _) = #r::assemble((**auto).children[#recruit_index], &lexemes[consumed..], text);
                        consumed += more_consumed;
                        recruits += 1;
                    },
                    produced: vec![],
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
                let ident = format_ident!("unnamed_field_{}", syn::Index::from(n));
                let on_victory = match end_behavior {
                    Last => quote! { Continuation::PassDie },
                    NotLast => quote! { Continuation::Advance },
                    Reset => quote! { Continuation::PassAdvance }
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
                    extra_routes: vec![],
                    assembler: quote! {
                        let (more_consumed, #ident) = #r::assemble((**auto).children[recruits], &lexemes[consumed..], text);
                        consumed += more_consumed;
                        recruits += 1;
                        (#ident,)
                    },
                    produced: vec![ident],
                }
            }
            BareNamedField(id) => {
                let ty = info.fields.search_named(id)?;
                let on_victory = match end_behavior {
                    Last => quote! { Continuation::PassDie },
                    NotLast => quote! { Continuation::Advance },
                    Reset => quote! { Continuation::PassAdvance }
                };
                let ident = format_ident!("{}", id);
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
                    extra_routes: vec![],
                    assembler: quote! {
                        let (more_consumed, #ident) = #r::assemble((**auto).children[recruits], &lexemes[consumed..], text);
                        consumed += more_consumed;
                        recruits += 1;
                        (#ident,)
                    },
                    produced: vec![ident],
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
                let mut produced = vec![vec![]];
                let mut assemblers = vec![];
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
                    assemblers.push(output.assembler);
                    produced.push(output.produced);
                }

                let output = rules.last().unwrap().to_matchers(grammar, lexer, info, state, next_route, end_behavior)?;
                extra_routes.extend(output.extra_routes);
                state += output.main_states;
                let next_matcher = output.main_route;
                main_route = quote! {
                    #main_route
                    #next_matcher
                };
                assemblers.push(output.assembler);
                produced.push(output.produced);
                MatcherOutput {
                    main_route,
                    main_states: state - first_state,
                    extra_routes,
                    assembler: quote! {
                        #(let #(#produced,)* = { #assemblers };)* // TODO will be an error when nothing is produced from assembler
                        (#(#(produced,)*)*)
                    },
                    produced: produced.iter().flatten().collect(),
                }
            }
            Or(rules) => {
                let on_victory = match end_behavior {
                    Last => quote! { Continuation::PassDie },
                    NotLast => quote! { Continuation::Advance },
                    Reset => quote! { Continuation::PassAdvance }
                };
                let rules_len = syn::Index::from(rules.len());
                let mut main_routes = vec![];
                let mut extra_routes = vec![];
                let mut next_extra_route = next_route + rules.len();
                let mut assemblers = vec![];
                let mut produced = vec![];
                for rule in rules {
                    let output = rule.to_matchers(grammar, lexer, info, 0, next_extra_route, EndBehavior::Last)?;
                    next_extra_route += output.extra_routes.len();
                    if end_behavior == Reset {
                        main_routes.push((output.main_route, Some(output.main_states)));
                    } else {
                        main_routes.push((output.main_route, None));
                    }
                    extra_routes.extend(output.extra_routes);
                    assemblers.push(output.assembler);
                    produced = output.produced;
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
                    extra_routes: main_routes,
                    assembler: quote! {
                        let recruit = (**auto).children[recruits];
                        match (**recruit).route {

                        }
                    },
                    produced,
                }
            }
            Star(rule) => {
                let matcher_output = rule.to_matchers(grammar, lexer, info, 0, next_route + 1, EndBehavior::Reset)?;
                let now = match end_behavior {
                    Last => quote! { Victory, Die },
                    NotLast => quote! { Advance },
                    Reset => quote! { Victory, Advance }
                };
                let on_victory = match end_behavior {
                    Last => quote! { Continuation::PassDie },
                    NotLast => quote! { Continuation::Advance },
                    Reset => quote! { Continuation::PassAdvance }
                };
                let mut extra_routes = vec![(matcher_output.main_route, Some(matcher_output.main_states))];
                extra_routes.extend(matcher_output.extra_routes);
                MatcherOutput {
                    main_route: quote! {
                        #first_u32 => array_vec!([AutomatonCommand;2] => Recruit {
                            rule: Rule::of::<#grammar>(),
                            route: #next_u32,
                            how_many: 1,
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
                    Reset => quote! { Victory, Advance }
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

    fn to_assembler(&self, info: &VariantInfo, next_route: u32, recruit_index: u32) -> Result<AssemblerOutput, ParceMacroError> {
        use ParseDiscriminantRule::*;

        Ok(match self {
            Lexeme(l) => AssemblerOutput::consume(1),
            Rule(name) => {
                let rule = format_ident!("{}", name);
                AssemblerOutput::rule(rule.into(), false, recruit_index)
            }
            BareUnnamedField(n) => {
                let rule = match info.fields {
                    VariantFields::Unnamed(ref v) => {
                        match v.get(*n) {
                            Some(t) => t,
                            None => return Err(ParceMacroError(Box::new(info.ident.clone()), format!("production has fewer than {} fields", n)))
                        }
                    }
                    _ => return Err(ParceMacroError(Box::new(info.ident.clone()), "variant does not have unnamed fields".to_string()))
                };
                let ident = format_ident!("unnamed_field_{}", syn::Index::from(n));
                AssemblerOutput::rule(rule.into(), Some(ident), recruit_index)
            },
            BareNamedField(s) => {
                let rule = info.fields.search_named(s)?;
                let ident = format_ident!("{}", s);
                AssemblerOutput::rule(rule.into(), Some(ident), recruit_index);
            }
            And(v) => {
                let mut produced = vec![];
                let mut recruits = 0;
                let mut assemblers = vec![];
                for part in v {
                    let output = part.to_assembler(info, recruit_index + recruits)?;
                    let new_produced = output.produced;
                    let new_assembler = output.assembler;
                    recruits += output.recruits;
                    assemblers.push(quote! {
                        let (more_consumed, #(#new_produced,)*) = { #new_assembler };
                        consumed += more_consumed;
                    });
                    produced.extend(new_produced);
                }
                AssemblerOutput {
                    assembler: quote! {
                        let mut consumed = 0;
                        #(#assemblers)*
                        (consumed, #(#produced),*)
                    },
                    produced,
                    recruits
                }
            }
            Or(v) => {
                let
                AssemblerOutput {

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
                    if j < s.len() - 1 && chars[j] == '=' {
                        let mut k = j + 2;
                        match chars[j + 1] {
                            c if c.is_alphabetic() || c == '#' => {
                                while k < s.len() {
                                    if chars[k].is_alphabetic() {
                                        k += 1;
                                    } else {
                                        break
                                    }
                                }
                            }
                            '(' => {
                                let mut group_depth = 1;
                                while k < s.len() {
                                    match chars[k] {
                                        '(' => group_depth += 1,
                                        ')' => {
                                            group_depth -= 1;
                                            if group_depth == 0 {
                                                break;
                                            }
                                        }
                                        _ => {}
                                    }
                                    k += 1;
                                }
                            }
                            other => return Err(ParceMacroError(Box::new(s.clone()), format!("'{}' is not valid after =", other)))
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

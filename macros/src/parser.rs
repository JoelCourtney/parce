use proc_macro2::TokenStream as TokenStream2;
use proc_macro2::Ident;
use super::ParceMacroError;
use crate::common::*;
use quote::{quote, format_ident};
use syn::Path;
use std::iter::FromIterator;
use crate::common::RangeRuleMax;
use crate::discriminants::parser_pattern;

struct VariantInfo {
    ident: Ident,
    pattern: ParserPattern,
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
    for variant in &mut input.variants {
        variants.push(
            VariantInfo {
                pattern: parser_pattern(get_pattern(&variant)?)?,
                ident: variant.ident.clone(),
                fields: match variant.fields.clone() {
                    syn::Fields::Unnamed(syn::FieldsUnnamed {unnamed, ..}) => {
                        let mut fields = Vec::with_capacity(unnamed.len());
                        for field in unnamed {
                            fields.push(unwrap_type(field.ty.clone())?);
                        }
                        VariantFields::Unnamed(fields)
                    }
                    syn::Fields::Named(syn::FieldsNamed {named, ..}) => {
                        let mut fields = Vec::with_capacity(named.len());
                        for field in named {
                            fields.push((field.ident.clone().unwrap(), unwrap_type(field.ty.clone())?));
                        }
                        VariantFields::Named(fields)
                    }
                    syn::Fields::Unit => VariantFields::Unit
                }
            }
        );
        variant.discriminant = None;
    }

    let enum_ident = input.ident.clone();
    let num_productions = variants.len();
    let num_prod_index = syn::Index::from(num_productions);

    let mut route_matchers = vec![];
    let mut end_route_matchers = vec![];
    let mut route_assemblers = vec![];
    let mut next_route = num_productions;
    for (i,variant) in variants.into_iter().enumerate() {
        let MatcherOutput {
            main_route,
            end_route,
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

        end_route_matchers.push(quote! {
            #iu32 => match state {
                #end_route
                other => panic!("state {} out of bounds", other)
            }
        });

        for (extra_route, extra_end_route, cycle) in extra_routes {
            let next_u32 = syn::Index::from(next_route);
            let modulus = match cycle {
                Some(n) => {
                    let index = syn::Index::from(n);
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
            let end_result = quote! {
                #next_u32 => match state #modulus {
                    #extra_end_route
                    other => panic!("state {} out of bounds", other)
                }
            };
            next_route += 1;
            route_matchers.push(result);
            end_route_matchers.push(end_result);
        }

        route_assemblers.push({
            let ident = variant.ident.clone();
            // TODO check fields are correct
            match variant.fields {
                VariantFields::Unit => quote! {
                    #iu32 => {
                        { #assembler }
                        Self::#ident
                    }
                },
                VariantFields::Unnamed(_fields) => quote! {
                    #iu32 => {
                        let (#(#produced,)*) = { #assembler };
                        Self::#ident(#(#produced),*)
                    }
                },
                VariantFields::Named(_) => quote! {
                    #iu32 => {
                        let (#(#produced,)*) = { #assembler };
                        Self::#ident { #(#produced),* }
                    }
                }
            }
        });
    }

    let mut parser_submission = lexer.clone();
    let last_ident = parser_submission.segments.last().unwrap().ident.clone();
    parser_submission.segments.last_mut().unwrap().ident = format_ident!("{}ParserSubmission", last_ident);

    Ok(quote! {
        #[derive(Debug, PartialEq)]
        #input

        parce::internal_prelude::inventory::submit! {
            #parser_submission(
                core::any::TypeId::of::<#enum_ident>(),
                |route: u32, mut state: u32, lexeme: parce::internal_prelude::SpannedLexeme<<#lexer as parce::internal_prelude::Lexer>::Lexemes>| -> parce::internal_prelude::ArrayVec<[parce::internal_prelude::AutomatonCommand; 3]> {
                    use parce::internal_prelude::*;
                    use AutomatonCommand::*;

                    match route {
                        #(#route_matchers)*
                        other => panic!("route {} out of bounds", other)
                    }
                },
                |route: u32, mut state: u32| -> bool {
                    use parce::internal_prelude::*;
                    use AutomatonCommand::*;

                    match route {
                        #(#end_route_matchers)*
                        other => panic!("route {} out of bounds", other)
                    }
                }
            )
        }

        impl parce::internal_prelude::Parseable for #enum_ident {
            type Lexer = #lexer;
            const PRODUCTIONS: u32 = #num_prod_index;

            fn default_lexer() -> Box<Self::Lexer> {
                Box::new(#lexer::default())
            }
            fn commands(rule: parce::internal_prelude::Rule, route: u32, mut state: u32, lexeme: parce::internal_prelude::SpannedLexeme<<#lexer as parce::internal_prelude::Lexer>::Lexemes>) -> parce::internal_prelude::ArrayVec<[parce::internal_prelude::AutomatonCommand; 3]> {
                use parce::internal_prelude::*;
                use AutomatonCommand::*;

                dbg!((route, state, lexeme));

                if rule == Rule::of::<#enum_ident>() {
                    let result = match route {
                        #(#route_matchers)*
                        other => panic!("route {} out of bounds", other)
                    };
                    dbg!(result)
                } else {
                    for submission in inventory::iter::<#parser_submission> {
                        if rule == submission.0 {
                            return submission.1(route, state, lexeme);
                        }
                    }
                    panic!("rule number {:?} not found", rule);
                }
            }
            fn last_commands(rule: parce::internal_prelude::Rule, route: u32, mut state: u32) -> bool {
                use parce::internal_prelude::*;
                use AutomatonCommand::*;

                dbg!((route, state));

                if rule == Rule::of::<#enum_ident>() {
                    let result = match route {
                        #(#end_route_matchers)*
                        other => panic!("route {} out of bounds", other)
                    };
                    dbg!(result)
                } else {
                    for submission in inventory::iter::<#parser_submission> {
                        if rule == submission.0 {
                            return submission.2(route, state);
                        }
                    }
                    panic!("rule number {:?} not found", rule);
                }
            }
            fn assemble(auto: parce::internal_prelude::Rawtomaton, lexemes: &[parce::internal_prelude::SpannedLexeme<<#lexer as parce::internal_prelude::Lexer>::Lexemes>], text: &str) -> Result<(usize, Self), parce::error::ParceError> {
                use parce::internal_prelude::*;

                unsafe {
                    let rule = (**auto).rule;
                    if rule == Rule::of::<#enum_ident>() {
                        let mut consumed = 0;
                        let mut recruits = 0;
                        let result = match (**auto).route {
                            #(#route_assemblers)*
                            other => panic!("route {} out of bounds, shouldn't be possible", other)
                        };
                        Ok((consumed, result))
                    } else {
                        unreachable!()
                    }
                }
            }
        }

        impl std::str::FromStr for #enum_ident {
            type Err = parce::error::ParceError;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                use parce::parser::Parse;
                s.parse_all()
            }
        }
    })
}

fn unwrap_type(mut ty: syn::Type) -> Result<syn::Type, ParceMacroError> {
    while let syn::Type::Path(syn::TypePath {ref path, ..}) = ty {
        if let Some(seg) = path.segments.first() {
            let id = seg.ident.clone();
            if id.to_string() == "Vec" || id.to_string() == "Option" || id.to_string() == "Box" {
                ty = match &path.segments.first().unwrap().arguments {
                    syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { args, .. }) => {
                        if args.len() == 1 {
                            match args.first().unwrap() {
                                syn::GenericArgument::Type(new_ty) => new_ty.clone(),
                                _ => return Err(ParceMacroError(Box::new(ty), "only generic type arguments are allowed".to_string()))
                            }
                        } else {
                            return Err(ParceMacroError(Box::new(ty), "must have exactly one arg".to_string()));
                        }
                    }
                    _ => return Err(ParceMacroError(Box::new(ty), "must be a single angle bracketed generic argument".to_string()))
                };
            } else {
                break;
            }
        } else {
            break;
        }
    }
    Ok(ty)
}

#[derive(Debug)]
pub(crate) enum ParserPattern {
    Lexeme(String),
    Rule(String),
    BareUnnamedField(usize),
    AssignUnnamedField(usize, Box<ParserPattern>),
    BareNamedField(String),
    AssignNamedField(String, Box<ParserPattern>),
    And(Vec<ParserPattern>),
    Or(Vec<ParserPattern>),
    Dot,
    Star(Box<ParserPattern>),
    Plus(Box<ParserPattern>),
    Question(Box<ParserPattern>),
    Range(Box<ParserPattern>, usize, RangeRuleMax),
}

struct MatcherOutput {
    main_route: TokenStream2,
    states: usize,
    extra_routes: Vec<(TokenStream2, TokenStream2, Option<usize>)>,
    end_route: TokenStream2,
    assembler: TokenStream2,
    produced: Vec<Ident>,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum EndBehavior {
    Last,
    NotLast,
    Reset
}

impl ParserPattern {
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
        use ParserPattern::*;
        use EndBehavior::*;

        let first_state_u32 = syn::Index::from(first_state);
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
                        #first_state_u32 => if lexeme == <#lexer as parce::internal_prelude::Lexer>::Lexemes::#ident {
                            array_vec!([AutomatonCommand; 3] => #success)
                        } else {
                            array_vec!([AutomatonCommand; 3] => Die)
                        },
                    },
                    states: 1,
                    extra_routes: vec![],
                    end_route: quote! {
                        #first_state_u32 => false,
                    },
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
                        #first_state_u32 => array_vec!([AutomatonCommand; 3] => Spawn {
                            rule: Rule::of::<#r>(),
                            route: 0,
                            how_many: <#r as Parseable>::PRODUCTIONS,
                            on_victory: #on_victory
                        }, Die),
                    },
                    states: 1,
                    extra_routes: vec![],
                    end_route: quote! {
                        #first_state_u32 => false,
                    },
                    assembler: quote! {
                        let (more_consumed, _) = #r::assemble((**auto).children[recruits], lexemes, text)?;
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
                let ident = format_ident!("unnamed_field_{}", syn::Index::from(*n));
                let on_victory = match end_behavior {
                    Last => quote! { Continuation::PassDie },
                    NotLast => quote! { Continuation::Advance },
                    Reset => quote! { Continuation::PassAdvance }
                };
                MatcherOutput {
                    main_route: quote! {
                        #first_state_u32 => array_vec!([AutomatonCommand; 3] => Spawn {
                            rule: Rule::of::<#r>(),
                            route: 0,
                            how_many: <#r as Parseable>::PRODUCTIONS,
                            on_victory: #on_victory
                        }, Die),
                    },
                    states: 1,
                    extra_routes: vec![],
                    end_route: quote! {
                        #first_state_u32 => false,
                    },
                    assembler: quote! {
                        let (more_consumed, #ident) = #r::assemble((**auto).children[recruits], lexemes, text)?;
                        consumed += more_consumed;
                        recruits += 1;
                        (#ident.into(),)
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
                        #first_state_u32 => array_vec!([AutomatonCommand; 3] => Spawn {
                            rule: Rule::of::<#ty>(),
                            route: 0,
                            how_many: <#ty as Parseable>::PRODUCTIONS,
                            on_victory: #on_victory
                        }, Die),
                    },
                    states: 1,
                    extra_routes: vec![],
                    end_route: quote! {
                        #first_state_u32 => false,
                    },
                    assembler: quote! {
                        let (more_consumed, #ident) = #ty::assemble((**auto).children[recruits], lexemes, text)?;
                        consumed += more_consumed;
                        recruits += 1;
                        (#ident.into(),)
                    },
                    produced: vec![ident],
                }
            }
            AssignUnnamedField(n, rule) => {
                let output = rule.to_matchers(grammar, lexer, info, first_state, next_route, end_behavior)?;
                let extra_produced = output.produced;
                let ident = format_ident!("unnamed_field_{}", syn::Index::from(*n));
                let mut produced = extra_produced.clone();
                produced.insert(0, ident);
                let assembler = output.assembler;
                let assign = if extra_produced.len() == 0 {
                    quote! {
                        { #assembler }
                    }
                } else {
                    quote! {
                        let (#(#extra_produced,)*) = { #assembler };
                    }
                };
                MatcherOutput {
                    assembler: quote! {
                        let start = dbg!(lexemes[consumed].start);
                        #assign
                        let end = lexemes[consumed-1].start + lexemes[consumed-1].len;
                        (
                            match text[start..end].parse() {
                                Ok(res) => res,
                                Err(e) => return Err(parce::error::ParceError {
                                    input: text.to_string(),
                                    start,
                                    info: parce::error::ParceErrorInfo::Assemble
                                })
                            }, #(#extra_produced),*)
                    },
                    produced,
                    ..output
                }
            }
            AssignNamedField(s, rule) => {
                let output = rule.to_matchers(grammar, lexer, info, first_state, next_route, end_behavior)?;
                let extra_produced = output.produced;
                let ident = format_ident!("{}", s);
                let mut produced = extra_produced.clone();
                produced.insert(0, ident);
                let assembler = output.assembler;
                let assign = if extra_produced.len() == 0 {
                    quote! {
                        { #assembler }
                    }
                } else {
                    quote! {
                        let (#(#extra_produced,)*) = { #assembler };
                    }
                };
                MatcherOutput {
                    assembler: quote! {
                        let start = dbg!(lexemes[consumed].start);
                        #assign
                        let end = lexemes[consumed-1].start + lexemes[consumed-1].len;
                        (
                            match text[start..end].parse() {
                                Ok(res) => res,
                                Err(e) => return Err(parce::error::ParceError {
                                    input: text.to_string(),
                                    start,
                                    info: parce::error::ParceErrorInfo::Assemble
                                })
                            }, #(#extra_produced),*)
                    },
                    produced,
                    ..output
                }
            }
            And(rules) => {
                let mut next_route = next_route;
                let mut extra_routes = vec![];
                let mut end_route = quote! {};
                let mut state = first_state;
                let mut main_route = quote! {};
                let mut produced = vec![vec![]];
                let mut assemblers = vec![];
                for (i, rule) in rules.iter().enumerate() {
                    let output = rule.to_matchers(
                        grammar, lexer,
                        info,
                        state, next_route,
                        if i == rules.len() - 1 { end_behavior } else { EndBehavior::NotLast }
                    )?;
                    next_route += output.extra_routes.len();
                    state += output.states;
                    extra_routes.extend(output.extra_routes);
                    let next_matcher = output.main_route;
                    main_route = quote! {
                        #main_route
                        #next_matcher
                    };
                    let next_end_matcher = output.end_route;
                    end_route = quote! {
                        #end_route
                        #next_end_matcher
                    };
                    let new_assembler = output.assembler;
                    let new_produced = output.produced;
                    assemblers.push(if new_produced.len() == 0 {
                        quote! {
                            { #new_assembler }
                        }
                    } else {
                        quote! {
                            let (#(#new_produced,)*) = { #new_assembler };
                        }
                    });
                    produced.push(new_produced);
                }

                MatcherOutput {
                    main_route,
                    states: state - first_state,
                    extra_routes,
                    end_route,
                    assembler: quote! {
                        #(#assemblers)*
                        (#(#(#produced,)*)*)
                    },
                    produced: produced.into_iter().flatten().collect(),
                }
            }
            Or(rules) => {
                let on_victory = match end_behavior {
                    Last => quote! { Continuation::PassDie },
                    NotLast => quote! { Continuation::Advance },
                    Reset => quote! { Continuation::PassAdvance }
                };
                let rules_len = syn::Index::from(rules.len());
                let mut routes = vec![];
                let mut extra_routes = vec![];
                let mut next_extra_route = next_route + rules.len();
                let mut assemblers = vec![];
                let mut produced = std::collections::HashSet::<Ident>::new();
                for (i, rule) in rules.iter().enumerate() {
                    let output = rule.to_matchers(grammar, lexer, info, 0, next_extra_route, EndBehavior::Last)?;
                    next_extra_route += output.extra_routes.len();
                    if end_behavior == Reset {
                        routes.push((output.main_route, output.end_route, Some(output.states)));
                    } else {
                        routes.push((output.main_route, output.end_route, None));
                    }
                    extra_routes.extend(output.extra_routes);
                    let new_assembler = output.assembler;
                    let route_number = syn::Index::from(next_route + i);
                    assemblers.push(quote! {
                        #route_number => {
                            let mut recruits = 0;
                            #new_assembler
                        }
                    });
                    if i == 0 {
                        produced = std::collections::HashSet::from_iter(output.produced.into_iter());
                    } else {
                        let set = std::collections::HashSet::<Ident>::from_iter(output.produced.into_iter());
                        if set != produced {
                            return Err(ParceMacroError(Box::new(info.ident.clone()), "not all possibilites in this pattern assign to the same fields in the enum variant".to_string()));
                        }
                    }
                }
                routes.extend(extra_routes);
                MatcherOutput {
                    main_route: quote! {
                        #first_state_u32 => array_vec!([AutomatonCommand; 3] => Spawn {
                            rule: Rule::of::<#grammar>(),
                            route: #next_u32,
                            how_many: #rules_len,
                            on_victory: #on_victory
                        }, Die),
                    },
                    states: 1,
                    extra_routes: routes,
                    end_route: quote! {
                        #first_state_u32 => false,
                    },
                    assembler: quote! {
                        let auto = (**auto).children[recruits];
                        match (**auto).route {
                            #(#assemblers)*
                            other => panic!("route {} out of bounds, this is an internal error", other)
                        }
                        recruits += 1;
                    },
                    produced: produced.into_iter().collect(),
                }
            }
            Star(rule) => repetition_operator(rule, RepetitionOperator::Star, grammar, lexer, info, first_state, next_route, end_behavior)?,
            Question(rule) => repetition_operator(rule, RepetitionOperator::Question, grammar, lexer, info, first_state, next_route, end_behavior)?,
            Plus(rule) => repetition_operator(rule, RepetitionOperator::Plus, grammar, lexer, info, first_state, next_route, end_behavior)?,
            Range(rule, start, max) => repetition_operator(rule, RepetitionOperator::Range(*start, *max), grammar, lexer, info, first_state, next_route, end_behavior)?,
            Dot => {
                let success = match end_behavior {
                    Last => quote! { Victory, Die },
                    NotLast => quote! { Advance },
                    Reset => quote! { Victory }
                };
                MatcherOutput {
                    main_route: quote! {
                        #first_state_u32 => array_vec!([AutomatonCommand; 3] => #success),
                    },
                    states: 1,
                    extra_routes: vec![],
                    end_route: quote! {
                        #first_state_u32 => false,
                    },
                    assembler: quote! {
                        consumed += 1;
                    },
                    produced: vec![]
                }
            }
        })
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum RepetitionOperator {
    Star,
    Question,
    Plus,
    Range(usize, RangeRuleMax)
}

fn repetition_operator(rule: &Box<ParserPattern>, op: RepetitionOperator, grammar: &Ident, lexer: &Path, info: &VariantInfo, first_state: usize, next_route: usize, end_behavior: EndBehavior) -> Result<MatcherOutput, ParceMacroError> {
    use RepetitionOperator::*;

    let first_state_u32 = syn::Index::from(first_state);
    let second_state_u32 = syn::Index::from(first_state + 1);

    let next_route_u32 = syn::Index::from(next_route);
    let second_route_u32 = syn::Index::from(next_route + 1);

    let (cycle_length, mut outputs) = match op {
        Star | Plus => {
            let output = rule.to_matchers(grammar, lexer, info, 0, next_route + 1, EndBehavior::Reset)?;
            (output.states, vec![output])
        },
        Question => {
            let output = rule.to_matchers(grammar, lexer, info, 0, next_route + 1, EndBehavior::Last)?;
            (output.states, vec![output])
        },
        Range(start, RangeRuleMax::Fixed) => {
            let mut state = 0;
            let mut main_route = quote! {};
            let mut end_route = quote! {};
            for _ in 0..(start - 1) {
                let output = rule.to_matchers(
                    grammar, lexer,
                    info,
                    state, next_route + 1,
                    EndBehavior::NotLast
                )?;
                state += output.states;
                let next_matcher = output.main_route;
                let next_end_matcher = output.end_route;
                main_route = quote! {
                    #main_route
                    #next_matcher
                };
                end_route = quote! {
                    #end_route
                    #next_end_matcher
                }
            }
            let output = rule.to_matchers(
                grammar, lexer,
                info,
                state, next_route + 1,
                EndBehavior::Last
            )?;
            let next_matcher = output.main_route;
            let next_end_matcher = output.end_route;
            (output.states, vec![MatcherOutput {
                main_route: quote! {
                    #main_route
                    #next_matcher
                },
                end_route: quote! {
                    #end_route
                    #next_end_matcher
                },
                states: output.states * start,
                ..output
            }])
        }
        Range(start, max) => {
            let mut state = 0;
            let mut main_route1 = quote! {};
            let mut end_route1 = quote! {};
            for _ in 0..(start - 2) {
                let output = rule.to_matchers(
                    grammar, lexer,
                    info,
                    state, next_route + 2,
                    EndBehavior::NotLast
                )?;
                state += output.states;
                let next_matcher = output.main_route;
                main_route1 = quote! {
                    #main_route1
                    #next_matcher
                };
                let next_end_matcher = output.end_route;
                end_route1 = quote! {
                    #end_route1
                    #next_end_matcher
                };
            }
            let output = rule.to_matchers(
                grammar, lexer,
                info,
                state, next_route + 2,
                EndBehavior::Last
            )?;
            let next_matcher = output.main_route;
            let next_end_matcher = output.end_route;
            let cycle_length = output.states;
            let mut outputs = vec![MatcherOutput {
                main_route: quote! {
                    #main_route1
                    #next_matcher
                },
                end_route: quote! {
                    #end_route1
                    #next_end_matcher
                },
                states: output.states * start,
                ..output
            }];
            state = 0;
            let mut main_route2 = quote! {};
            let mut end_route2 = quote! {};
            if let RangeRuleMax::Some(max) = max {
                for _ in start..max {
                    let output = rule.to_matchers(
                        grammar, lexer,
                        info,
                        state, next_route + 2,
                        EndBehavior::Reset
                    )?;
                    state += output.states;
                    let next_matcher = output.main_route;
                    main_route2 = quote! {
                        #main_route2
                        #next_matcher
                    };
                    let next_end_matcher = output.end_route;
                    end_route2 = quote! {
                        #end_route2
                        #next_end_matcher
                    };
                }
                let output = rule.to_matchers(
                    grammar, lexer,
                    info,
                    state, next_route + 2,
                    EndBehavior::Last
                )?;
                let next_matcher = output.main_route;
                let next_end_matcher = output.end_route;
                outputs.push(MatcherOutput {
                    main_route: quote! {
                        #main_route2
                        #next_matcher
                    },
                    states: output.states * start,
                    end_route: quote! {
                        #end_route2
                        #next_end_matcher
                    },
                    ..output
                });
            } else {
                outputs.push(rule.to_matchers(
                    grammar, lexer,
                    info,
                    state, next_route + 2,
                    EndBehavior::Reset
                )?);
            }
            (cycle_length, outputs)
        }
    };

    let cycle_length_u32 = syn::Index::from(cycle_length);

    let produced = outputs.get(0).unwrap().produced.clone();
    let produced_temps: Vec<_> = produced.iter().map(|id| format_ident!("{}_temp", id.to_string())).collect();
    let interior_assembler = outputs.get(0).unwrap().assembler.clone();

    let (init, receiver, assign) = match (op, produced.is_empty()) {
        (_, true) => (quote! {}, quote! { { #interior_assembler } }, quote! {}),
        (Question, _) => (quote! { #(let mut #produced = None;)* }, quote! { let (#(#produced_temps,)*) = { #interior_assembler }; }, quote! { #(#produced = Some(#produced_temps);)* }),
        (_,_) => (quote! { #(let mut #produced = Vec::with_capacity(((**auto).state / #cycle_length_u32) as usize);)* },
                  quote! { let (#(#produced_temps,)*) = { #interior_assembler }; },
                  quote! { #(#produced.push(#produced_temps);)* })
    };

    let now = match (op, end_behavior) {
        (Range(_,_) | Plus, _) => quote! { Die },
        (_, EndBehavior::Last) => quote! { Victory, Die },
        (_, EndBehavior::NotLast) => quote! { Advance, Fallthrough },
        (_, EndBehavior::Reset) => quote! { Victory, Fallthrough }
    };
    let on_victory = match end_behavior {
        EndBehavior::Last => quote! { Continuation::PassDie },
        EndBehavior::NotLast => quote! { Continuation::Advance },
        EndBehavior::Reset => quote! { Continuation::PassAdvance }
    };

    let returns = if produced.is_empty() {
        quote! {}
    } else {
        quote! { (#(#produced,)*) }
    };
    let states = match op {
        Star | Question | Plus | Range(_, RangeRuleMax::Fixed) => 1,
        _ => 2
    };

    let (extra_routes, assembler) = match op {
        Star | Question => {
            let output = outputs.remove(0);
            let mut extra = vec![(output.main_route, output.end_route, Some(cycle_length))];
            extra.extend(output.extra_routes);
            (extra, quote! {
                #init
                if recruits < (**auto).children.len() {
                    let auto = (**auto).children[recruits];
                    if (**auto).route == #next_route_u32 && (**auto).lexeme_start == consumed {
                        {
                            let mut recruits = 0;
                            for _ in 0..((**auto).state / #cycle_length_u32) {
                                #receiver
                                #assign
                            }
                        }
                        recruits += 1;
                    }
                }
                #returns
            })
        }
        o@(Plus | Range(_, RangeRuleMax::Fixed)) => {
            let output = outputs.remove(0);
            let mut extra = vec![(output.main_route, output.end_route, if o == Plus { Some(cycle_length) } else { None })];
            extra.extend(output.extra_routes);
            (extra, quote! {
                #init
                {
                    let auto = (**auto).children[recruits];
                    let mut recruits = 0;
                    for _ in 0..((**auto).state / #cycle_length_u32) {
                        #receiver
                        #assign
                    }
                }
                recruits += 1;
                #returns
            })
        }
        Range(_, max) => {
            let output1 = outputs.remove(0);
            let output2 = outputs.remove(0);
            let mut extra = vec![(output1.main_route, output1.end_route, None), (output2.main_route, output2.end_route, if max == RangeRuleMax::Infinite { Some(cycle_length) } else { None })];
            extra.extend(output1.extra_routes);
            extra.extend(output2.extra_routes);
            (extra, quote! {
                #init
                for _ in 0..2 {
                    {
                        let auto = (**auto).children[recruits];
                        let mut recruits = 0;
                        for _ in 0..((**auto).state / #cycle_length_u32) {
                            #receiver
                            #assign
                        }
                    }
                    recruits += 1;
                }
                #returns
            })
        }
    };


    Ok(MatcherOutput {
        main_route: match op {
            Star | Question | Plus | Range(_, RangeRuleMax::Fixed) => {
                quote! {
                    #first_state_u32 => {
                        array_vec!([AutomatonCommand; 3] =>
                            Spawn {
                                rule: Rule::of::<#grammar>(),
                                route: #next_route_u32,
                                how_many: 1,
                                on_victory: #on_victory
                            },
                            #now
                        )
                    }
                }
            }
            _ => {
                quote! {
                    #first_state_u32 => {
                        array_vec!([AutomatonCommand; 3] =>
                            Spawn {
                                rule: Rule::of::<#grammar>(),
                                route: #next_route_u32,
                                how_many: 1,
                                on_victory: Continuation::Advance
                            },
                            Die
                        )
                    }
                    #second_state_u32 => {
                        array_vec!([AutomatonCommand; 3] =>
                            Spawn {
                                rule: Rule::of::<#grammar>(),
                                route: #second_route_u32,
                                how_many: 1,
                                on_victory: #on_victory
                            },
                            Die
                        )
                    }
                }
            }
        },
        states,
        extra_routes,
        end_route: match op {
            Star | Question => quote! {
                #first_state_u32 => true,
            },
            Plus | Range(_, RangeRuleMax::Fixed) => quote! {
                #first_state_u32 => false,
            },
            Range(_,_) => quote! {
                #first_state_u32 => false,
                #second_state_u32 => false,
            }
        },
        assembler,
        produced
    })
}


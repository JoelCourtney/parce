mod automata;

use crate::lexer::{Lexeme, Lexer};
use core::any::TypeId as Rule;
use automata::*;
use tinyvec::TinyVec;

#[derive(Debug, Eq, PartialEq)]
pub struct ParserError;

pub trait Parse<I, O: Sized> {
    fn parse(input: I) -> Result<O, ParserError>;
}

pub trait Parser {
    type Lexemes: Copy + Eq;

    fn default_lexer() -> Box<dyn Lexer<Lexemes = Self::Lexemes>>;
    fn starting_productions() -> u32;
    fn command(rule: Rule, production: u32, state: u32, lexeme: Lexeme<Self::Lexemes>) -> TinyVec<[AutomatonCommand; 4]>;
    fn assemble(auto: *mut Automaton) -> Self where Self: Sized;
}

impl<L: Copy + Eq, C: 'static + Sized + Parser<Lexemes = L>> Parse<&Vec<Lexeme<L>>, C> for C {
    fn parse(lexemes: &Vec<Lexeme<L>>) -> Result<Self, ParserError> {
        let army: Army = Army::new();
        let mut alive: Vec<Rawtomaton>= vec![];

        for i in 0..Self::starting_productions() {
            alive.push(army.recruit(Rule::of::<C>(), i).into());
        }

        let mut last = None;

        let mut i = 0;
        while !alive.is_empty() {
            let mut j = alive.len();
            while j > 0 {
                j -= 1;
                let auto = alive[j];
                unsafe {
                    let actions = Self::command((**auto).rule, (**auto).production, (**auto).state, lexemes[i]);
                    let (new, victory, remove) = army.act(auto, actions);
                    alive.extend(new);
                    if victory {
                        last = Some(auto);
                    }
                    if remove {
                        alive.remove(j);
                    }
                }
            }
            i += 1;
        }

        if let Some(l) = last {
            Ok(Self::assemble(*l))
        } else {
            Err(ParserError)
        }
    }
}

impl<C: 'static + Sized + Parser> Parse<&str, C> for C {
    fn parse(input: &str) -> Result<Self, ParserError> {
        match Self::default_lexer().lex(input) {
            Ok(lexemes) => Self::parse(&lexemes),
            Err(_) => Err(ParserError)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::prelude::*;
    use crate as parce;
    use tinyvec::{TinyVec, tiny_vec};

    #[lexer(MyLexer)]
    enum MyLexeme {
        A = "'a'",
        B = "'b'"
    }

    #[derive(Debug, Eq, PartialEq)]
    enum MyGrammar {
        Thing // "A (B | B B) A
    }

    #[test]
    fn parse() {
        let now = std::time::Instant::now();
        for _ in 0..10000 {
            assert_eq!(MyGrammar::parse(&vec![
                Lexeme {
                    data: MyLexeme::A,
                    start: 0,
                    len: 1
                },
                Lexeme {
                    data: MyLexeme::B,
                    start: 1,
                    len: 1
                },
                Lexeme {
                    data: MyLexeme::B,
                    start: 2,
                    len: 1
                },
                Lexeme {
                    data: MyLexeme::A,
                    start: 3,
                    len: 1
                },
            ]), Ok(MyGrammar::Thing));
        }
        println!("{}", now.elapsed().as_micros());
    }

    impl Parser for MyGrammar {
        type Lexemes = MyLexeme;

        fn default_lexer() -> Box<dyn Lexer<Lexemes = MyLexeme>> {
            Box::new(MyLexer::default())
        }
        fn starting_productions() -> u32 { 1 }
        fn command(rule: Rule, production: u32, state: u32, lexeme: Lexeme<MyLexeme>) -> TinyVec<[AutomatonCommand; 4]> {
            use AutomatonCommand::*;

            if rule == Rule::of::<MyGrammar>() {
                match production {
                    0 => {
                        match state {
                            0 => if lexeme == MyLexeme::A {
                                tiny_vec!(
                                    [AutomatonCommand; 4] =>
                                    Recruit(Rule::of::<MyGrammar>(), 1),
                                    RecruitDie(Rule::of::<MyGrammar>(), 2)
                                )
                            } else {
                                tiny_vec!([AutomatonCommand; 4] => Die)
                            }
                            1 => if lexeme == MyLexeme::A {
                                tiny_vec!([AutomatonCommand; 4] => VictoryDie)
                            } else {
                                tiny_vec!([AutomatonCommand; 4] => Die)
                            }
                            _ => panic!("only two states")
                        }
                    }
                    1 => {
                        match state {
                            0 => if lexeme == MyLexeme::B {
                                tiny_vec!([AutomatonCommand; 4] => VictoryDie)
                            } else {
                                tiny_vec!([AutomatonCommand; 4] => Die)
                            }
                            _ => panic!("only 1 state")
                        }
                    }
                    2 => {
                        match state {
                            0 => if lexeme == MyLexeme::B {
                                tiny_vec!([AutomatonCommand; 4] => Advance)
                            } else {
                                tiny_vec!([AutomatonCommand; 4] => Die)
                            }
                            1 => if lexeme == MyLexeme::B {
                                tiny_vec!([AutomatonCommand; 4] => VictoryDie)
                            } else {
                                tiny_vec!([AutomatonCommand; 4] => Die)
                            }
                            _ => panic!("only 2 states")
                        }
                    }
                    _ => panic!("only 3 productions")
                }
            } else {
                panic!("only one rule")
            }
        }

        fn assemble(_auto: *mut Automaton) -> Self {
            Self::Thing
        }
    }
}
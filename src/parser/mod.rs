pub mod automata;

use crate::lexer::{Lexeme, Lexer};
use core::any::TypeId as Rule;
use automata::*;
use tinyvec::ArrayVec;
use std::collections::VecDeque;
use crate::{ParceError, ParcePhase};

pub trait Parce<O: Parser>: ToString  {
    fn parce(&self) -> Result<O, ParceError>;
}

pub trait Parser: 'static + Sized {
    type Lexemes: Copy + Eq;
    const PRODUCTIONS: u32;

    fn default_lexer() -> Box<dyn Lexer<Lexemes = Self::Lexemes>>;
    fn commands(rule: Rule, route: u32, state: u32, lexeme: Lexeme<Self::Lexemes>) -> ArrayVec<[AutomatonCommand; 2]>;
    fn assemble(auto: Rawtomaton, lexemes: &[Lexeme<Self::Lexemes>], text: &str) -> (usize, Self);
}

impl<I: ToString, O: Parser> Parce<O> for I {
    fn parce(&self) -> Result<O, ParceError> {
        let text = self.to_string();
        let lexemes = O::default_lexer().lex(&text)?;

        let army: Army = Army::new();
        let mut alive: VecDeque<Rawtomaton> = VecDeque::new();

        for i in 0..O::PRODUCTIONS {
            alive.push_back(army.recruit(Rule::of::<O>(), i).into());
        }

        let mut last = None;

        let mut i = 0;
        while !alive.is_empty() {
            let mut j = 0;
            while j < alive.len() {
                let auto = alive[j];
                unsafe {
                    let commands = O::commands((**auto).rule, (**auto).route, (**auto).state, lexemes[i]);
                    let result = army.command(auto, commands);
                    alive.extend(result.new_recruits);
                    j += result.reactivated.len();
                    for old in result.reactivated {
                        alive.push_front(old);
                    }
                    if let Some(vic) = result.victorious {
                        last = Some(vic);
                    }
                    if result.remove {
                        alive.remove(j);
                    } else {
                        j += 1;
                    }
                }
            }
            i += 1;
        }

        if let Some(l) = last {
            let (consumed, result) = O::assemble(l, lexemes.as_slice(), &text);
            result
        } else {
            Err(ParceError {
                input: text,
                start: lexemes[i].start + lexemes[i].len,
                phase: ParcePhase::Parse
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use crate as parce;
    use parce::*;

    #[lexer(MyLexer)]
    enum MyLexeme {
        A = "'a'",
        B = "'b'"
    }

    #[parser(MyLexer)]
    enum MyGrammar {
        Thing = "A (B | B B) A"
    }

    // #[parser(HellOLexer)]
    // enum Hello {
    //     One = " #Thing C* "
    // }

    #[test]
    fn parse() {
        use crate::parser::Parce;
        let now = std::time::Instant::now();
        for _ in 0..1000 {
            assert_eq!("abba".parce(), Ok(MyGrammar::Thing));
        }
        println!("{}", now.elapsed().as_micros());
    }

    // impl Parser for MyGrammar {
    //     type Lexemes = <MyLexer as Lexer>::Lexemes;
    //     const PRODUCTIONS: u32 = 1;
    //
    //     fn default_lexer() -> Box<dyn Lexer<Lexemes = <MyLexer as Lexer>::Lexemes>> {
    //         Box::new(MyLexer::default())
    //     }
    //     fn commands(rule: Rule, route: u32, state: u32, lexeme: Lexeme<<MyLexer as Lexer>::Lexemes>) -> ArrayVec<[AutomatonCommand; 2]> {
    //         use AutomatonCommand::*;
    //
    //         // println!("");
    //         // dbg!((route, state, lexeme));
    //
    //         let result = if rule == Rule::of::<MyGrammar>() {
    //             match route {
    //                 0 => {
    //                     match state {
    //                         0 => if lexeme == <MyLexer as Lexer>::Lexemes::A {
    //                             array_vec!([AutomatonCommand;2] => Advance)
    //                         } else {
    //                             array_vec!([AutomatonCommand;2] => Die)
    //                         }
    //                         1 => array_vec!([AutomatonCommand;2] => Recruit {
    //                             rule: Rule::of::<MyGrammar>(),
    //                             route: 1,
    //                             how_many: 2,
    //                             on_victory: Continuation::Advance
    //                         }, Die),
    //                         2 => if lexeme == <MyLexer as Lexer>::Lexemes::A {
    //                             array_vec!([AutomatonCommand;2] => Victory, Die)
    //                         } else {
    //                             array_vec!([AutomatonCommand;2] => Die)
    //                         }
    //                         _ => panic!("only three states")
    //                     }
    //                 }
    //                 1 => {
    //                     match state {
    //                         0 => if lexeme == <MyLexer as Lexer>::Lexemes::B {
    //                             array_vec!([AutomatonCommand;2] => Victory, Die)
    //                         } else {
    //                             array_vec!([AutomatonCommand;2] => Die)
    //                         }
    //                         _ => panic!("only 1 state")
    //                     }
    //                 }
    //                 2 => {
    //                     match state {
    //                         0 => if lexeme == <MyLexer as Lexer>::Lexemes::B {
    //                             array_vec!([AutomatonCommand;2] => Advance)
    //                         } else {
    //                             array_vec!([AutomatonCommand;2] => Die)
    //                         }
    //                         1 => array_vec!([AutomatonCommand;2] => Victory, Die),
    //                         _ => panic!("only 2 states")
    //                     }
    //                 }
    //                 _ => panic!("only 3 routes")
    //             }
    //         } else {
    //             panic!("only one rule")
    //         };
    //         result
    //     }
    //
    //     fn assemble(_auto: *mut Automaton) -> Self {
    //         Self::Thing
    //     }
    // }
}
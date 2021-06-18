pub mod automata;

use crate::lexer::{Lexeme, Lexer};
use core::any::TypeId as Rule;
use automata::*;
use tinyvec::ArrayVec;
use std::collections::VecDeque;
use crate::{ParceError, ParcePhase};
use std::fmt::Debug;

pub trait Parse<O: Parser>: ToString  {
    fn parse_max(&self) -> Result<(O, ParseCompletion), ParceError>;
    fn parse_all(&self) -> Result<O, ParceError>;
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ParseCompletion {
    Complete,
    Incomplete(usize)
}

pub trait Parser: 'static + Sized {
    type Lexemes: Copy + Eq + Debug;
    const PRODUCTIONS: u32;

    fn default_lexer() -> Box<dyn Lexer<Lexemes = Self::Lexemes>>;
    fn commands(rule: Rule, route: u32, state: u32, lexeme: Lexeme<Self::Lexemes>) -> ArrayVec<[AutomatonCommand; 3]>;
    fn assemble(auto: Rawtomaton, lexemes: &[Lexeme<Self::Lexemes>], text: &str) -> (usize, Self);
}

impl<I: ToString, O: Parser> Parse<O> for I {
    fn parse_max(&self) -> Result<(O, ParseCompletion), ParceError> {
        let text = self.to_string();
        let lexemes = O::default_lexer().lex(&text)?;

        let army: Army = Army::new();
        let mut alive: VecDeque<Rawtomaton> = VecDeque::new();

        for i in 0..O::PRODUCTIONS {
            alive.push_back(army.recruit(Rule::of::<O>(), i).into());
        }

        let mut last = None;

        let mut i = 0;
        while !alive.is_empty() && i < lexemes.len() {
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
            let completion = if consumed == lexemes.len() {
                ParseCompletion::Complete
            } else {
                ParseCompletion::Incomplete(lexemes[consumed-1].start + lexemes[consumed-1].len)
            };
            Ok((result, completion))
        } else {
            Err(ParceError {
                input: text,
                start: if alive.len() == 0 {
                    if i > 1 {
                        lexemes[i-2].start + lexemes[i-2].len
                    } else {
                        0
                    }
                } else if i > 0 {
                    lexemes[i-1].start + lexemes[i-1].len
                } else {
                    0
                },
                phase: ParcePhase::Parse
            })
        }
    }

    fn parse_all(&self) -> Result<O, ParceError> {
        let (result, completion) = self.parse_max()?;
        match completion {
            ParseCompletion::Complete => Ok(result),
            ParseCompletion::Incomplete(n) => Err(ParceError {
                input: self.to_string(),
                start: n,
                phase: ParcePhase::Parse
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use crate as parce;
    use parce::*;

    macro_rules! parser_error {
        ($input:literal $start:literal) => {
            Err(ParceError {
                input: $input.to_string(),
                start: $start,
                phase: ParcePhase::Parse
            })
        };
    }

    macro_rules! pass {
        ($str:literal $result:path) => {
            assert_eq!($str.parse_all(), Ok($result))
        }
    }

    macro_rules! fail {
        ($str:literal $grammar:ident $where:literal) => {
            assert_eq!($str.parse_all() as Result<$grammar,_>, parser_error!($str $where))
        }
    }

    #[lexer(MyLexer)]
    enum MyLexeme {
        A = "'a'",
        B = "'b'",
        C = "'c'",
        #[skip] WhiteSpace = "[ \t\n\r]"
    }

    ////// BASIC

    #[parser(MyLexer)]
    enum BasicGrammar {
        Thing = "A B C"
    }

    #[test]
    fn basic() {
        pass!("a b c" BasicGrammar::Thing);
        fail!("a b" BasicGrammar 3);
    }

    ////// OR

    #[parser(MyLexer)]
    enum OrGrammar {
        Or = "A (B | B C) A"
    }

    #[test]
    fn or() {
        pass!("abca" OrGrammar::Or);
        pass!("aba" OrGrammar::Or);
        fail!("aa" OrGrammar 1);
        fail!("a bbc a" OrGrammar 3);
    }

    ////// STAR, PARSE COMPLETION

    #[parser(MyLexer)]
    enum StarGrammar {
        Star = "(A B C)*"
    }

    #[test]
    fn star() {
        pass!("abc abc" StarGrammar::Star);
        assert_eq!("abc a".parse_max(), Ok((StarGrammar::Star, ParseCompletion::Incomplete(3))));
        fail!("abc a" StarGrammar 3);
    }

    ////// Other Rules

    #[parser(MyLexer)]
    enum DelegateGrammar {
        Start = "A #OrGrammar A"
    }

    #[test]
    fn other_rule() {
        pass!("a aba a" DelegateGrammar::Start);
    }
}
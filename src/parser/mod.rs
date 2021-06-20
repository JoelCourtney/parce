pub mod automata;

use crate::lexer::{Lexeme, Lexer};
use core::any::TypeId as Rule;
use automata::*;
use tinyvec::ArrayVec;
use std::collections::VecDeque;
use crate::error::{ParceError, ParcePhase, ParsePhaseFailure};
use std::fmt::Debug;
use crate::error::ParsePhaseFailure::NothingToParse;

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
    fn last_commands(rule: Rule, route: u32, state: u32) -> bool;
    fn assemble(auto: Rawtomaton, lexemes: &[Lexeme<Self::Lexemes>], text: &str) -> (usize, Self);
}

impl<I: ToString, O: Parser> Parse<O> for I {
    fn parse_max(&self) -> Result<(O, ParseCompletion), ParceError> {
        let text = self.to_string();
        if text.len() == 0 {
            return Err(ParceError {
                input: self.to_string(),
                start: 0,
                phase: ParcePhase::Parse(NothingToParse)
            })
        }
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
                    } else if !result.fallthrough {
                        j += 1;
                    }
                }
            }
            i += 1;
        }

        if i == lexemes.len() && last == None {
            for auto in &alive {
                unsafe {
                    if O::last_commands((***auto).rule, (***auto).route, (***auto).state) {
                        let result = army.command(*auto, tinyvec::array_vec!([AutomatonCommand; 3] => automata::AutomatonCommand::Victory));
                        if let Some(vic) = result.victorious {
                            dbg!("hellO");
                            last = Some(vic);
                        }
                    }
                }
            }
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
                start: if alive.len() == 0 {
                    if i > 1 {
                        lexemes[i-1].start
                    } else {
                        0
                    }
                } else if i > 0 {
                    text.len()
                } else {
                    0
                },
                input: text,
                phase: ParcePhase::Parse(
                    if alive.len() == 0 {
                        ParsePhaseFailure::NoMatches
                    } else {
                        ParsePhaseFailure::InputEndedTooSoon
                    }
                )
            })
        }
    }

    fn parse_all(&self) -> Result<O, ParceError> {
        let (result, completion) = self.parse_max()?;
        match completion {
            ParseCompletion::Complete => Ok(result),
            ParseCompletion::Incomplete(n) => Err(ParceError {
                input: self.to_string(),
                start: n+1,
                phase: ParcePhase::Parse(ParsePhaseFailure::LeftoverLexemes)
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use crate as parce;
    use parce::*;

    macro_rules! parser_error {
        ($input:literal $start:literal $error:ident) => {
            Err(parce::error::ParceError {
                input: $input.to_string(),
                start: $start,
                phase: parce::error::ParcePhase::Parse(parce::error::ParsePhaseFailure::$error)
            })
        };
    }

    macro_rules! pass {
        ($str:literal $result:path) => {
            assert_eq!($str.parse_all(), Ok($result))
        }
    }

    macro_rules! fail {
        ($str:literal $grammar:ident $where:literal $error:ident) => {
            assert_eq!($str.parse_all() as Result<$grammar,_>, parser_error!($str $where $error))
        }
    }

    #[lexer(MyLexer)]
    enum MyLexeme {
        A = "'a'",
        B = "'b'",
        C = "'c'",
        D = "'d'",
        E = "'e'",
        F = "'f'",
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
        fail!("a b" BasicGrammar 3 InputEndedTooSoon);
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
        fail!("aa" OrGrammar 1 NoMatches);
        fail!("a bbc a" OrGrammar 3 NoMatches);
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
        fail!("abc a" StarGrammar 4 LeftoverLexemes);
    }

    ////// PLUS, QUESTION, RANGE

    #[parser(MyLexer)]
    enum OperatorGrammar {
        Plus = "A (B C)+ A",
        Question = "B (A C)? B",
        FixedRange = "C (B A){3} C",
        InfiniteRange = "D (A B){2,} D",
        LimitedRange = "E (A B){2,4} E"
    }

    #[test]
    fn question() {
        pass!("b b" OperatorGrammar::Question);
        pass!("b ac b" OperatorGrammar::Question);
        fail!("b a b" OperatorGrammar 4 NoMatches);
    }
    //
    #[test]
    fn plus() {
        pass!("a bc a" OperatorGrammar::Plus);
        pass!("a bcbcbc a" OperatorGrammar::Plus);
        fail!("a bcb a" OperatorGrammar 6 NoMatches);
        fail!("aa" OperatorGrammar 1 NoMatches);
    }

    #[test]
    fn range() {
        pass!("c bababa c" OperatorGrammar::FixedRange);
        fail!("c baba c" OperatorGrammar 7 NoMatches);
        fail!("c babababa c" OperatorGrammar 8 NoMatches);

        pass!("d abab d" OperatorGrammar::InfiniteRange);
        pass!("d ababab d" OperatorGrammar::InfiniteRange);
        fail!("d ab" OperatorGrammar 4 InputEndedTooSoon);
        fail!("d ab d" OperatorGrammar 5 NoMatches);

        pass!("e abab e" OperatorGrammar::LimitedRange);
        pass!("e ababab e" OperatorGrammar::LimitedRange);
        pass!("e abababab e" OperatorGrammar::LimitedRange);
        fail!("e ab e" OperatorGrammar 5 NoMatches);
        fail!("e ababababab e" OperatorGrammar 10 NoMatches);
    }

    ////// Other Rules

    #[parser(MyLexer)]
    enum DelegateGrammar {
        Start = "A #OrGrammar A"
    }

    #[test]
    fn other_rule() {
        pass!("a aba a" DelegateGrammar::Start);
        pass!("a abca a" DelegateGrammar::Start);
    }

    ////// DOT & GREEDINESS

    #[parser(MyLexer)]
    enum DotGrammar {
        Dot = "A .* C"
    }

    #[test]
    fn dot_and_greedy() {
        pass!("a b c" DotGrammar::Dot);
        pass!("a babcabcbacbac cc" DotGrammar::Dot)
    }

    ////// END BEHAVIOR

    #[parser(MyLexer)]
    enum EndBehaviorGrammar {
        Star = "A B*",
        Question = "B A C?", // too low to be coding by myself on a saturday night
        Plus = "C A B+",
        FixedRange = "D A{2}",
        InfiniteRange = "E A{2,}",
        LimitedRange = "F A{2,3}"
    }

    #[test]
    fn end_behavior() {
        fail!("" EndBehaviorGrammar 0 NothingToParse);
        pass!("ab" EndBehaviorGrammar::Star);

        pass!("ba" EndBehaviorGrammar::Question);

        fail!("ca" EndBehaviorGrammar 2 InputEndedTooSoon);

        fail!("d" EndBehaviorGrammar 1 InputEndedTooSoon);
        fail!("da" EndBehaviorGrammar 2 InputEndedTooSoon);
        pass!("daa" EndBehaviorGrammar::FixedRange);
        fail!("daaa" EndBehaviorGrammar 4 LeftoverLexemes);

        fail!("e" EndBehaviorGrammar 1 InputEndedTooSoon);
        fail!("ea" EndBehaviorGrammar 2 InputEndedTooSoon);
        pass!("eaa" EndBehaviorGrammar::InfiniteRange);
        pass!("eaaa" EndBehaviorGrammar::InfiniteRange);

        fail!("f" EndBehaviorGrammar 1 InputEndedTooSoon);
        fail!("fa" EndBehaviorGrammar 2 InputEndedTooSoon);
        pass!("faa" EndBehaviorGrammar::LimitedRange);
        pass!("faaa" EndBehaviorGrammar::LimitedRange);
        fail!("faaaa" EndBehaviorGrammar 5 LeftoverLexemes);
    }

    ////// NESTING
}
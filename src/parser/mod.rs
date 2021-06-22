pub mod automata;

use crate::lexer::{Lexeme, Lexer};
use core::any::TypeId as Rule;
use automata::*;
use tinyvec::ArrayVec;
use std::collections::VecDeque;
use crate::error::{ParceError, ParsePhaseFailure, ParceErrorInfo};
use std::fmt::Debug;
use crate::error::ParsePhaseFailure::NothingToParse;

/// Trait for parsing types that implement [ToString] into types that
/// implement [Parser].
///
/// This is implemented by default for all types that implement [ToString],
/// you do not need to implement it yourself.
///
/// # Example
///
/// ```
/// use parce::*;
///
/// #[lexer(MyLexer)]
/// enum MyLexemes {
///     A = "'a'",
///     B = "'b'"
/// }
///
/// #[parser(MyLexer)] // generates a Parser implementation for MyGrammar
/// enum MyGrammar {
///     Rule = "A B"
/// }
///
/// fn main() {
///     let parsed: MyGrammar = "ab".parse_all().unwrap();
/// }
/// ```
pub trait Parse<O: Parser>: ToString  {
    /// Parses the production that matches the most number of lexemes. Might not use
    /// the entire set of lexemes.
    ///
    /// If successful, returns a tuple (rule, completion) where completion indicates
    /// how many lexemes were used. If unsuccessful, returns [ParceError].
    fn parse_max(&self) -> Result<(O, ParseCompletion), ParceError>;

    /// Parses a rule and requires that it uses all of the input lexemes. Returns an
    /// error if no productions use all of the input.
    fn parse_all(&self) -> Result<O, ParceError>;
}

/// Indicates how much of the input was used by a call to [Parse::parse_max].
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ParseCompletion {
    /// The result used all of the input.
    Complete,

    /// The result used some of the input. The number of lexemes used is stored as the data here.
    Incomplete(usize)
}

/// Trait implemented by the [parce_macros::parser] attribute macro.
pub trait Parser: 'static + Sized {
    type Lexer: Lexer;

    /// The number of variants in this rule.
    const PRODUCTIONS: u32;

    /// Returns the default lexer that will be used to generate the lexemes.
    fn default_lexer() -> Box<Self::Lexer>;

    /// The state machine used by the [Parse] trait to drive the automata during parsing.
    ///
    /// - rule: all commands will come from the [Parser::commands] function *on the type being parsed*,
    ///   even if it uses other rules internally. So each implementation checks if rule is it's own type,
    ///   and delegates to other types if not.
    /// - route: routes are a generalization of productions. The automata in this algorithm don't have
    ///   branching decision trees, they are a straight line of requirements. Each top-level production
    ///   in a rule are the main routes, indexed 0 through `PRODUCTIONS - 1`. In grammars that could match
    ///   multiple different sets of lexemes, extra routes are generated for each path. For example,
    ///   in the grammar:
    ///   ```
    ///   # use parce::*;
    ///   # #[lexer(MyLexer)]
    ///   # enum MyLexeme {
    ///   #   A = "'a'",
    ///   #  B = "'b'",
    ///   #  C = "'c'",
    ///   # }
    ///   #[parser(MyLexer)]
    ///   enum MyGrammar {
    ///       Rule = "A (B | C) A"
    ///   }
    ///   ```
    ///   The full rule is route 0, just the "B" is route 1, and just the "C" is route 2.
    /// - state: Each time a requirement for a route is fulfilled, the state of the automaton is incremented.
    ///   For example, in the grammar above:
    ///   - route 0 state 0 looks for A. If found, increments state
    ///   - route 0 state 1 doesn't look for anything, instead it spawns two more automata on routes 1 and 2,
    ///     and enters the special "dead" state, where it no longer receives commands
    ///   - route 1 state 0 and route 2 state 0 look for B and C, respectively. If they find their lexeme,
    ///     they increment their state, die, reactivate the parent automaton on route 0, and increment its state.
    ///     In this case, only one of the child automata can actually succeed, but that is not always the case.
    ///     In cases where multiple child routes can succeed, it is perfectly normal to reactivate multiple
    ///     clones of the original parent.
    ///   - route 0 state 2 looks for A. If found, increments state, declares victory, and the parse is successful.
    fn commands(rule: Rule, route: u32, state: u32, lexeme: Lexeme<<Self::Lexer as Lexer>::Lexemes>) -> ArrayVec<[AutomatonCommand; 3]>;

    /// This is a special case of the [Parser::commands] function. If parsing reaches the end of the lexemes and
    /// no rules are successful yet, [Parser::last_commands] is called because it is possible for the Star or Question
    /// operators to be unconditionally successful *after* all of the lexemes have been used. A result of
    /// `true` from this function means that the automaton would have been successful on the next lexeme,
    /// but only if it did not even need that lexeme to be there. For example, in this grammar:
    /// ```
    /// # use parce::*;
    ///   # #[lexer(MyLexer)]
    ///   # enum MyLexeme {
    ///   #   A = "'a'",
    ///   #  B = "'b'",
    ///   #  C = "'c'",
    ///   # }
    /// #[parser(MyLexer)]
    /// enum MyGrammar {
    ///     Rule = "A B*"
    /// }
    /// ```
    /// The input `[A]` should be successfully parsed, but that would usually require the automaton to be
    /// in state 1, where it would be unconditionally successful, and spawn a child automaton to look for B's.
    /// But since there are no more lexemes, the star would not have the chance to be successful without this function.
    fn last_commands(rule: Rule, route: u32, state: u32) -> bool;

    /// The last step of the parsing process. After the parse is successful, [Parser::assemble] builds the resulting
    /// grammar rule. `auto` is the automaton that was on the main route that was successful, and its
    /// pointers to its children are used to build the output.
    fn assemble(auto: Rawtomaton, lexemes: &[Lexeme<<Self::Lexer as Lexer>::Lexemes>], text: &str) -> (usize, Self);
}

impl<I: ToString, O: Parser> Parse<O> for I {
    fn parse_max(&self) -> Result<(O, ParseCompletion), ParceError> {
        let text = self.to_string();
        let lexemes = O::default_lexer().lex(&text)?;
        if lexemes.len() == 0 {
            return Err(ParceError {
                input: text,
                start: 0,
                info: ParceErrorInfo::Parse { failure: NothingToParse }
            })
        }

        let army: Army = Army::new();
        let mut alive: VecDeque<Rawtomaton> = VecDeque::new();

        for i in 0..O::PRODUCTIONS {
            alive.push_back(army.spawn(Rule::of::<O>(), i, 0).into());
        }

        let mut last = None;

        let mut i = 0;
        while !alive.is_empty() && i < lexemes.len() {
            let mut j = 0;
            while j < alive.len() {
                let auto = alive[j];
                unsafe {
                    let commands = O::commands((**auto).rule, (**auto).route, (**auto).state, lexemes[i]);
                    let result = army.command(auto, commands, i);
                    alive.extend(result.new_spawns);
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

        if i == lexemes.len() {
            for auto in &alive {
                unsafe {
                    if O::last_commands((***auto).rule, (***auto).route, (***auto).state) {
                        let result = army.command(*auto, tinyvec::array_vec!([AutomatonCommand; 3] => automata::AutomatonCommand::Victory), 0);
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
                info: ParceErrorInfo::parse(
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
                info: ParceErrorInfo::parse(ParsePhaseFailure::LeftoverLexemes)
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
                info: parce::error::ParceErrorInfo::parse(parce::error::ParsePhaseFailure::$error)
            })
        };
    }

    macro_rules! pass {
        ($str:literal $result:expr) => {
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
        A = 'a',
        B = 'b',
        C = 'c',
        D = 'd',
        E = 'e',
        F = 'f',
        G = 'g',
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

    #[parser(MyLexer)]
    enum NestingGrammar {
        Or = "A (B | C (A A | B B)) D",
        And = "B C (D E)",
        Star = "C (A B C*)*"
    }

    #[test]
    fn or_nesting() {
        pass!("abd" NestingGrammar::Or);
        pass!("acaad" NestingGrammar::Or);
        pass!("acbbd" NestingGrammar::Or);

        fail!("abaad" NestingGrammar 2 NoMatches);
        fail!("abbbd" NestingGrammar 2 NoMatches);
        fail!("acd" NestingGrammar 2 NoMatches);
    }

    #[test]
    fn and_nesting() {
        pass!("bcde" NestingGrammar::And);
        fail!("bc" NestingGrammar 2 InputEndedTooSoon);
    }

    #[test]
    fn star_nesting() {
        pass!("c" NestingGrammar::Star);
        pass!("c abc" NestingGrammar::Star);
        pass!("c ab ab" NestingGrammar::Star);
        pass!("c abc ab abcccc" NestingGrammar::Star);
    }

    ////// BARE FIELDS

    #[parser(MyLexer)]
    enum BareUnnamedGrammar {
        Basic(BasicGrammar) = "A 0 A",
        StarVec(Vec<BasicGrammar>) = "B (C 0)*",
        QuestionOption(Option<BasicGrammar>) = "C 0?",
        PlusVec(Vec<BasicGrammar>) = "D (A 0)+",
        RangeVec(Vec<BasicGrammar>) = "E 0{2,4}",
        NestedVecOption(Vec<Option<BasicGrammar>>) = "F (D 0?)+",

        Multiple(Option<BasicGrammar>, Vec<OrGrammar>) = "G 0? A 1+"
    }

    #[test]
    fn bare_unnamed_grammar() {
        pass!("a abc a" BareUnnamedGrammar::Basic(BasicGrammar::Thing));

        pass!("b cabc cabc cabc" BareUnnamedGrammar::StarVec(vec![BasicGrammar::Thing, BasicGrammar::Thing, BasicGrammar::Thing]));
        pass!("b" BareUnnamedGrammar::StarVec(vec![]));

        pass!("c abc" BareUnnamedGrammar::QuestionOption(Some(BasicGrammar::Thing)));
        pass!("c" BareUnnamedGrammar::QuestionOption(None));

        pass!("d aabc aabc aabc" BareUnnamedGrammar::PlusVec(vec![BasicGrammar::Thing, BasicGrammar::Thing, BasicGrammar::Thing]));
        pass!("d aabc" BareUnnamedGrammar::PlusVec(vec![BasicGrammar::Thing]));

        pass!("e abc abc" BareUnnamedGrammar::RangeVec(vec![BasicGrammar::Thing, BasicGrammar::Thing]));

        pass!("f dabc dd dabc" BareUnnamedGrammar::NestedVecOption(vec![Some(BasicGrammar::Thing), None, None, Some(BasicGrammar::Thing)]));

        pass!("g abc a abca" BareUnnamedGrammar::Multiple(Some(BasicGrammar::Thing), vec![OrGrammar::Or]));
    }

    ////// NAMED FIELDS

    #[parser(MyLexer)]
    enum BareNamedGrammar {
        Basic {hello: BasicGrammar} = "A hello A",
        StarVec {there: Vec<BasicGrammar>} = "B (C there)*",
        QuestionOption{general: Option<BasicGrammar>} = "C general?",
        PlusVec {kenobi: Vec<BasicGrammar>} = "D (A kenobi)+",
        RangeVec {some_body: Vec<BasicGrammar>} = "E some_body{2,4}", // i know somebody is one word, shush
        NestedVecOption {once: Vec<Option<BasicGrammar>>} = "F (D once?)+",

        Multiple {told: Option<BasicGrammar>, me: Vec<OrGrammar>} = "G told? A me+"
    }

    #[test]
    fn bare_named_grammar() {
        pass!("a abc a" BareNamedGrammar::Basic {hello: BasicGrammar::Thing});

        pass!("b cabc cabc cabc" BareNamedGrammar::StarVec {there: vec![BasicGrammar::Thing, BasicGrammar::Thing, BasicGrammar::Thing]});
        pass!("b" BareNamedGrammar::StarVec {there: vec![]});

        pass!("c abc" BareNamedGrammar::QuestionOption {general: Some(BasicGrammar::Thing)});
        pass!("c" BareNamedGrammar::QuestionOption {general: None});

        pass!("d aabc aabc aabc" BareNamedGrammar::PlusVec {kenobi: vec![BasicGrammar::Thing, BasicGrammar::Thing, BasicGrammar::Thing]});
        pass!("d aabc" BareNamedGrammar::PlusVec {kenobi: vec![BasicGrammar::Thing]});

        pass!("e abc abc" BareNamedGrammar::RangeVec {some_body: vec![BasicGrammar::Thing, BasicGrammar::Thing]});

        pass!("f dabc dd dabc" BareNamedGrammar::NestedVecOption {once: vec![Some(BasicGrammar::Thing), None, None, Some(BasicGrammar::Thing)]});

        pass!("g abc a abca" BareNamedGrammar::Multiple {told: Some(BasicGrammar::Thing), me: vec![OrGrammar::Or]});
    }
}
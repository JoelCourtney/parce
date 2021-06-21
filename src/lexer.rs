use crate::error::ParceError;
use std::fmt::Debug;

pub trait Lexer: std::fmt::Display + Debug {
    type Lexemes: Eq + Copy + std::fmt::Debug;

    fn lex(&mut self, s: &str) -> Result<Vec<Lexeme<Self::Lexemes>>, ParceError>;
}

#[derive(Debug, Default, Eq, PartialEq, Copy, Clone, Hash)]
pub struct Lexeme<T: Copy + Eq + Debug> {
    pub data: T,
    pub start: usize,
    pub len: usize
}

/// Can only implement Lexeme<T> == T, because trying to implement T == Lexeme<T> violates
/// orphan rules. When comparing a wrapped Lexeme to its internal type, put the lexeme first.
impl<T: Copy + Eq + Debug> PartialEq<T> for Lexeme<T> {
    fn eq(&self, other: &T) -> bool {
        self.data == *other
    }
}

#[cfg(test)]
mod tests {

    /// Shorthand to make testing easier
    macro_rules! lexemes {
        ($($lexeme:ident $start:literal $len:literal),+) => {
            Ok(vec![
                $(
                    Lexeme {
                        data: $lexeme,
                        start: $start,
                        len: $len
                    }
                ),+
            ])
        }
    }

    macro_rules! lexer_error {
        ($input:literal $start:literal) => {
            Err(ParceError {
                input: $input.to_string(),
                start: $start,
                phase: error::ParcePhase::Lex("Default".to_string())
            })
        };
        ($input:literal $start:literal $mode:literal) => {
            Err(ParceError {
                input: $input.to_string(),
                start: $start,
                phase: error::ParcePhase::Lex($mode.to_string())
            })
        }
    }

    use crate::*;
    use crate as parce;
    use super::*;

    /////// LITERALS

    #[lexer(LiteralLexer)]
    enum LiteralLexeme {
        A = 'a',
        B = "'b'"
    }

    #[test]
    fn basic_pass() {
        use LiteralLexeme::*;

        assert_eq!(LiteralLexer::default().lex("ab"), lexemes![A 0 1, B 1 1]);
    }

    #[test]
    fn basic_fail() {
        assert_eq!(LiteralLexer::default().lex("a b"), lexer_error!("a b" 1));
    }

    /////// OPERATORS

    #[lexer(OperatorLexer)]
    enum OperatorLexeme {
        A = "'a'",
        BPlus = "'b'+",
        CStar = "'c'*",
        DQuestion = "'d'?",
        ERange = "'e'{2,4}",
        FExact = "'f'{2}",
        GMin = "'g'{2,}"
    }

    #[test]
    fn empty() {
        assert_eq!(OperatorLexer::default().lex(""), Ok(vec![]));
    }

    #[test]
    fn plus() {
        use OperatorLexeme::*;

        assert_eq!(OperatorLexer::default().lex("bbbabb"), lexemes![BPlus 0 3, A 3 1, BPlus 4 2]);
    }

    #[test]
    fn star() {
        use OperatorLexeme::*;

        assert_eq!(OperatorLexer::default().lex("cccacc"), lexemes![CStar 0 3, A 3 1, CStar 4 2]);
    }

    #[test]
    fn question() {
        use OperatorLexeme::*;

        assert_eq!(OperatorLexer::default().lex("a"), lexemes![A 0 1]);
        assert_eq!(OperatorLexer::default().lex("d"), lexemes![DQuestion 0 1]);
        assert_eq!(OperatorLexer::default().lex("dd"), lexemes![DQuestion 0 1, DQuestion 1 1]);
    }

    #[test]
    fn range() {
        use OperatorLexeme::*;

        assert_eq!(OperatorLexer::default().lex("e"), lexer_error!("e" 0));
        assert_eq!(OperatorLexer::default().lex("ee"), lexemes![ERange 0 2]);
        assert_eq!(OperatorLexer::default().lex("eee"), lexemes![ERange 0 3]);
        assert_eq!(OperatorLexer::default().lex("eeee"), lexemes![ERange 0 4]);
        assert_eq!(OperatorLexer::default().lex("eeeee"), lexer_error!("eeeee" 4));

        assert_eq!(OperatorLexer::default().lex("f"), lexer_error!("f" 0));
        assert_eq!(OperatorLexer::default().lex("ff"), lexemes![FExact 0 2]);
        assert_eq!(OperatorLexer::default().lex("fff"), lexer_error!("fff" 2));

        assert_eq!(OperatorLexer::default().lex("g"), lexer_error!("g" 0));
        assert_eq!(OperatorLexer::default().lex("gg"), lexemes![GMin 0 2]);
        assert_eq!(OperatorLexer::default().lex("ggg"), lexemes![GMin 0 3]);
        assert_eq!(OperatorLexer::default().lex("gggg"), lexemes![GMin 0 4]);
    }

    /////// NESTING & SEQUENCES

    #[lexer(NestingLexer)]
    enum NestingLexeme {
        A = "'a'",
        Nest = "'b' A 'b'",
        DoubleNest = "'a' Nest* 'a'"
    }

    #[test]
    fn nest_and_sequence() {
        use NestingLexeme::*;

        assert_eq!(NestingLexer::default().lex("bab"), lexemes![Nest 0 3]);
        assert_eq!(NestingLexer::default().lex("ababbaba"), lexemes![DoubleNest 0 8]);
        assert_eq!(NestingLexer::default().lex("aa"), lexemes![DoubleNest 0 2]);
        assert_eq!(NestingLexer::default().lex("a"), lexemes![A 0 1]);
    }

    /////// PRIORITY & DOTS

    #[lexer(DotLexer)]
    enum DotLexeme {
        A = "'a'",
        Dot = ".",
        Junk = "'c' .* 'c'"
    }

    #[test]
    fn priority_and_dot() {
        use DotLexeme::*;

        assert_eq!(DotLexer::default().lex("ab"), lexemes![A 0 1, Dot 1 1]);
        assert_eq!(DotLexer::default().lex("cxcvbxc"), lexemes![Junk 0 7]);
    }

    /////// CLASSES

    #[lexer(ClassLexer)]
    enum ClassLexeme {
        AB = "[ab]",
        Number = "[0-9]+",
        String = r#" '"' [^"\n\r]* '"' "#
    }

    #[test]
    fn class() {
        use ClassLexeme::*;

        assert_eq!(ClassLexer::default().lex("a"), lexemes![AB 0 1]);
        assert_eq!(ClassLexer::default().lex("b"), lexemes![AB 0 1]);
        assert_eq!(ClassLexer::default().lex("c"), lexer_error!("c" 0));

        assert_eq!(ClassLexer::default().lex("1105"), lexemes![Number 0 4]);

        assert_eq!(ClassLexer::default().lex(r#""Hello World!""#), lexemes![String 0 14]);
        assert_eq!(ClassLexer::default().lex(r#""Unclosed"#), lexer_error!(r#""Unclosed"# 0));
        assert_eq!(ClassLexer::default().lex(r#""Extra" "#), lexer_error!(r#""Extra" "# 7));
    }

    /////// GREEDINESS

    #[lexer(GreedyLexer)]
    enum GreedyLexeme {
        A = "'a' .* 'b'",
        One = "'c' [de]*",
        Two = "'e' 'd' 'e' 'f'*",
        Both = "One Two"
    }

    #[test]
    fn greedy() {
        use GreedyLexeme::*;

        assert_eq!(GreedyLexer::default().lex("abbb"), lexemes![A 0 4]);
        assert_eq!(GreedyLexer::default().lex("cddedefff"), lexemes![Both 0 9]);
    }

    /////// OR & GROUPS

    #[lexer(OrLexer)]
    enum OrLexeme {
        AB = "'a' | 'b'",
        Group = "'c' ( AB | 'd' )+ 'c'"
    }

    #[test]
    fn or() {
        use OrLexeme::*;

        assert_eq!(OrLexer::default().lex("a"), lexemes![AB 0 1]);
        assert_eq!(OrLexer::default().lex("b"), lexemes![AB 0 1]);
        assert_eq!(OrLexer::default().lex("c"), lexer_error!("c" 0));

        assert_eq!(OrLexer::default().lex("cabdbdabdabdc"), lexemes![Group 0 13])
    }

    /////// SKIPS

    #[lexer(SkipLexer)]
    enum SkipLexeme {
        #[skip] WhiteSpace = "[ \n\r\t]",
        A = "'a'",
        B = "'b'"
    }

    #[test]
    fn skip() {
        use SkipLexeme::*;

        assert_eq!(SkipLexer::default().lex(" a\nb "), lexemes![A 1 1, B 3 1]);
    }

    /////// FRAGMENTS

    #[lexer(FragmentLexer)]
    enum FragmentLexeme {
        #[frag] A = "'a'",
        AA = "A A"
    }

    #[test]
    fn fragment() {
        use FragmentLexeme::*;

        assert_eq!(FragmentLexer::default().lex("aa"), lexemes![AA 0 2]);
        assert_eq!(FragmentLexer::default().lex("a"), lexer_error!["a" 0]);
    }

    /////// MODES

    #[lexer(ModalLexer)]
    #[modes(One, Two)]
    enum ModalLexeme {
        A = "'a'",
        #[set_mode(Two)] B = "'b'",

        #[mode(Two)]

        C = "'c'",
        #[set_mode(One)] D = "'d'",

        #[mode(One, Two)]
        E = "'e'"
    }

    #[test]
    fn change_mode() {
        use ModalLexeme::*;

        assert_eq!(ModalLexer::default().lex("aeabcecda"), lexemes![
            A 0 1, E 1 1, A 2 1, B 3 1, C 4 1, E 5 1, C 6 1, D 7 1, A 8 1
        ]);
        assert_eq!(ModalLexer::default().lex("d"), lexer_error!["d" 0 "One"]);

        assert_eq!(ModalLexer::Two.lex("d"), lexemes![D 0 1]);
    }
}
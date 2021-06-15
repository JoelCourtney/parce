use std::fmt::Formatter;

pub trait Lexer<T: Copy>: std::fmt::Display + std::fmt::Debug {
    fn lex(&mut self, s: &str) -> Result<Vec<Lexeme<T>>, LexerError> where Self: Sized;
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LexerError {
    pub input: String,
    pub start: usize,
    pub mode: String
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use colored::Colorize;

        let start = self.start.saturating_sub(10);
        let end = std::cmp::min(self.input.len(), self.start + 10);
        let short = format!("{}{}{}",
            if start != 0 {
                "..."
            } else {
                ""
            },
            &self.input[start..end],
            if end != self.input.len() {
                "..."
            } else {
                ""
            }
        );

        write!(
            f,
            "Lexer Error: {}\nLexer Mode: {}\nInput: {}{}\n{}{}",
            "no possible lexemes matched this input".red(),
            self.mode.bright_blue(),
            &short[..self.start - if start != 0 {start - 3} else {0}],
            &short[(self.start - if start != 0 {start - 3} else {0})..].red(),
            " ".repeat(7 + self.start - if start != 0 {start - 3} else {0}),
            "^".red().bold(),
        )
    }
}

impl std::error::Error for LexerError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> { None }
}

#[derive(Debug, Default, Eq, PartialEq, Copy, Clone, Hash)]
pub struct Lexeme<T: Copy> {
    pub data: T,
    pub start: usize,
    pub len: usize
}

#[cfg(test)]
mod tests {

    /// Shorthand to make testing easier
    macro_rules! lexemes {
        ($lexemes:ident; $($lexeme:ident $start:literal $len:literal),+) => {
            vec![
                $(
                    Lexeme {
                        data: $lexemes::$lexeme,
                        start: $start,
                        len: $len
                    }
                ),+
            ]
        }
    }

    use crate::prelude::*;
    use crate as parce;

    /////// LITERALS

    #[lexer(LiteralLexer)]
    enum LiteralLexeme {
        A = "'a'",
        B = "'b'"
    }

    #[test]
    fn basic_pass() {
        assert_eq!(LiteralLexer::default().lex("ab"), Ok(lexemes![
            LiteralLexeme;
            A 0 1, B 1 1
        ]));
    }

    #[test]
    fn basic_fail() {
        assert_eq!(LiteralLexer::default().lex("a b"), Err(
            LexerError {
                input: "a b".to_string(),
                start: 1,
                mode: "Default".to_string()
            }
        ))
    }

    /////// OPERATORS

    #[lexer(OperatorLexer)]
    enum OperatorLexeme {
        A = "'a'",
        BPlus = "'b'+",
        CStar = "'c'*",
        DQuestion = "'d'?",
        ERange = "'e'{2,4}"
    }

    #[test]
    fn empty() {
        assert_eq!(OperatorLexer::default().lex(""), Ok(vec![]));
    }

    #[test]
    fn plus() {
        assert_eq!(OperatorLexer::default().lex("bbbabb"), Ok(lexemes![
            OperatorLexeme;
            BPlus 0 3, A 3 1, BPlus 4 2
        ]));
    }

    #[test]
    fn star() {
        assert_eq!(OperatorLexer::default().lex("cccacc"), Ok(lexemes![
            OperatorLexeme;
            CStar 0 3, A 3 1, CStar 4 2
        ]));
    }

    #[test]
    fn question() {
        assert_eq!(OperatorLexer::default().lex("a"), Ok(lexemes![
            OperatorLexeme;
            A 0 1
        ]));
        assert_eq!(OperatorLexer::default().lex("d"), Ok(lexemes![
            OperatorLexeme;
            DQuestion 0 1
        ]));
        assert_eq!(OperatorLexer::default().lex("dd"), Ok(lexemes![
            OperatorLexeme;
            DQuestion 0 1, DQuestion 1 1
        ]));
    }

    #[test]
    fn range() {
        assert_eq!(OperatorLexer::default().lex("e"), Err(LexerError {
            input: "e".to_string(),
            start: 0,
            mode: "Default".to_string()
        }));
        assert_eq!(OperatorLexer::default().lex("ee"), Ok(lexemes![
            OperatorLexeme;
            ERange 0 2
        ]));
        assert_eq!(OperatorLexer::default().lex("eee"), Ok(lexemes![
            OperatorLexeme;
            ERange 0 3
        ]));
        assert_eq!(OperatorLexer::default().lex("eeee"), Ok(lexemes![
            OperatorLexeme;
            ERange 0 4
        ]));
        assert_eq!(OperatorLexer::default().lex("eeeee"), Err(LexerError {
            input: "eeeee".to_string(),
            start: 4,
            mode: "Default".to_string()
        }));
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
        assert_eq!(NestingLexer::default().lex("bab"), Ok(lexemes![
            NestingLexeme;
            Nest 0 3
        ]));
        assert_eq!(NestingLexer::default().lex("ababbaba"), Ok(lexemes![
            NestingLexeme;
            DoubleNest 0 8
        ]));
        assert_eq!(NestingLexer::default().lex("aa"), Ok(lexemes![
            NestingLexeme;
            DoubleNest 0 2
        ]));
        assert_eq!(NestingLexer::default().lex("a"), Ok(lexemes![
            NestingLexeme;
            A 0 1
        ]));
    }

    /////// PRIORITY & DOTS

    #[lexer(DotLexer)]
    enum DotLexeme {
        A = "'a'",
        Dot = "."
    }

    #[test]
    fn priority_and_dot() {
        assert_eq!(DotLexer::default().lex("ab"), Ok(lexemes![
            DotLexeme;
            A 0 1, Dot 1 1
        ]))
    }

    /////// GREEDYNESS

    #[lexer(GreedyLexer)]
    enum GreedyLexeme {
        A = "'a' .* 'b'"
    }

    #[test]
    fn greedy() {
        assert_eq!(GreedyLexer::default().lex("abbb"), Ok(lexemes![
            GreedyLexeme;
            A 0 4
        ]));
        let re = regex::Regex::new("a.*b").unwrap();
        assert!(re.is_match("abbb"));
    }

    /////// OR & GROUPS
    /////// DOTS
    /////// CLASSES
    /////// SKIPS
    /////// FRAGMENTS
    /////// MODES
}
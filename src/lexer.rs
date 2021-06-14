use std::fmt::Formatter;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LexerError {
    pub full: String,
    pub start: usize,
    pub mode: String
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use colored::Colorize;

        let start = self.start.saturating_sub(10);
        let end = std::cmp::min(self.full.len(), self.start + 10);
        let short = format!("{}{}{}",
            if start != 0 {
                "..."
            } else {
                ""
            },
            &self.full[start..end],
            if end != self.full.len() {
                "..."
            } else {
                ""
            }
        );

        write!(
            f,
            "{}\n{}{}\nLexer Error: no possible lexemes matched this input\nLexer Mode: {}",
            short,
            " ".repeat(self.start - if start != 0 {start - 3} else {0}),
            "^".red(),
            self.mode.bright_blue()
        )
    }
}

impl std::error::Error for LexerError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> { None }
}

pub trait Lexer<T: Copy>: std::fmt::Display + std::fmt::Debug {
    fn lex(&mut self, s: &str) -> Result<Vec<Lexeme<T>>, LexerError> where Self: Sized;
}

#[derive(Debug, Default, Eq, PartialEq, Copy, Clone, Hash)]
pub struct Lexeme<T: Copy> {
    pub data: T,
    pub start: usize,
    pub len: usize
}
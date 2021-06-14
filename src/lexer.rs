use std::fmt::Formatter;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct LexerError {
    pub full: String,
    pub start: usize,
    pub len: usize,
    pub message: String,
    pub mode: String
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use colored::Colorize;

        write!(
            f,
            "{}\n{}{}\nLexer Error: {}\nLexer Mode: {}",
            self.full,
            " ".repeat(self.start),
            "^".repeat(self.len).red(),
            self.message,
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
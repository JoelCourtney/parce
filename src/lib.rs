pub mod prelude;

use parce_core::{Lexeme, LexError};

pub trait Lexer<T> {
    fn lex(&mut self, s: &str) -> Result<Vec<Lexeme<T>>, LexError> where Self: Sized;
}

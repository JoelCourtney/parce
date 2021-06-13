pub struct LexError;

// lexeme enum implements FromStr
// parser rules implement FromTokens
// auto-impl FromStr for FromTokens

pub trait Lexer<T> {
    fn lex(&mut self, s: &str, start: usize) -> Result<Lexeme<T>, LexError> where Self: Sized;
}

pub struct Lexeme<T> {
    pub data: T,
    pub start: usize,
    pub length: usize
}
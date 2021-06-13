pub use gen_parser_macro::lex;

pub struct ParseError;
pub struct LexError;

// lexeme enum implements FromStr
// parser rules implement FromTokens
// auto-impl FromStr for FromTokens

pub trait Lex {
    fn lex(s: &str, start: usize) -> Result<Lexeme<Self>, LexError> where Self: Sized;
}

pub struct Lexeme<T> {
    data: T,
    start: usize,
    length: usize
}
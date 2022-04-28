use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize)]
pub enum ParserAst {
    Lexeme(String),
    Plus(Box<ParserAst>)
}
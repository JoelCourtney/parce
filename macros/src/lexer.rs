use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub enum LexerAst {
    Literal(String),
    Ident(String),
    Group(Vec<LexerAst>),
    Star(Box<LexerAst>),
    Plus(Box<LexerAst>),
    Question(Box<LexerAst>),
    Dot,
    Or(Vec<LexerAst>)
}
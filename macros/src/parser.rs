#[allow(dead_code)]
pub enum ParserAst {
    Lexeme(String),
    Plus(Box<ParserAst>),
}

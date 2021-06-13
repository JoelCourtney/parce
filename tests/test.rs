use parce::prelude::*;

#[lex]
pub enum Lexemes {
    #[frag]
    Digit = "[0-9]",

    Period = "'.'",

    Carrot = "'^'",

    #[set_mode = "helo"]
    Star = "'*'",

    Slash = "'/'",

    #[skip]
    WhiteSpace = "[ \n\r\t]",

    #[frag]
    StringChar = "~( '\"' | '\\' | '\r' | '\n' )",

    StringLiteral = r#" '"' StringChar* '"' "#,

    #[mode = "helo"]
    Asdf = "asdf",

    #[set_mode = "default"] Zxcv = "zxcv"
}

#[test]
fn test_parse() {
    let mut lexer = LexemesLexer::default();
    lexer.lex("hello", 0);
}
//
// #[parse]
// pub enum Number {
//     Int(i32) = "0=(Digit+)",
//     Float(f32) = "0=(Digit+ Period Digit* | Digit* Period Digit+)"
// }
//
// #[parse]
// pub enum Expression {
//     Power(Box<Expression>, Box<Expression>) = "0 Carrot 1",
//     TimesDiv(Box<Expression>, Lexeme, Box<Expression>) = "0 1=(Star | Slash) 2",
//     Number(Number) = "0"
// }
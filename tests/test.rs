use parce::prelude::*;

#[lex]
pub enum Lexemes {
    // #[frag]
    // Digit = "[0-9]",

    Period = "'.'",

    Carrot = "'^'",

    #[set_mode = "helo"]
    Star = "'*'",

    Slash = "'/'",

    // #[skip]
    // WhiteSpace = "[ \n\r\t]",

    // #[frag]
    // StringChar = "~( '\"' | '\\' | '\r' | '\n' )",
    StringChar = "[asdf]",

    StringLiteral = r#" '"' StringChar* '"' "#,
    TestyBoi = "StringChar*",

    #[mode = "helo"]
    Asdf = "'asdf'",

    #[set_mode = "default"] Zxcv = "'zxcv'"
}

#[test]
fn test_parse() {
    let mut lexer = LexemesLexer::default();
    assert_eq!(lexer.lex(r#""asf""#), Ok(vec![Lexeme {
        data: Lexemes::StringLiteral,
        start: 0,
        len: 5
    }]));
}

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
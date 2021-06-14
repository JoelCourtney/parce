use parce::prelude::*;

#[lexer(MyLexer)]
pub enum Lexemes {
    // #[frag]
    // Digit = "[0-9]",

    #[skip]
    // WhiteSpace = "[ \n\r\t]",
    WhiteSpace = "' '",

    #[frag]
    StringChar = "[^\"\\\\\r\n]",

    StringLiteral = r#" '"' StringChar* '"' "#,
}

#[test]
fn test_parse() {
    let mut lexer = MyLexer::default();
    let result = lexer.lex(r#"   "Hello World!"  "#);
    assert_eq!(result, Ok(vec![Lexeme {
        data: Lexemes::StringLiteral,
        start: 3,
        len: 14
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
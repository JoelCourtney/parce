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

    StringLiteral = r#" '"' StringChar{2,} '"' "#,

    SomeThingElse = " 'c' ('a' 'b'+)+ "
}

#[test]
fn test_parse() {
    let mut lexer = MyLexer::default();
    let result = lexer.lex("   \"H\"  ");
    match result {
        Ok(o) => {dbg!(o);}
        Err(e) => println!("{}", e)
    }
    // assert_eq!(result, Ok(vec![Lexeme {
    //     data: Lexemes::StringLiteral,
    //     start: 3,
    //     len: 14
    // }]));
}

#[test]
fn test_group() {
    let result = MyLexer::default().lex(" \"String time\" cabbababbbbb");
    match result {
        Ok(emes) => {dbg!(emes);}
        Err(e) => println!("{}", e)
    }
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
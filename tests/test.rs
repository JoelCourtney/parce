use gen_parser_macro::lex;

// #[lex]
// pub enum Lexeme {
//     #[frag] Digit = "[0-9]",
//     Period = ".",
//     Carrot = "^",
//     Star = "*",
//     Slash = "/"
// }
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
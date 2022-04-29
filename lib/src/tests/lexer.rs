use crate::*;

#[lexer(BasicLexer)]
enum BasicLexeme {
    AAB = d!(
        | 'a' +
        | 'b'
    ),
}

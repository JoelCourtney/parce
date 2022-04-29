use crate::*;

#[lexer(BasicLexer)]
#[default_mode = Hello]
enum BasicLexeme {
    #[mode = Hello]
    #[set_mode = Zxcv]
    #[skip]
    AAB = d!(
        | 'a' +
        | 'b'
    ),
}

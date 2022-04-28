use parce_macros::l;
// #[derive(Lexer)]
// #[lexer_name(BasicLexer)]
enum BasicLexeme {
    AAB = l!(
        | 'a' +
        | 'b'
    ),
}

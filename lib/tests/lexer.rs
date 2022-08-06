use parce::*;
use logos::Logos;

#[lexer(MyLexer)]
#[derive(Debug, PartialEq, Eq)]
enum MyToken {
    AB = "ab",
    AAB = p!("aa"  'b')
}

#[derive(Logos, Debug, PartialEq, Eq)]
enum LogosFinder {
    #[token("ab")]
    AB,

    #[token("aab")]
    AAB,

    #[error]
    Error
}

#[test]
fn test() {
    let mut input = String::new();
    let stdin = std::io::stdin();
    stdin.read_line(&mut input).unwrap();
    let start = std::time::Instant::now();
    let chars: Vec<char> = input.chars().collect();
    for _ in 0..100000000 {
        // assert_eq!(LogosFinder::lexer(&input).next(), Some(LogosFinder::AB));
        assert_eq!(MyToken::lexer().lex(&chars).unwrap(), Lexeme {
            span: &chars[..2],
            token: MyToken::AB
        });
    }
    dbg!(std::time::Instant::now() - start);
}

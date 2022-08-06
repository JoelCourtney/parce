use parce::*;
use logos::Logos;

#[lexer(MyLexer)]
#[derive(Debug, PartialEq, Eq)]
enum MyToken {
    AB = "asdf",
    AAB = p!("asdfzxcv")

}

#[derive(Logos, Debug, PartialEq, Eq)]
enum LogosFinder {
    #[token("asdf")]
    AB,

    #[regex("asdfzxcv")]
    AAB,

    #[error]
    Error
}

#[test]
fn test() {
    let mut input = String::new();
    let stdin = std::io::stdin();
    stdin.read_line(&mut input).unwrap();
    let chars: Vec<char> = input.chars().collect();
    let start = std::time::Instant::now();
    for _ in 0..100000000 {
        unsafe {
            // assert_eq!(LogosFinder::lexer(&input).next().unwrap_unchecked(), LogosFinder::AAB);
            assert_eq!(MyToken::lexer().lex(&chars).unwrap_unchecked(), MyToken::AAB);
        }
    }
    dbg!(std::time::Instant::now() - start);
}

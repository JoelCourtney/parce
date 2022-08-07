use parce::prelude::*;
use logos::Logos;

#[lexer(MyLexer)]
#[derive(Debug, PartialEq, Eq)]
enum MyToken {
    AA = "aa",
    ABAA = p!("ab"? "ab"),
}

#[derive(Logos, Debug, PartialEq, Eq)]
enum LogosFinder {
    #[token("aa")]
    AA,

    #[regex("(ab)?ab")]
    ABAA,

    #[error]
    Error
}

#[test]
fn test() {
    // let mut input = String::new();
    // let stdin = std::io::stdin();
    // stdin.read_line(&mut input).unwrap();
    // let start = std::time::Instant::now();
    // let chars = input.chars();
    // for _ in 0..1000000000 {
    //     unsafe {
            // assert_eq!(LogosFinder::lexer(&input).next().unwrap_unchecked(), LogosFinder::ABAA);
            // assert_eq!(input.chars().lex::<MyLexer>().next().unwrap_unchecked(), MyToken::ABAA);
            // assert_eq!(MyLexer::default().lex_from_slice(&chars).unwrap_unchecked(), MyToken::ABAA);
        // };
    // }
    // dbg!(std::time::Instant::now() - start);

    let lexemes = "ababaa".lex::<MyLexer>();
    assert_eq!(lexemes.collect::<Vec<_>>(), vec![MyToken::ABAA, MyToken::AA]);
}

use parce::*;
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
    let mut input = String::new();
    let stdin = std::io::stdin();
    stdin.read_line(&mut input).unwrap();
    let chars: Vec<char> = input.chars().collect();
    let start = std::time::Instant::now();
    for _ in 0..1000000000 {
        unsafe {
            // assert_eq!(LogosFinder::lexer(&input).next().unwrap_unchecked(), LogosFinder::ABAA);
            assert_eq!(MyToken::lexer().lex(&chars).unwrap_unchecked(), MyToken::ABAA);
        }
    }
    dbg!(std::time::Instant::now() - start);
    //
    // let input = "aaaa";
    // let chars: Vec<char> = input.chars().collect();
    // assert_eq!(MyToken::lexer().lex(&chars).unwrap(), MyToken::AAAA);
}

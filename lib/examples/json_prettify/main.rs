use parce::prelude::*;

#[derive(Debug)]
#[parce(JsonLexer)]
enum JsonToken {
    OBrace = '{',
    CBrace = '}',
    OBracket = '[',
    CBracket = ']',
    Comma = ',',
    Colon = ':',
    Quote = '"',
    True = "true",
    False = "false",
    Null = "null",
    Else = p!(.)
}

const INPUT: &str = r#"
[ ]
{ }
, : "
true, false, null
"#;

fn main() {
    dbg!(INPUT.chars().lex::<JsonLexer>().collect::<Vec<_>>());
}

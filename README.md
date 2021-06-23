[![version](https://img.shields.io/crates/v/parce)](https://crates.io/crates/parce)
[![downloads](https://img.shields.io/crates/d/parce)](https://crates.io/crates/parce)
[![docs](https://img.shields.io/docsrs/parce)](https://docs.rs/parce/)
![license](https://img.shields.io/crates/l/parce)

# parce

Parce is a parser and lexer generator, where the grammar and the parse tree are the same
data structure. It is similar to ANTLR, but the grammar is written in Rust code, not a
special DSL.

## Links

- [crates.io](https://crates.io/crates/parce)
- [docs.rs](https://docs.rs/parce/)

## Features

### Lexer Features

- Regex-like repetition operators
    - The usual `*`, `+`, and `?`
    - And also `{n}` (exactly n), `{n,}` (n or more), and `{n,m}` (between n and m inclusive)    <- ANTLR doesn't have those :)
- Lexeme nesting
- Regex-like character classes
- Skipped lexemes
- Fragment lexemes
- Modal lexers
    - unlike ANTLR, lexemes can be active in multiple modes

### Parser Features

## Future plans

- Semantic predicates
    - left-recursive grammar re-writing like ANTLR, using semantic predicates
- Data post-processors
- multi-threaded lexing and parsing.

## Contributing

If you find a bug or want a new feature, please create an issue or pull request on [GitHub](https://github.com/JoelCourtney/parce)!

License: MIT OR Apache-2.0

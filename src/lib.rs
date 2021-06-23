//! Parce a is parser and lexer generator, where the grammar and the parse tree are the same
//! data structure. It is similar to ANTLR, but the grammar is written in Rust code, not a
//! special DSL.
//!
//! # Quick Links to Documentation
//!
//! If you're new to Parce, read the example below, then go to these links to learn more.
//!
//! - [Lexer Attribute Macro](parce_macros::lexer)
//! - [Parser Attribute Macro](parce_macros::parser)
//! - [Lexer Trait](crate::lexer::Lexer)
//! - [Parse Trait](crate::parser::Parse)
//!
//! # Basic Example
//!
//! Parce is used in three steps:
//! - Create a lexer
//! - Create a parser that uses the lexer
//! - Parse things with the parser!
//!
//! ## Creating a lexer
//!
//! A lexer is a single enum with the [lexer attribute macro](macro@parce_macros::lexer) applied to it. Rules for each
//! lexeme are written as a string literal discriminant which get turned into matching logic by the
//! macro. The example below is just to give you a feel for the syntax; the real docs for lexer creation
//! are under the [lexer attribute macro](macro@parce_macros::lexer).
//!
//! ```
//! use parce::prelude::*;
//!
//! #[lexer(MyLexer)]
//! enum MyLexemes {
//!     Bool = " 'true' | 'false' ", // match string literals, use | for multiple possible patterns
//!     Digit = "[0-9]", // use Regex-like character classes
//!     And = '&', // can omit double quotes if pattern is a single character
//!     #[skip] Whitespace = "[ \n\r\t]" // skippable lexemes
//! }
//!
//! // The macro generates an enum "MyLexer" that implements the Lexer trait.
//! ```
//!
//! ## Creating a parser
//!
//! A parser is a collection of enums with the [parser attribute macro](macro@parce_macros::parser) applied to it.
//! Each enum is a grammar rule, and the variants of the enum are productions. Again, the rules for
//! each production are written as a string literal discriminant which is processed by the parser macro.
//! The example below is just to give you a feel for the syntax; the real docs for parser creation are
//! under the [parser attribute macro](macro@parce_macros::parser).
//!
//! TODO after left-recursion rewriting
//!
//! # Features
//!
//! ## Lexer Features
//!
//! - Regex-like repetition operators
//!     - The usual `*`, `+`, and `?`
//!     - And also `{n}` (exactly n), `{n,}` (n or more), and `{n,m}` (between n and m inclusive)    <- ANTLR doesn't have those :)
//! - Lexeme nesting
//! - Regex-like character classes
//! - Skipped lexemes
//! - Fragment lexemes
//! - Modal lexers
//!     - unlike ANTLR, lexemes can be active in multiple modes
//!
//! ## Parser Features
//!
//! # Comparison to ANTLR
//!
//! Since Parce and ANTLR serve very similar purposes, here are the pros and cons of using Parce over ANTLR:
//!
//! **Pros:**
//! - Parce operates directly on the syntax tree enums that you create. ANTLR generates it's own
//!   tree, and if you don't want to use that one, you have to convert it to your own.
//! - Rust's enums make Parce's usage more intuitive for people who are unfamiliar with ANTLR.
//! - ANTLR currently doesn't have a stable Rust target.
//!
//! **Cons:**
//! - ANTLR's runtime performance is faster.
//!     - I haven't actually benchmarked it, but Parce is extremely unlikely to be faster, given how
//!       much smarter the ANTLR devs are than me ;)
//! - ANTLR's grammars are language-independent, as long as you don't embed code in your grammars.
//! - ANTLR has more features.
//!     - Mixed lexer/parse grammars.
//!     - (there are others, but I don't know what they are off the top of my head.)
//!
//! # Future plans
//!
//! - Semantic predicates
//!     - left-recursive grammar re-writing like ANTLR, using semantic predicates
//! - Data post-processors
//! - multi-threaded lexing and parsing.
//!
//! # Contributing
//!
//! If you find a bug or want a new feature, please create an issue or pull request on [GitHub](https://github.com/JoelCourtney/parce)!

pub mod internal_prelude;
pub mod prelude;
pub mod lexer;
pub mod parser;
pub mod error;

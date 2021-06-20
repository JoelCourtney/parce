//! Parce is parser and lexer generator, where the grammar and the parse tree are the same
//! data structure. It is similar to ANTLR, but the grammar is written in Rust code, not a
//! special DSL.
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
//! A parce lexer is a single enum with the `lexer` attribute macro applied to it. Rules for each
//! lexeme are written as a string literal discriminant which get turned into matching logic by the
//! macro.
//!
//! ```
//! use parce::*;
//!
//! #[lexer(MyLexer)]
//! enum MyLexemes {
//!     AB = " 'ab' ", // match string literals with single quotes
//!     ABC = " AB 'c' ", // nest other lexemes
//!     XYOrZ = " [x-z] " // you can use regex-like character classes
//! }
//!
//! // You don't have to use the lexer manually, this is just for demonstration:
//! let mut lexer = MyLexer::default();
//! let lexemes = lexer.lex("abcaby").unwrap();
//! /*
//! lexemes now contains: vec![
//!     ABC     matched on "abc",
//!     AB      matched on "ab",
//!     XYOrZ   matched on "y"
//! ]
//!  */
//! ```
//!
//! # Features
//!
//! ## Lexer Features
//!
//! - Regex-like repetition operators
//!     - `*`, `+`, and `?`
//!     - `{#}`, `{#,}`, and `{#,#}`    <- ANTLR doesn't have those :)
//! - Lexeme nesting
//! - Regex-like character classes
//! - Skipped lexemes
//! - Fragment lexemes
//! - Modal lexers
//!     - unlike ANTLR, lexemes can be active in multiple modes
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

pub mod reexports;
pub mod lexer;
pub mod parser;
pub mod error;

pub use parce_macros::{lexer, parser};
pub use crate::lexer::Lexer;
pub use crate::parser::Parse;
pub use crate::parser::ParseCompletion;


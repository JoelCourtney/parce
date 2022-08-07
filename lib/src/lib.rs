//! # parce
//!
//! Create fast idiomatic lexers and parsers in pure safe Rust.
//!
//! ## Getting Started
//!
//! Add the following to your Cargo.toml:
//!
//! ```toml
//! [dependencies]
//! parce = "0.0.1"
//! ```
//!
//! If you'd like to use async streams with parce, you can enable the `async` feature:
//!
//! ```toml
//! [dependencies]
//! parce = { version = "0.0.1", features = "async" }
//! ```
//!
//! Below is a simple [Brainfuck](https://en.wikipedia.org/wiki/Brainfuck) lexer/parser example. For more
//! details, see the [`lexer`](crate::lexer) and [`parser`](crate::parser) documentation.
//!
//! ```
//! use parce::prelude::*;
//!
//! #[lexer(Lexer)]
//! #[derive(PartialEq, Eq)]
//! enum Token {
//!     // Each of the following match a single specific character
//!     ShiftRight = '>',
//!     ShiftLeft = '<',
//!     Increment = '+',
//!     Decrement = '-',
//!     Output = '.',
//!     Input = ',',
//!     StartLoop = '[',
//!     EndLoop = ']',
//!
//!     // Matches spaces, tabs, newlines, and carriage returns, and ignores them in the output.
//!     #[skip]
//!     IgnoreElse = p!(.)
//! }
//!
//! fn main() {
//!     let input = "[ hello there ] >>.[]";
//!     assert_eq!(2, 2);
//! }
//! ```
// #[parser(Lexer)]
// #[pattern(0 = Atom*)]
// struct Program(Vec<Atom>);
//
// #[parser(Lexer)]
// enum Atom {
//     Operation(Lexeme) = p!(0 = (
//         | ShiftRight | ShiftLeft | Increment | Decrement | Output | Input
//     )),
//     Loop(Vec<Atom>) = p!(StartLoop 0=(Atom*) EndLoop)
// }
// ```

// #![doc(test(attr(deny(warnings))))]

pub mod compatability;

pub mod prelude;
pub mod iterator;

use crate::iterator::{BufferedIterator};

#[derive(Debug, PartialEq, Eq)]
pub struct Lexeme<T> {
    pub start: usize,
    pub length: usize,
    pub token: T
}

impl<T: PartialEq> PartialEq<T> for Lexeme<T> {
    fn eq(&self, other: &T) -> bool {
        self.token == *other
    }
}

pub trait Lexer: Default {
    type Input: Eq + Copy;
    type Output;

    fn lex_from_slice(&mut self, input: &[Self::Input]) -> Option<Lexeme<Self::Output>>;
    fn lex_from_buffered_iter<Iter: Iterator<Item=Self::Input>>(&mut self, input: &mut BufferedIterator<Self::Input, Iter>) -> Option<Lexeme<Self::Output>>;
}

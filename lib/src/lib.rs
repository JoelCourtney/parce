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
//! use parce::{lexer};
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

#[cfg(feature = "logos")]
pub mod compatability;
pub mod iterator;

#[doc(inline)]
pub use parce_macros::lexer;

use crate::iterator::{BufferedIterator, IteratorSourceIterator, SliceSourceIterator};

pub trait Token {
    type Lexer: Lexer;
    fn lexer() -> Self::Lexer;
}

#[derive(Debug, PartialEq, Eq)]
pub struct Lexeme<T: Token> {
    pub start: usize,
    pub length: usize,
    pub token: T
}

impl<T: Token + PartialEq> PartialEq<T> for Lexeme<T> {
    fn eq(&self, other: &T) -> bool {
        self.token == *other
    }
}

pub trait Lexer: Default {
    type Input: Eq + Copy;
    type Output: Token;

    fn lex_from_slice(&mut self, input: &[Self::Input]) -> Option<Lexeme<Self::Output>>;
    fn lex_from_buffered_iter<Iter: Iterator<Item=Self::Input>>(&mut self, input: &mut BufferedIterator<Self::Input, Iter>) -> Option<Lexeme<Self::Output>>;
}

pub trait SliceToLexerExt {
    type Item;
    fn lex<'a, L: Lexer<Input=Self::Item>>(self) -> SliceSourceIterator<'a, L> where Self: 'a + Sized;
}

pub trait IteratorToLexerExt: Iterator {
    fn lex<L: Lexer<Input=Self::Item>>(self) -> IteratorSourceIterator<L, Self> where Self: Sized, Self::Item: Copy + Default;
}

pub trait StrToLexerExt {
    fn lex<'a, L: Lexer<Input=char>>(self) -> IteratorSourceIterator<L, std::str::Chars<'a>> where Self: 'a;
}


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
//! #[parce(Lexer)]
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
//!     IgnoreElse = p!(.)
//! }
//!
//! fn main() {
//!     let _input = "[ hello there ] >>.[]";
//!     assert_eq!(2, 2);
//! }
//! ```
// #[parser(Lexer)]
// #[pattern(0 = Atom*)]
// struct Program(Vec<Atom>);
//
// #[parser(Lexer)]
// enum Atom {
//     Operation(Sentence) = p!(0 = (
//         | ShiftRight | ShiftLeft | Increment | Decrement | Output | Input
//     )),
//     Loop(Vec<Atom>) = p!(StartLoop 0=(Atom*) EndLoop)
// }
// ```

#![doc(test(attr(deny(warnings))))]

pub mod compatability;

pub mod prelude;
pub mod iterator;

use std::ops::Range;
use crate::iterator::{BufferedIterator};
use shrinkwraprs::Shrinkwrap;

#[derive(Debug, PartialEq, Eq, Shrinkwrap)]
pub struct Sentence<T> {
    #[shrinkwrap(main_field)]
    pub data: T,
    pub span: Range<usize>
}

impl<T: PartialEq> PartialEq<T> for Sentence<T> {
    fn eq(&self, other: &T) -> bool {
        self.data == *other
    }
}

pub trait Parce: Default {
    type Input: Eq + Copy;
    type Output;

    fn process_slice(&mut self, input: &[Self::Input]) -> Option<Sentence<Self::Output>>;
    fn process_buffered_iter<Iter: Iterator<Item=Self::Input>>(&mut self, input: &mut BufferedIterator<Self::Input, Iter>) -> Option<Sentence<Self::Output>>;
}

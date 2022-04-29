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
//! If you'd like to use async streams with parce, you can enable the `stream` feature:
//!
//! ```toml
//! [dependencies]
//! parce = { version = "0.0.1", features = "stream" }
//! ```
//!
//! Below is a simple [Brainfuck](https://en.wikipedia.org/wiki/Brainfuck) lexer/parser example. For more
//! details, see the [`lexer`](crate::lexer) and [`parser`](crate::parser) documentation.
//!
//! ```
//! use parce::{lexer};
//!
//! #[lexer(Lexer)]
//! enum Lexeme {
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
//!     Whitespace = p!(
//!         | ' '
//!         | "\t"
//!         | "\n"
//!         | "\r"
//!     )
//!
//!     // TODO: ignore comments
//! }
//! fn main() {
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

#[doc(inline)]
pub use parce_macros::lexer;

#[cfg(test)]
mod tests;

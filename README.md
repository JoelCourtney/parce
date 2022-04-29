[![crates.io](https://img.shields.io/crates/v/parce.svg)](https://crates.io/crates/parce)
[![downloads](https://img.shields.io/crates/d/parce)](https://crates.io/crates/parce)
[![docs](https://img.shields.io/docsrs/parce)](https://docs.rs/parce/)
[![Dependency status](https://deps.rs/repo/github/orium/cargo-rdme/status.svg)](https://deps.rs/repo/github/orium/cargo-rdme)
[![Github stars](https://img.shields.io/github/stars/JoelCourtney/parce.svg?logo=github)](https://github.com/orium/cargo-rdme/stargazers)
[![License](https://img.shields.io/crates/l/parce.svg)](./LICENSE.md)

<!-- cargo-rdme start -->

# parce

Create fast idiomatic lexers and parsers in pure safe Rust.

## Getting Started

Add the following to your Cargo.toml:

```toml
[dependencies]
parce = "0.0.1"
```

If you'd like to use async streams with parce, you can enable the `stream` feature:

```toml
[dependencies]
parce = { version = "0.0.1", features = "stream" }
```

Below is a simple [Brainfuck](https://en.wikipedia.org/wiki/Brainfuck) lexer/parser example. For more
details, see the `lexer` and `parser` documentation.

```rust
use parce::{lexer};

#[lexer(Lexer)]
enum Lexeme {
    // Each of the following match a single specific character
    ShiftRight = '>',
    ShiftLeft = '<',
    Increment = '+',
    Decrement = '-',
    Output = '.',
    Input = ',',
    StartLoop = '[',
    EndLoop = ']',

    // Matches spaces, tabs, newlines, and carriage returns, and ignores them in the output.
    #[skip]
    Whitespace = p!(
        | ' '
        | "\t"
        | "\n"
        | "\r"
    )

    // TODO: ignore comments
}
fn main() {
    assert_eq!(2, 2);
}
```

<!-- cargo-rdme end -->

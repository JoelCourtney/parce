#[doc(inline)]
pub use parce_macros::lexer;

pub use crate::{
    Lexeme,
    Lexer,
    compatability::ConvertToParceExt,
    iterator::{
        IteratorToLexerExt,
        SliceToLexerExt,
        StrToLexerExt
    }
};

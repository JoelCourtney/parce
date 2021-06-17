pub use lazy_static::lazy_static;
pub use regex::Regex;

pub use tinyvec::{tiny_vec, array_vec, TinyVec, ArrayVec};

pub use crate::parser::{*, automata};
pub use crate::lexer::*;

pub use core::any::TypeId as Rule;
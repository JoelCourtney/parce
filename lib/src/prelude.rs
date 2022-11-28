#[doc(inline)]
pub use parce_macros::parce;

pub use crate::{
    Sentence,
    Parce,
    compatability::ConvertToParceExt,
    iterator::{
        IteratorToParceExt,
        SliceToParceExt,
        StrToParceExt
    }
};

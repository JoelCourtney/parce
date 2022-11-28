use crate::Sentence;

pub trait ConvertToParceExt: Iterator {
    type Iterator: Iterator<Item=Sentence<Self::Item>>;
    fn to_parce(self) -> Self::Iterator;
}

#[cfg(feature = "logos")]
impl<'source, T: logos::Logos<'source>> ConvertToParceExt for logos::Lexer<'source, T> {
    type Iterator = std::iter::Map<logos::SpannedIter<'source, T>, fn((T, std::ops::Range<usize>)) -> Sentence<T>>;

    fn to_parce(self) -> Self::Iterator {
        self.spanned().map(|(data, span)| Sentence { data, span })
    }
}

#[cfg(test)]
#[cfg(feature = "logos")]
mod tests {
    use ::logos::*;
    use crate::Sentence;
    use crate::compatability::ConvertToParceExt;

    #[derive(Logos, PartialEq, Eq, Debug)]
    enum Token {
        #[token("aa")] AA,
        #[token("ab")] AB,
        #[error] Error
    }

    #[test]
    fn logos_to_parce() {
        assert_eq!(
            Token::lexer("aaab").to_parce().collect::<Vec<_>>(),
            vec![
                Sentence { data: Token::AA, span: 0..2 },
                Sentence { data: Token::AB, span: 2..4 },
            ]
        )
    }
}

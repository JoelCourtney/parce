use crate::Lexeme;

pub trait ConvertToParceExt: Iterator {
    type Iterator: Iterator<Item=Lexeme<Self::Item>>;
    fn to_parce(self) -> Self::Iterator;
}

#[cfg(feature = "logos")]
impl<'source, T: logos::Logos<'source>> ConvertToParceExt for logos::Lexer<'source, T> {
    type Iterator = std::iter::Map<logos::SpannedIter<'source, T>, fn((T, std::ops::Range<usize>)) -> Lexeme<T>>;

    fn to_parce(self) -> Self::Iterator {
        self.spanned().map(|(token, span)| Lexeme {
            token,
            start: span.start,
            length: span.end - span.start
        })
    }
}

#[cfg(test)]
#[cfg(feature = "logos")]
mod tests {
    use ::logos::*;
    use crate::Lexeme;
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
                Lexeme { token: Token::AA, start: 0, length: 2 },
                Lexeme { token: Token::AB, start: 2, length: 2 },
            ]
        )
    }
}

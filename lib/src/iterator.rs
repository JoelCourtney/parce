use std::collections::VecDeque;
use std::str::Chars;
use crate::{Lexeme, Lexer};

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

pub struct IteratorSourceIterator<L: crate::Lexer<Input=I::Item>, I: Iterator> where I::Item: Copy {
    pub(crate) lexer: L,
    pub(crate) iter: BufferedIterator<L::Input, I>
}

impl<L: crate::Lexer, I: Iterator<Item=L::Input>> Iterator for IteratorSourceIterator<L, I> {
    type Item = Lexeme<L::Output>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.lex_from_buffered_iter(&mut self.iter)
    }
}

impl<T: Iterator> IteratorToLexerExt for T where T::Item: Copy + Default {
    fn lex<L: Lexer<Input=T::Item>>(self) -> IteratorSourceIterator<L, Self> {
        IteratorSourceIterator {
            lexer: L::default(),
            iter: BufferedIterator::new(self)
        }
    }
}

impl StrToLexerExt for &str {
    fn lex<'a, L: Lexer<Input=char>>(self) -> IteratorSourceIterator<L, Chars<'a>> where Self: 'a {
        IteratorSourceIterator {
            lexer: L::default(),
            iter: BufferedIterator::new(self.chars())
        }
    }
}

impl<I> SliceToLexerExt for &[I] {
    type Item = I;

    fn lex<'a, L: Lexer<Input=I>>(self) -> SliceSourceIterator<'a, L> where Self: 'a {
        SliceSourceIterator {
            lexer: L::default(),
            slice: self,
            index: 0
        }
    }
}

pub struct SliceSourceIterator<'a, L: Lexer> {
    pub(crate) lexer: L,
    pub(crate) slice: &'a [L::Input],
    pub(crate) index: usize
}

impl<L: Lexer> Iterator for SliceSourceIterator<'_, L> {
    type Item = Lexeme<L::Output>;

    fn next(&mut self) -> Option<Self::Item> {
        let lexeme = self.lexer.lex_from_slice(&self.slice[self.index..]);
        if let Some(ref l) = lexeme {
            self.index += l.length;
        }
        lexeme
    }
}

pub struct BufferedIterator<Item: Copy, Iter: Iterator<Item=Item>> {
    iter: Iter,
    buffer: VecDeque<Item>
}

impl<Item: Copy + Default, Iter: Iterator<Item=Item>> BufferedIterator<Item, Iter> {
    pub(crate) fn new(iter: Iter) -> Self {
        Self {
            iter,
            buffer: VecDeque::new()
        }
    }

    pub fn next(&mut self) -> Option<Item> {
        self.buffer.pop_front()
            .or_else(|| self.iter.next())
    }

    pub fn next_chunk<const N: usize>(&mut self) -> Option<[Item; N]> {
        let buffer_len = self.buffer.len();
        let mut result = [Item::default(); N];
        if buffer_len >= N {
            let mut index = 0;
            self.buffer.drain(..N).for_each(|item| {
                result[index] = item;
                index += 1;
            });
            return Some(result);
        } else {
            let mut index = buffer_len;
            while let Some(item) = self.iter.next() {
                result[index] = item;
                index += 1;
                if index == N {
                    if buffer_len != 0 {
                        index = 0;
                        self.buffer.drain(..).for_each(|item| {
                            result[index] = item;
                            index += 1;
                        });
                    }
                    return Some(result);
                }
            }
            return None;
        }
    }
}

use std::collections::VecDeque;
use std::str::Chars;
use crate::{Parce, Sentence};

pub trait SliceToParceExt {
    type Item;
    fn lex<'a, P: Parce<Input=Self::Item>>(self) -> SliceSourceIterator<'a, P> where Self: 'a + Sized;
}

pub trait IteratorToParceExt: Iterator {
    fn lex<P: Parce<Input=Self::Item>>(self) -> IteratorSourceIterator<P, Self> where Self: Sized, Self::Item: Copy + Default;
}

pub trait StrToParceExt {
    fn lex<'a, P: Parce<Input=char>>(self) -> IteratorSourceIterator<P, std::str::Chars<'a>> where Self: 'a;
}

pub struct IteratorSourceIterator<P: Parce<Input=I::Item>, I: Iterator> where I::Item: Copy {
    pub(crate) processor: P,
    pub(crate) iter: BufferedIterator<P::Input, I>
}

impl<P: Parce, I: Iterator<Item=P::Input>> Iterator for IteratorSourceIterator<P, I> {
    type Item = Sentence<P::Output>;

    fn next(&mut self) -> Option<Self::Item> {
        self.processor.process_buffered_iter(&mut self.iter)
    }
}

impl<T: Iterator> IteratorToParceExt for T where T::Item: Copy + Default {
    fn lex<P: Parce<Input=T::Item>>(self) -> IteratorSourceIterator<P, Self> {
        IteratorSourceIterator {
            processor: P::default(),
            iter: BufferedIterator::new(self)
        }
    }
}

impl StrToParceExt for &str {
    fn lex<'a, P: Parce<Input=char>>(self) -> IteratorSourceIterator<P, Chars<'a>> where Self: 'a {
        IteratorSourceIterator {
            processor: P::default(),
            iter: BufferedIterator::new(self.chars())
        }
    }
}

impl<I> SliceToParceExt for &[I] {
    type Item = I;

    fn lex<'a, P: Parce<Input=I>>(self) -> SliceSourceIterator<'a, P> where Self: 'a {
        SliceSourceIterator {
            processor: P::default(),
            slice: self,
            index: 0
        }
    }
}

pub struct SliceSourceIterator<'a, P: Parce> {
    pub(crate) processor: P,
    pub(crate) slice: &'a [P::Input],
    pub(crate) index: usize
}

impl<P: Parce> Iterator for SliceSourceIterator<'_, P> {
    type Item = Sentence<P::Output>;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.processor.process_slice(&self.slice[self.index..]);
        if let Some(ref l) = result {
            self.index += l.span.len();
        }
        result
    }
}

pub struct BufferedIterator<Item, Iter: Iterator<Item=Item>> {
    iter: Iter,
    buffer: VecDeque<Item>,
    peek_cursor: usize
}

impl<Item, Iter: Iterator<Item=Item>> BufferedIterator<Item, Iter> {
    pub(crate) fn new(iter: Iter) -> Self {
        Self {
            iter,
            buffer: VecDeque::new(),
            peek_cursor: 0
        }
    }

    pub fn peek_chunk(&mut self, n: usize) -> Option<&[Item]> {
        let pull_from_iterator = (self.peek_cursor + n).saturating_sub(self.buffer.len());
        for _ in 0..pull_from_iterator {
            if let Some(item) = self.iter.next() {
                self.buffer.push_back(item);
            } else {
                return None;
            }
        }
        self.peek_cursor += n;
        Some(&self.buffer.make_contiguous()[self.peek_cursor - n..self.peek_cursor])
    }

    pub fn reset_to(&mut self, n: usize) {
        self.buffer.drain(..n);
        self.peek_cursor = 0;
    }
}

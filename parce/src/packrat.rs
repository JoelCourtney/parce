use std::marker::PhantomData;

trait Rat {
    type Input: Copy;
    type Output;
    type Continue: Rat<Input=Self::Input, Output=Self::Output>;

    fn step(input: Self::Input) -> Step<Self::Output>;
}

enum Step<O> {
    Continue,
    Die,
    Success(O)
}

// generated
#[inline]
fn race2<I:Copy,Iter:Iterator<Item=I>,O,Rat0:Rat<Input=I,Output=O>,Rat1:Rat<Input=I,Output=O>>(mut input: Iter) -> Option<O> {
    let first = input.next().unwrap();
    match (Rat0::step(first), Rat1::step(first)) {
        (Step::Continue, Step::Continue) => race2::<_,_,_,Rat0::Continue,Rat1::Continue>(input),
        (Step::Continue, Step::Die) => race1::<_,_,_,Rat0::Continue>(input),
        (Step::Die, Step::Continue) => race1::<_,_,_,Rat1::Continue>(input),
        (Step::Die, Step::Die) => {
            None
        }
        (Step::Success(succ), Step::Continue) => race1::<_,_,_,Rat1::Continue>(input).or(Some(succ)),
        (Step::Continue, Step::Success(succ)) => race1::<_,_,_,Rat0::Continue>(input).or(Some(succ)),
        (Step::Success(succ), _) => Some(succ),
        (Step::Die, Step::Success(succ)) => Some(succ)
    }
}

#[inline]
fn race1<I:Copy,Iter:Iterator<Item=I>,O,Rat0:Rat<Input=I,Output=O>>(mut input: Iter) -> Option<O> {
    let first = input.next().unwrap();
    match Rat0::step(first) {
        Step::Continue => race1::<_,_,_,Rat0::Continue>(input),
        Step::Die => None,
        Step::Success(succ) => Some(succ)
    }
}

#[derive(PartialEq, Eq, Debug)]
enum FinderResult {
    FoundAB,
    FoundAAB
}

struct CannotContinue<I,O>(PhantomData<(I,O)>);
impl<I:Copy,O> Rat for CannotContinue<I,O> {
    type Input = I;
    type Output = O;
    type Continue = CannotContinue<I,O>;

    #[inline]
    fn step(_: I) -> Step<O> {
        unreachable!()
    }
}

struct FindsAB0;
struct FindsAB1;
impl Rat for FindsAB0 {
    type Input = char;
    type Output = FinderResult;
    type Continue = FindsAB1;

    #[inline]
    fn step(input: char) -> Step<FinderResult> {
        if input == 'a' {
            Step::Continue
        } else {
            Step::Die
        }
    }
}
impl Rat for FindsAB1 {
    type Input = char;
    type Output = FinderResult;
    type Continue = CannotContinue<char,FinderResult>;

    #[inline]
    fn step(input: char) -> Step<FinderResult> {
        if input == 'b' {
            Step::Success(FinderResult::FoundAB)
        } else {
            Step::Die
        }
    }
}

struct FindsAAB0;
struct FindsAAB1;
struct FindsAAB2;
impl Rat for FindsAAB0 {
    type Input = char;
    type Output = FinderResult;
    type Continue = FindsAAB1;

    #[inline]
    fn step(input: char) -> Step<FinderResult> {
        if input == 'a' {
            Step::Continue
        } else {
            Step::Die
        }
    }
}
impl Rat for FindsAAB1 {
    type Input = char;
    type Output = FinderResult;
    type Continue = FindsAAB2;

    #[inline]
    fn step(input: char) -> Step<FinderResult> {
        if input == 'a' {
            Step::Continue
        } else {
            Step::Die
        }
    }
}
impl Rat for FindsAAB2 {
    type Input = char;
    type Output = FinderResult;
    type Continue = CannotContinue<char,FinderResult>;

    #[inline]
    fn step(input: char) -> Step<FinderResult> {
        if input == 'b' {
            Step::Success(FinderResult::FoundAAB)
        } else {
            Step::Die
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use logos::Logos;

    #[derive(Logos, Debug, PartialEq, Eq)]
    enum LogosFinder {
        #[token("ab")]
        AB,

        #[token("aab")]
        AAB,

        #[error]
        Error
    }

    #[test]
    fn finders() {
        let mut input = "aab";
        for _ in 0..1000000000 {
            assert_eq!(race2::<_, _, _, FindsAB0, FindsAAB0>("ab".chars()), Some(FinderResult::FoundAB))
            // assert_eq!(LogosFinder::lexer(input).next(), Some(LogosFinder::AAB))
        }
    }
}
use std::marker::PhantomData;

trait Rat {
    type Input: Copy;
    type Output;
    type Continue: Rat<Input=Self::Input, Output=Self::Output>;

    fn step(self, input: Self::Input) -> Step<Self::Output,Self::Continue>;
}

enum Step<O,C> {
    Continue(C),
    Die,
    Success(O)
}

// generated
#[inline]
fn race2<I:Copy,O>(input: &[I], rat0: impl Rat<Input=I,Output=O>, rat1: impl Rat<Input=I,Output=O>) -> Option<O> {
    let first = input[0];
    match (rat0.step(first), rat1.step(first)) {
        (Step::Continue(r0), Step::Continue(r1)) => race2(&input[1..],r0,r1),
        (Step::Continue(r0), Step::Die) => race1(&input[1..], r0),
        (Step::Die, Step::Continue(r1)) => race1(&input[1..], r1),
        (Step::Die, Step::Die) => {
            None
        }
        (Step::Success(succ), Step::Continue(r1)) => race1(&input[1..], r1).or(Some(succ)),
        (Step::Continue(r0), Step::Success(succ)) => race1(&input[1..], r0).or(Some(succ)),
        (Step::Success(succ), _) => Some(succ),
        (Step::Die, Step::Success(succ)) => Some(succ)
    }
}

#[inline]
fn race1<I:Copy,O>(input: &[I], rat0: impl Rat<Input=I,Output=O>) -> Option<O> {
    let first = input[0];
    match rat0.step(first) {
        Step::Continue(r0) => race1(&input[1..], r0),
        Step::Die => None,
        Step::Success(succ) => Some(succ)
    }
}

#[derive(PartialEq, Eq, Debug)]
enum FinderResult {
    FoundAB,
    FoundAAB,
    FoundBAB
}

struct CannotContinue<I,O>(PhantomData<(I,O)>);
impl<I:Copy,O> Rat for CannotContinue<I,O> {
    type Input = I;
    type Output = O;
    type Continue = CannotContinue<I,O>;

    #[inline]
    fn step(self, _: I) -> Step<O,CannotContinue<I,O>> {
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
    fn step(self, input: char) -> Step<FinderResult,FindsAB1> {
        if input == 'a' {
            Step::Continue(FindsAB1)
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
    fn step(self, input: char) -> Step<FinderResult,Self::Continue> {
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
    fn step(self, input: char) -> Step<FinderResult,Self::Continue> {
        if input == 'a' {
            Step::Continue(FindsAAB1)
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
    fn step(self, input: char) -> Step<FinderResult,Self::Continue> {
        if input == 'a' {
            Step::Continue(FindsAAB2)
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
    fn step(self, input: char) -> Step<FinderResult,Self::Continue> {
        if input == 'b' {
            Step::Success(FinderResult::FoundAAB)
        } else {
            Step::Die
        }
    }
}

enum FindsEither<L:Rat,R:Rat<Input=L::Input,Output=L::Output>> {
    Both(L,R),
    Left(L),
    Right(R)
}

impl<L:Rat,R:Rat<Input=L::Input,Output=L::Output>> Rat for FindsEither<L,R> {
    type Input = L::Input;
    type Output = ();
    type Continue = FindsEither<L::Continue, R::Continue>;

    #[inline]
    fn step(self, input: L::Input) -> Step<(),FindsEither<L::Continue,R::Continue>> {
        match self {
            Self::Both(rat0, rat1) => {
                match (rat0.step(input), rat1.step(input)) {
                    (Step::Continue(rl), Step::Continue(rr)) => {
                        Step::Continue(FindsEither::Both(rl, rr))
                    }
                    (Step::Die, Step::Continue(rr)) => {
                        Step::Continue(FindsEither::Right(rr))
                    }
                    (Step::Continue(rl), Step::Die) => {
                        Step::Continue(FindsEither::Left(rl))
                    }
                    (Step::Die, Step::Die) => Step::Die,
                    (Step::Success(_), _) => {
                        Step::Success(())
                    }
                    (_, Step::Success(_)) => Step::Success(())
                }
            }
            Self::Left(rat0) => {
                match rat0.step(input) {
                    Step::Continue(rl) => {
                        Step::Continue(FindsEither::Left(rl))
                    }
                    Step::Die => Step::Die,
                    Step::Success(_) => Step::Success(())
                }
            }
            Self::Right(rat0) => {
                match rat0.step(input) {
                    Step::Continue(rr) => {
                        Step::Continue(FindsEither::Right(rr))
                    }
                    Step::Die => Step::Die,
                    Step::Success(_) => Step::Success(())
                }
            }
        }
    }
}
// ba*b
// baac
fn dumb_parser(chars: &[char]) -> Option<FinderResult> {
    let mut index = 0;
    match chars[index] {
        'a' => loop {
            index += 1;
            break match chars[index] {
                'a' => loop {
                    index += 1;
                    break match chars[index] {
                        'b' => Some(FinderResult::FoundAAB),
                        _ => None
                    }
                }
                'b' => Some(FinderResult::FoundAB),
                _ => None
            }
        }
        'b' => loop {
            index += 1;
            break match chars[index] {
                'a' => { continue }
                'b' => Some(FinderResult::FoundBAB),
                _ => None
            }
        }
        _ => None
    }
    //match chars[index] {
    //         'b' => loop {
    //             index += 1;
    //             break match chars[index] {
    //                 'a' => { continue }
    //                 'b' => Some(FinderResult::FoundBASB),
    //                 _ => None
    //             }
    //         }
    //         _ => None
    //     }
}


#[cfg(test)]
mod tests {
    use super::*;
    use logos::Logos;
    use std::time::Instant;


    #[derive(Logos, Debug, PartialEq, Eq)]
    enum LogosFinder {
        // #[token("ab")]
        // AB,
        //
        // #[token("aab")]
        // AAB,

        #[regex("ba*b")]
        BAB,

        #[error]
        Error
    }

    #[test]
    fn finders() {
        let mut input = String::new();
        let stdin = std::io::stdin(); // We get `Stdin` here.
        stdin.read_line(&mut input).unwrap();
        let start = Instant::now();
        let chars: Vec<char> = input.chars().collect();
        for _ in 0..1000000000 {
            // assert_eq!(race2(&chars, FindsAB0, FindsAAB0), Some(FinderResult::FoundAAB))
            // assert_eq!(race1(&chars, FindsEither::Both(FindsAB0, FindsAAB0)), Some(()));
            assert_eq!(LogosFinder::lexer(&input).next(), Some(LogosFinder::BAB));
            // assert_eq!(dumb_parser(&chars), Some(FinderResult::FoundBAB));
        }
        dbg!(Instant::now() - start);
    }
}
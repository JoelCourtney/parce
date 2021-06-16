use std::any::TypeId;
use crate::lexer::Lexeme;
use std::hash::{Hash, Hasher};
use typed_arena::Arena;
use core::any::TypeId as Rule;
use shrinkwraprs::Shrinkwrap;
use std::borrow::Borrow;

#[derive(Debug, Eq, PartialEq)]
pub struct ParserError;

pub trait Parser<L: Copy + Eq> {
    fn parse(input: &Vec<Lexeme<L>>) -> Result<Vec<Self>, ParserError> where Self: Sized;
}

#[derive(Shrinkwrap)]
struct Army<'a>(Arena<Automaton<'a>>);
impl<'a> Army<'a> {
    fn new() -> Army<'a> {
        Army(Arena::new())
    }

    fn act(&'a self, auto: &'a mut Automaton<'a>, actions: Vec<AutomatonAction>) -> (Vec<*mut Automaton>, bool) {
        use AutomatonAction::*;
        use AutomatonState::*;

        let mut new_recruits: Vec<*mut Automaton> = vec![];

        let mut victory = false;

        for action in actions {
            match action {
                Advance => auto.step += 1,
                Recruit(rule, prod) => {
                    let mut new = self.recruit(rule, prod);
                    new.parents.push(self.alloc(auto.clone()));
                    new_recruits.push(new);
                }
                Wait => {
                    auto.state = Waiting;
                }
                Victory => unsafe {
                    let mut parents: Vec<_> = auto.parents.drain(..).collect();
                    if parents.len() == 0 {
                        victory = true;
                    }
                    for parent in &parents {
                        (**parent).state = Alive;
                        (**parent).step += 1;
                        (**parent).children.push(auto);
                    }
                    new_recruits.extend(parents);
                }
                Die => {
                    auto.state = Dead;
                }
            }
        }

        (new_recruits, victory)
    }

    fn recruit(&'a self, rule: Rule, prod: u32) -> &mut Automaton {
        self.alloc(Automaton::new(rule, prod))
    }
}

#[derive(Clone, Debug)]
pub struct Automaton<'a> {
    pub rule: Rule,
    pub production: u32,
    pub state: AutomatonState,
    pub step: u32,
    pub parents: Vec<*mut Automaton<'a>>,
    pub children: Vec<*mut Automaton<'a>>
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub enum AutomatonState {
    Alive,
    Waiting,
    Dead
}

impl Automaton<'_> {
    fn new<'a>(rule: TypeId, production: u32) -> Automaton<'a> {
        Automaton {
            rule,
            production,
            state: AutomatonState::Alive,
            step: 0,
            parents: vec![],
            children: vec![]
        }
    }

    fn from<'a>(other: &'a Automaton<'a>) -> Automaton<'a> {
        Automaton {
            parents: vec![],
            children: vec![],
            ..*other
        }
    }
}

impl PartialEq for Automaton<'_> {
    fn eq(&self, other: &Automaton) -> bool {
        self.rule == other.rule &&
            self.production == other.production &&
            self.state == other.state &&
            self.step == other.step
    }
}
impl Eq for Automaton<'_> {}

pub trait AutomatonCommander<T: Eq + Copy> {
    fn step(auto: &Automaton, lexeme: Lexeme<T>) -> Vec<AutomatonAction>;
    fn assemble(auto: &Automaton) -> Vec<AutomatonStep>;
}

pub enum AutomatonStep {
    Number(u32),
    Child
}

/// Actions that Automata can do at each step.
///
/// They can do as many of these as they need at each step.
#[derive(Debug, Copy, Clone)]
pub enum AutomatonAction {
    /// Increase step by one
    Advance,

    /// Start a new automaton with this as its parent
    Recruit(TypeId, u32),

    /// Put this to sleep, wait to be woken by children
    Wait,

    /// Wake copies of parents, increase their steps by 1, add this as their child
    Victory,

    /// time to die :)
    Die
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum MyLexeme {
    A,
    B
}

#[derive(Debug)]
enum MyGrammar {
    Thing // "A (B | B B) A
}

impl<L: Copy + Eq, C: AutomatonCommander<L>> Parser<L> for C {
    fn parse(lexemes: &Vec<Lexeme<L>>) -> Result<Vec<Self>, ParserError> where Self: Sized {
        let mut army: Army = Army::new();
        let mut alive: Vec<*mut Automaton>= vec![];
        alive.push(army.recruit(Rule::of::<MyGrammar>(), 0));

        let mut last = None;

        let mut i = 0;
        while !alive.is_empty() {
            let mut j = 0;
            let mut new_recruits = vec![];
            while j < alive.len() {
                let auto = alive[j];
                unsafe {
                    match (*auto).state {
                        AutomatonState::Alive => {
                            println!("\n{} {}", (*auto).production, (*auto).step);

                            let actions = dbg!(Self::step(&*auto, lexemes[i]));
                            let (new, victory) = army.act(&mut *auto, actions);
                            new_recruits.extend(new);
                            if victory {
                                last = Some(auto);
                            }
                            j += 1;
                        }
                        _ => {
                            dbg!(&*auto);
                            alive.remove(j);
                        }
                    }
                }
            }
            alive.extend(new_recruits);
            i += 1;
        }

        unsafe {dbg!(&*(last.unwrap()));}

        todo!()
    }
}

#[test]
fn parse() {
    MyGrammar::parse(&vec![
        Lexeme {
            data: MyLexeme::A,
            start: 0,
            len: 1
        },
        Lexeme {
            data: MyLexeme::B,
            start: 1,
            len: 1
        },
        Lexeme {
            data: MyLexeme::B,
            start: 2,
            len: 1
        },
        Lexeme {
            data: MyLexeme::A,
            start: 3,
            len: 1
        },
    ]);
}

impl AutomatonCommander<MyLexeme> for MyGrammar {
    fn step(auto: &Automaton, lexeme: Lexeme<MyLexeme>) -> Vec<AutomatonAction> {
        use AutomatonAction::*;

        if auto.rule == Rule::of::<MyGrammar>() {
            match auto.production {
                0 => {
                    match auto.step {
                        0 => if lexeme == MyLexeme::A {
                            vec![
                                Recruit(Rule::of::<MyGrammar>(), 1),
                                Recruit(Rule::of::<MyGrammar>(), 2),
                                Wait
                            ]
                        } else {
                            vec![Die]
                        }
                        1 => if lexeme == MyLexeme::A {
                            vec![Victory, Die]
                        } else {
                            vec![Die]
                        }
                        _ => panic!("only two states")
                    }
                }
                1 => {
                    match auto.step {
                        0 => if lexeme == MyLexeme::B {
                            vec![Victory, Die]
                        } else {
                            vec![Die]
                        }
                        _ => panic!("only 1 state")
                    }
                }
                2 => {
                    match auto.step {
                        0 => if lexeme == MyLexeme::B {
                            vec![Advance]
                        } else {
                            vec![Die]
                        }
                        1 => if lexeme == MyLexeme::B {
                            vec![Victory, Die]
                        } else {
                            vec![Die]
                        }
                        _ => panic!("only 2 states")
                    }
                }
                _ => panic!("only 3 productions")
            }
        } else {
            panic!("only one rule")
        }
    }

    fn assemble(auto: &Automaton) -> Vec<AutomatonStep> {
        use AutomatonStep::*;

        if auto.rule == Rule::of::<MyGrammar>() {
            match auto.production {
                0 => vec![Number(1), Child, Number(1)],
                1 => vec![Number(1)],
                2 => vec![Number(2)],
                _ => panic!("only three productions")
            }
        } else {
            panic!("only one rule")
        }
    }
}
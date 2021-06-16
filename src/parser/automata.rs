use typed_arena::Arena;
use core::any::TypeId as Rule;
use shrinkwraprs::Shrinkwrap;
use tinyvec::{TinyVec, tiny_vec};
use std::ptr::null_mut;

#[derive(Clone, Debug)]
pub struct Automaton<'a> {
    pub rule: Rule,
    pub production: u32,
    pub state: u32,
    pub parents: TinyVec<[Rawtomaton<'a>; 2]>,
    pub children: TinyVec<[Rawtomaton<'a>; 2]>
}

#[derive(Shrinkwrap, Debug, Copy, Clone)]
pub struct Rawtomaton<'a>(*mut Automaton<'a>);

impl Default for Rawtomaton<'_> {
    fn default() -> Self {
        Rawtomaton(null_mut())
    }
}

impl<'a> From<*mut Automaton<'a>> for Rawtomaton<'a> {
    fn from(a: *mut Automaton<'a>) -> Self {
        Rawtomaton(a)
    }
}

impl Automaton<'_> {
    fn new<'a>(rule: Rule, production: u32) -> Automaton<'a> {
        Automaton {
            rule,
            production,
            state: 0,
            parents: tiny_vec![],
            children: tiny_vec![]
        }
    }
}

impl PartialEq for Automaton<'_> {
    fn eq(&self, other: &Automaton) -> bool {
        self.rule == other.rule &&
            self.production == other.production &&
            self.state == other.state
    }
}
impl Eq for Automaton<'_> {}

/// Commands that Automata can execute at each step.
///
/// They can do as many of these as they need at each step.
#[derive(Debug, Copy, Clone)]
pub enum AutomatonCommand {
    /// Start a new automaton with this as its parent
    Recruit(Rule, u32),

    /// Increase state by one
    Advance,

    /// Wake copies of parents, increase their state by 1, add this as their child
    Victory,

    /// time to die :)
    Die
}

#[derive(Shrinkwrap)]
pub struct Army<'a>(Arena<Automaton<'a>>);

impl<'a> Army<'a> {
    pub fn new() -> Army<'a> {
        Army(Arena::with_capacity(10))
    }

    pub unsafe fn act(&'a self, auto: *mut Automaton<'a>, actions: Vec<AutomatonCommand>) -> (Vec<*mut Automaton>, bool, bool) {
        use AutomatonCommand::*;

        let mut new_recruits: Vec<*mut Automaton> = vec![];

        let mut victory = false;
        let mut remove = false;

        let mut clone: Option<*mut Automaton> = None;
        let mut get_clone = move || unsafe {
            if let Some(c) = clone {
                c
            } else {
                let c = self.alloc((*auto).clone()) as *mut Automaton;
                clone = Some(c);
                c
            }
        };

        for action in actions {
            match action {
                Recruit(rule, prod) => {
                    let new = self.recruit(rule, prod);
                    (*new).parents.push(get_clone().into());
                    new_recruits.push(new);
                }
                Victory => {
                    let parents: Vec<*mut Automaton> = (*auto).parents.iter().map(
                        |p| self.alloc((***p).clone()) as *mut Automaton
                    ).collect();
                    if parents.len() == 0 {
                        victory = true;
                    } else {
                        for parent in &parents {
                            (**parent).state += 1;
                            (**parent).children.push(get_clone().into());
                        }
                        new_recruits.extend(parents);
                    }
                }
                Advance => (*auto).state += 1,
                Die => {
                    remove = true;
                }
            }
        }

        (new_recruits, victory, remove)
    }

    pub fn recruit(&'a self, rule: Rule, prod: u32) -> *mut Automaton {
        self.alloc(Automaton::new(rule, prod))
    }
}


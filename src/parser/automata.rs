use typed_arena::Arena;
use core::any::TypeId as Rule;
use shrinkwraprs::Shrinkwrap;
use tinyvec::{TinyVec, tiny_vec, ArrayVec};
use std::ptr::null_mut;

#[derive(Clone, Debug)]
pub struct Automaton<'a> {
    pub rule: Rule,
    pub route: u32,
    pub state: u32,
    pub parent: Option<(Rawtomaton<'a>, Continuation)>,
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

impl<'a> From<&mut Automaton<'a>> for Rawtomaton<'a> {
    fn from(a: &mut Automaton<'a>) -> Self {
        Rawtomaton(a)
    }
}

impl Automaton<'_> {
    fn new<'a>(rule: Rule, route: u32) -> Automaton<'a> {
        Automaton {
            rule,
            route,
            state: 0,
            parent: None,
            children: tiny_vec![]
        }
    }
}

impl PartialEq for Automaton<'_> {
    fn eq(&self, other: &Automaton) -> bool {
        self.rule == other.rule &&
            self.route == other.route &&
            self.state == other.state
    }
}
impl Eq for Automaton<'_> {}

/// Commands that Automata can execute at each step.
///
/// They can do as many of these as they need at each step.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AutomatonCommand {
    Recruit {
        rule: Rule,
        route: u32,
        how_many: u32,
        on_victory: Continuation
    },

    /// Increase state by one
    Advance,

    Victory,

    Reset(u32),

    /// time to die :)
    Die
}

impl Default for AutomatonCommand {
    fn default() -> AutomatonCommand {
        AutomatonCommand::Die
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Continuation {
    Advance,
    Die,
    Reset(u32)
}

#[derive(Shrinkwrap)]
pub struct Army<'a>(Arena<Automaton<'a>>);

#[derive(Default)]
pub struct CommandResult<'a> {
    pub new_recruits: TinyVec<[Rawtomaton<'a>; 4]>,
    pub reactivated: TinyVec<[Rawtomaton<'a>; 4]>,
    pub victorious: Option<Rawtomaton<'a>>,
    pub remove: bool
}

impl<'a> Army<'a> {
    pub fn new() -> Army<'a> {
        Army(Arena::with_capacity(10))
    }

    pub fn recruit(&'a self, rule: Rule, route: u32) -> Rawtomaton {
        self.alloc(Automaton::new(rule, route)).into()
    }

    pub unsafe fn command(&'a self, auto: Rawtomaton<'a>, actions: ArrayVec<[AutomatonCommand; 2]>) -> CommandResult<'a> {
        use AutomatonCommand::*;

        let mut clone: Option<Rawtomaton> = None;
        let mut get_clone = move || unsafe {
            if let Some(c) = clone {
                c
            } else {
                let c = self.alloc((**auto).clone()).into();
                clone = Some(c);
                c
            }
        };

        let mut result = CommandResult::default();

        let mut die = actions.contains(&AutomatonCommand::Die);
        for action in actions {
            match action {
                Advance => {
                    (**auto).state += 1;
                }
                Die => {
                    result.remove = true;
                }
                Reset(n) => {
                    (**auto).state = n;
                }
                Recruit {
                    rule,
                    route,
                    how_many,
                    on_victory
                } => {
                    for i in 0..how_many {
                        let new = self.recruit(rule, route + i);
                        if die {
                            (**new).parent = Some((auto, on_victory));
                            die = false;
                        } else {
                            (**new).parent = Some((get_clone(), on_victory));
                        }
                        result.new_recruits.push(new);
                    }
                }
                Victory => {
                    let mut auto = auto;
                    loop {
                        match (**auto).parent {
                            Some((parent, cont)) => {
                                match cont {
                                    Continuation::Die => {
                                        auto = parent;
                                    }
                                    Continuation::Reset(n) => {
                                        (**parent).state = n;
                                        if die {
                                            (**parent).children.push(auto);
                                        } else {
                                            (**parent).children.push(get_clone());
                                        }
                                        result.reactivated.push(parent);
                                        auto = parent;
                                    }
                                    Continuation::Advance => {
                                        (**parent).state += 1;
                                        if die {
                                            (**parent).children.push(auto);
                                        } else {
                                            (**parent).children.push(get_clone());
                                        }
                                        result.reactivated.push(parent);
                                        break;
                                    }
                                }
                            }
                            None => {
                                result.victorious = Some(auto);
                                break;
                            }
                        }
                    }
                }
            }
        }
        result
    }
}

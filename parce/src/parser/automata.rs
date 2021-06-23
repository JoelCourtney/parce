//! Contains the automata used by the parser algorithm, and their basic operations.

use typed_arena::Arena;
use core::any::TypeId as Rule;
use shrinkwraprs::Shrinkwrap;
use tinyvec::{TinyVec, tiny_vec, ArrayVec};
use std::ptr::null_mut;

/// Represents the full state of a DFA used in the parser.
#[derive(Clone, Debug)]
pub struct Automaton<'a> {
    /// The rule this automaton is parsing
    pub rule: Rule,
    /// The route it is parsing. See [Parser](crate::parser::Parser) for details on routes.
    pub route: u32,
    /// The current state of the automaton
    pub state: u32,
    /// The lexeme index that this automaton started parsing at.
    ///
    /// This is used in the assembly phase by Star and Question.
    pub lexeme_start: usize,
    /// If this automaton is a child, this is a pointer to its parent.
    pub parent: Option<(Rawtomaton<'a>, Continuation)>,
    /// After being successfully reawakened by a child, it is added to this vec
    pub children: TinyVec<[Rawtomaton<'a>; 2]>
}

/// A newtype wrapper for a raw pointer to an [Automaton].
#[derive(Shrinkwrap, Debug, Copy, Clone, Eq, PartialEq)]
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
    fn new<'a>(rule: Rule, route: u32, lexeme_start: usize) -> Automaton<'a> {
        Automaton {
            rule,
            route,
            state: 0,
            lexeme_start,
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
/// They can do as many of these as they need at each step, but in practice they never need
/// more than three, so they are passed around in array_vecs of length 3.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AutomatonCommand {
    /// Spawns a new child (or children), whose state 0 will be evaluated on this same lexeme.
    Spawn {
        /// Which grammar rule to use
        rule: Rule,
        /// Lowest route to use
        route: u32,
        /// How many children to spawn. If greater than one, they are spawned on consecutive routes.
        how_many: u32,
        /// What to do if a child declares victory.
        on_victory: Continuation
    },

    /// Increase state by one
    Advance,

    /// Increase state by one. If this automaton is a child, reactivate the parent and follow its
    /// on_victory instructions. Otherwise, a successful parse match was found.
    Victory,

    /// Deactivate the automaton
    Die,

    /// Makes the parser re-evaluate this automaton immediately after this step. This is used for
    /// the star and question operators, because they need to spawn automata, then move to the next
    /// state and evaluate that state all on one lexeme. *Must* be used together with either Advance
    /// or Victory.
    Fallthrough
}

impl Default for AutomatonCommand {
    fn default() -> AutomatonCommand {
        AutomatonCommand::Die
    }
}

/// What to do when an automaton's child declares victory.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Continuation {
    /// Pass the victory to another level up, and die
    PassDie,
    /// Pass the victory to another level up, but also stay alive and advance
    PassAdvance,
    /// Don't pass on the victory, and advance
    Advance,
}

/// A wrapper around a typed arena of Automata. Handles the AutomatonCommands because they often
/// require memory allocations.
#[derive(Shrinkwrap)]
pub(super) struct Army<'a>(Arena<Automaton<'a>>);

impl<'a> Army<'a> {
    pub fn new() -> Army<'a> {
        Army(Arena::with_capacity(10))
    }

    pub fn spawn(&'a self, rule: Rule, route: u32, lexeme_start: usize) -> Rawtomaton {
        self.alloc(Automaton::new(rule, route, lexeme_start)).into()
    }

    pub(crate) unsafe fn command(&'a self, auto: Rawtomaton<'a>, actions: ArrayVec<[AutomatonCommand; 3]>, lexeme_index: usize) -> CommandResult<'a> {
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

        for action in &actions {
            match action {
                Advance => {
                    (**auto).state += 1;
                }
                Die => {
                    result.remove = true;
                }
                Spawn {
                    rule,
                    route,
                    how_many,
                    on_victory
                } => {
                    let mut die = actions.contains(&AutomatonCommand::Die);
                    for i in 0..*how_many {
                        let new = self.spawn(*rule, route + i, lexeme_index);
                        if die {
                            (**new).parent = Some((auto, *on_victory));
                            die = false;
                        } else {
                            (**new).parent = Some((get_clone(), *on_victory));
                        }
                        result.new_spawns.push(new);
                    }
                }
                Victory => {
                    let mut die = actions.contains(&AutomatonCommand::Die);
                    let mut auto = auto;
                    (**auto).state += 1;
                    loop {
                        match (**auto).parent {
                            Some((mut parent, cont)) => {
                                if die {
                                    (**parent).state += 1;
                                    (**parent).children.push(auto);
                                } else {
                                    parent = self.alloc((**parent).clone()).into();
                                    (**parent).state += 1;
                                    (**parent).children.push(self.alloc((**auto).clone()).into());
                                }
                                match cont {
                                    Continuation::PassDie => {
                                        auto = parent;
                                        die = true;
                                    }
                                    Continuation::PassAdvance => {
                                        result.reactivated.push(parent);
                                        auto = parent;
                                        die = false;
                                    }
                                    Continuation::Advance => {
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
                Fallthrough => {
                    result.fallthrough = true
                }
            }
        }
        result
    }
}

#[derive(Default)]
pub(super) struct CommandResult<'a> {
    pub new_spawns: TinyVec<[Rawtomaton<'a>; 4]>,
    pub reactivated: TinyVec<[Rawtomaton<'a>; 4]>,
    pub victorious: Option<Rawtomaton<'a>>,
    pub remove: bool,
    pub fallthrough: bool
}
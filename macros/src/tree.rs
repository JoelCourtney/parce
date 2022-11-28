use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

#[derive(Debug, Clone)]
pub enum Tree {
    Match {
        arms: Vec<(SliceMatcher, Box<Tree>)>,
        length: usize
    },
    // Loop(Box<Tree>),
    // Break(Box<Tree>),
    Ok {
        result: TokenStream,
        priority: usize
    },
    Err
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum SliceMatcher {
    If(Vec<CharMatcher>),
    Else
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum CharMatcher {
    Literal(char),
    Any
}

#[derive(Copy, Clone)]
pub enum ItemSource {
    Slice,
    BufferedIter
}

impl Tree {
    pub(crate) fn to_tokens(&self, index: impl ToTokens + Clone, source: ItemSource) -> impl ToTokens {
        match self {
            Tree::Match { arms, length} => {
                let (matchers, results): (Vec<_>, Vec<_>) = arms.iter().map(|(matcher, tree)| {
                    match matcher {
                        SliceMatcher::If(_) => (matcher, tree.to_tokens(quote! {#index + #length}, source)),
                        SliceMatcher::Else => (matcher, tree.to_tokens(quote! { #index }, source))
                    }
                }).unzip();
                let match_expr = match source {
                    ItemSource::Slice => quote! {input.get(#index..#index+#length)},
                    ItemSource::BufferedIter => quote! {input.peek_chunk( #length )}
                };
                quote! {
                    match #match_expr {
                        #(
                            #matchers => {
                                #results
                            }
                        )*
                    }
                }
            }
            // Tree::Loop(_) => {todo!()}
            // Tree::Break(_) => todo!(),
            Tree::Ok { result, .. } => {
                let drop_expr = match source {
                    ItemSource::Slice => quote! {},
                    ItemSource::BufferedIter => quote! { input.reset_to( #index ); }
                };
                quote! {
                    #drop_expr
                    Some(parce::Sentence {
                        data: #result,
                        span: 0..#index
                    })
                }
            }
            Tree::Err => {
                let drop_expr = match source {
                    ItemSource::Slice => quote! {},
                    ItemSource::BufferedIter => quote! { input.reset_to( 0 ); }
                };
                quote! {
                    #drop_expr
                    None
                }
            }
        }
    }

    pub(crate) fn merge(self, other: Tree) -> Tree {
        match (self, other) {
            (
                Tree::Match { arms: mut left_arms, length: left_length },
                Tree::Match { arms: mut right_arms, length: right_length }
            ) => {
                let length = if left_length > right_length {
                    left_arms = left_arms.into_iter().map(|(mut matcher, tree)| {
                        let tree = matcher.split(left_length, right_length, *tree);
                        (matcher, Box::new(tree))
                    }).collect();
                    right_length
                } else if right_length > left_length {
                    right_arms = right_arms.into_iter().map(|(mut matcher, tree)| {
                        let tree = matcher.split(right_length, left_length, *tree);
                        (matcher, Box::new(tree))
                    }).collect();
                    left_length
                } else {
                    left_length
                };
                let mut arms: Vec<_> = left_arms.into_iter().chain(right_arms.into_iter()).collect();
                Self::combine_arms(&mut arms);
                Tree::Match { arms, length }
            }
            (
                mut tree @Tree::Match { .. },
                new@Tree::Ok{..}
            ) | (
                new@Tree::Ok{..},
                mut tree@Tree::Match { .. }
            ) => {
                tree.fallback(new, true);
                tree
            }
            (Tree::Err, tree) | (tree, Tree::Err) => tree,
            (Tree::Ok{ priority: self_priority, result: self_result }, Tree::Ok{ priority: other_priority, result: other_result }) => {
                if self_priority > other_priority {
                    Tree::Ok {
                        result: other_result,
                        priority: other_priority
                    }
                } else {
                    Tree::Ok {
                        result: self_result,
                        priority: other_priority
                    }
                }
            }
        }
    }

    fn fallback(&mut self, other: Tree, same_depth: bool) {
        match self {
            Tree::Match { ref mut arms, .. } => {
                for (matcher, tree) in arms {
                    tree.fallback(other.clone(), *matcher == SliceMatcher::Else && same_depth);
                }
            }
            Tree::Ok { priority, .. } => {
                match other {
                    Tree::Ok { priority: new_priority, .. } if same_depth && dbg!(new_priority) < dbg!(*priority) => {
                        *self = other;
                    }
                    _ => {}
                }
            }
            Tree::Err => *self = other,
            // _ => todo!("asdf")
        }
    }

    pub(crate) fn squash(&mut self) {
        match self {
            Tree::Match { arms, length } => {
                let length_option = arms.iter().filter_map(|(_, tree)| tree.squashable_length()).min();
                length_option.map(|squash_length| {
                    if squash_length > 0 {
                        *arms = arms.iter().cloned().flat_map(|(matcher, tree)| {
                            match *tree {
                                Tree::Err => vec![(matcher, tree)],
                                Tree::Match { arms: sub_arms, length: sub_length } => {
                                    sub_arms.into_iter().map(move |(mut sub_matcher, sub_tree)| {
                                        let tree = sub_matcher.split(sub_length, squash_length, *sub_tree);
                                        (matcher.concatenate(sub_matcher), Box::new(tree))
                                    }).collect()
                                }
                                _ => unreachable!("asdf")
                            }
                        }).collect();
                        *length += squash_length;
                    }
                });
                for (_, tree) in arms.iter_mut() {
                    tree.squash();
                }
                Self::combine_arms(arms);
            }
            _ => {}
        }
    }

    fn squashable_length(&self) -> Option<usize> {
        match self {
            Tree::Match { length, arms } => {
                let mut squashable = true;
                for (matcher, tree) in arms {
                    match (matcher, tree.as_ref()) {
                        (SliceMatcher::Else, Tree::Ok{..}) => squashable = false,
                        _ => {}
                    }
                }
                if squashable {
                    Some(*length)
                } else {
                    Some(0)
                }
            }
            Tree::Ok{..} => Some(0),
            Tree::Err => None
            // _ => todo!("match length")
        }
    }

    pub(crate) fn prune_err(&mut self) {
        match self {
            Tree::Match { arms, .. } => {
                let mut not_err = false;
                for (_, tree) in arms {
                    tree.prune_err();
                    match **tree {
                        Tree::Err => {}
                        _ => not_err = true
                    }
                }
                if !not_err {
                    *self = Tree::Err;
                }
            }
            _ => {}
        }
    }

    pub(crate) fn deprioritize(self) -> Self {
        match self {
            Tree::Match { mut arms, length } => {
                arms = arms.into_iter().map(|(matcher, tree)| (matcher, Box::new(tree.deprioritize()))).collect();
                Tree::Match { arms, length }
            }
            Tree::Ok { result, priority } => Tree::Ok {
                result,
                priority: priority + 1
            },
            Tree::Err => self
        }
    }

    fn combine_arms(arms: &mut Vec<(SliceMatcher, Box<Tree>)>) {
        arms.sort_by(|l, r| l.0.cmp(&r.0));
        let mut i = 0;
        while i < arms.len() - 1 {
            if arms[i].0 == arms[i+1].0 {
                let left = arms.remove(i);
                let right = arms.remove(i);
                arms.insert(i, (left.0, Box::new(left.1.merge(*right.1))));
            } else {
                i += 1;
            }
        }
    }
}

impl SliceMatcher {
    pub(crate) fn any(length: usize) -> Self {
        SliceMatcher::If(vec![CharMatcher::Any; length])
    }
    pub(crate) fn literal(lit: String) -> Self {
        SliceMatcher::If(lit.chars().map(|c| CharMatcher::Literal(c)).collect())
    }

    fn split(&mut self, old_length: usize, new_length: usize, tree: Tree) -> Tree {
        if old_length == new_length {
            return tree;
        }
        match self {
            SliceMatcher::Else => tree,
            SliceMatcher::If(chars) => {
                let remainder = chars.split_off(new_length);
                Tree::Match {
                    arms: vec![
                        (SliceMatcher::If(remainder), Box::new(tree)),
                        (SliceMatcher::Else, Box::new(Tree::Err))
                    ],
                    length: old_length - new_length
                }
            }
        }
    }

    fn concatenate(&self, other: Self) -> Self {
        match (self, other) {
            (SliceMatcher::If(self_vec), SliceMatcher::If(other_vec)) => {
                let mut new_vec = self_vec.clone();
                new_vec.extend(other_vec);
                SliceMatcher::If(new_vec)
            }
            (_, SliceMatcher::Else) => SliceMatcher::Else,
            _ => unreachable!("cannot concatenate SliceMatcher::Else")
        }
    }
}

impl ToTokens for SliceMatcher {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let result = match self {
            SliceMatcher::Else => quote! { _ },
            SliceMatcher::If(chars) => {
                if chars.iter().all(|m| m == &CharMatcher::Any) {
                    quote! { Some([..]) }
                } else {
                    quote! { Some([#(#chars), *]) }
                }
            }
        };
        result.to_tokens(tokens);
    }
}

impl ToTokens for CharMatcher {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let result = match self {
            CharMatcher::Literal(c) => quote! { #c },
            CharMatcher::Any => quote! { _ }
        };
        result.to_tokens(tokens);
    }
}

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

#[derive(Clone)]
pub enum Tree {
    Match {
        arms: Vec<(SliceMatcher, Box<Tree>)>,
        length: usize
    },
    // Loop(Box<Tree>),
    // Break(Box<Tree>),
    Ok(TokenStream),
    Err
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum SliceMatcher {
    If(Vec<CharMatcher>),
    Else
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum CharMatcher {
    Literal(char),
    Any
}

impl Tree {
    fn to_tokens_indexed(&self, previous_index: impl ToTokens + Clone, index: impl ToTokens + Clone) -> impl ToTokens {
        match self {
            Tree::Match { arms, length} => {
                let (matchers, results): (Vec<_>, Vec<_>) = arms.iter().map(|(matcher, tree)| {
                    (matcher, tree.to_tokens_indexed(index.clone(), quote! {#index + #length}))
                }).unzip();
                quote! {
                    match input.get(#index..#index + #length) {
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
            Tree::Ok(token) => {
                quote! {
                    Ok(parce::Lexeme {
                        span: &input[0..#index],
                        token: #token
                    })
                }
            }
            Tree::Err => {
                quote! {
                    Err(#previous_index)
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
                arms.sort_by(|l, r| l.0.cmp(&r.0));
                let mut i = 0;
                while i < arms.len() - 1 {
                    if arms[i].0 == arms[i+1].0 {
                        let left = arms.remove(i);
                        let right = arms.remove(i);
                        arms.insert(i, (left.0, Box::new(left.1.merge(*right.1))));
                    }
                    i += 1;
                }
                Tree::Match { arms, length }
            }
            (
                mut tree @Tree::Match { .. },
                Tree::Ok(token)
            ) | (
                Tree::Ok(token),
                mut tree@Tree::Match { .. }
            ) => {
                tree.override_failure(token);
                tree
            }
            (Tree::Err, tree) | (tree, Tree::Err) => tree,
            _ => todo!()
        }
    }

    fn override_failure(&mut self, token: TokenStream) {
        match self {
            Tree::Match { ref mut arms, .. } => {
                for (_, tree) in arms {
                    tree.override_failure(token.clone());
                }
            }
            Tree::Ok(_) => {}
            Tree::Err => *self = Tree::Ok(token),
            // _ => todo!("asdf")
        }
    }

    pub(crate) fn squash(&mut self) {
        match self {
            Tree::Match { arms, length } => {
                let length_option = arms.iter().map(|(_, tree)| tree.match_length()).min();
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
                for (_, tree) in arms {
                    tree.squash();
                }
            }
            _ => {}
        }
    }

    fn match_length(&self) -> usize {
        match self {
            Tree::Match { length, .. } => *length,
            Tree::Ok(_) | Tree::Err => 0,
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
            _ => unreachable!("cannot concatenate SliceMatcher::Else")
        }
    }
}

impl ToTokens for Tree {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.to_tokens_indexed(quote! {0}, quote! {0}).to_tokens(tokens);
    }
}

impl ToTokens for SliceMatcher {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let result = match self {
            SliceMatcher::Else => quote! { _ },
            SliceMatcher::If(chars) => {
                if chars.iter().all(|m| m == &CharMatcher::Any) {
                    quote! { Some(_) }
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

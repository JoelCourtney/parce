use crate::Ast;
use proc_macro::TokenStream;
use proc_macro2::{Delimiter, TokenTree};
use proc_macro_error::{abort, abort_call_site};
use syn::buffer::Cursor;
use syn::parse::{Parse, ParseStream};

#[derive(Debug)]
enum DiscriminantLexeme {
    Literal(String),
    Ident(String),
    Group(TokenStream),
    Star,
    Plus,
    Question,
    Dot,
    Pipe,
}

impl Parse for Ast {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lexemes = input.step(|cursor| {
            let mut cursor = *cursor;
            let mut lexemes = vec![];

            while let Some((tt, next)) = cursor.token_tree() {
                lexemes.push((
                    match &tt {
                        TokenTree::Literal(lit) => {
                            let as_string = lit.to_string();
                            if as_string.starts_with('"') && as_string.ends_with('"')
                                || as_string.len() == 3
                                    && as_string.starts_with('\'')
                                    && as_string.ends_with('\'')
                            {
                                DiscriminantLexeme::Literal(
                                    as_string[1..as_string.len() - 1].to_string(),
                                )
                            } else {
                                abort!(
                                    tt.span(),
                                    "Unsupported literal; must be string or char literal"
                                );
                            }
                        }
                        TokenTree::Ident(ident) => {
                            DiscriminantLexeme::Ident(ident.to_string())
                        }
                        TokenTree::Punct(punct) => match punct.as_char() {
                            '*' => DiscriminantLexeme::Star,
                            '+' => DiscriminantLexeme::Plus,
                            '?' => DiscriminantLexeme::Question,
                            '.' => DiscriminantLexeme::Dot,
                            '|' => DiscriminantLexeme::Pipe,
                            _ => abort!(tt.span(), "Unsupported punctuation"),
                        },
                        TokenTree::Group(group) => match group.delimiter() {
                            Delimiter::Parenthesis => {
                                DiscriminantLexeme::Group(group.stream().clone().into())
                            }
                            Delimiter::Bracket => todo!(),
                            Delimiter::Brace => todo!(),
                            Delimiter::None => abort!(group.clone(), "how"),
                        },
                    },
                    tt.clone(),
                ));
                cursor = next;
            }

            assert!(cursor.eof());

            Ok((lexemes, Cursor::empty()))
        })?;

        parse_lexer_lexemes(&lexemes)
    }
}

fn parse_lexer_lexemes(
    lexemes: &[(DiscriminantLexeme, TokenTree)],
) -> syn::Result<Ast> {
    let mut index = 0;
    let mut group = vec![];
    loop {
        match lexemes.get(index) {
            Some((DiscriminantLexeme::Pipe, tt)) => {
                if !group.is_empty() {
                    abort!(
                                tt, "| must also be used at the start of an alternation";
                                help = "Try inserting a | in front of the first alternative.";
                                example = "Example:\n      Instead of  l!('a' | 'b')\n      Use         l!(| 'a' | 'b')\n                     ^"
                            )
                }
                let mut alternatives = vec![];
                let mut last_pipe = 0;
                for i in 1..lexemes.len() {
                    if let (DiscriminantLexeme::Pipe, _) = lexemes[i] {
                        alternatives.push(parse_lexer_lexemes(&lexemes[last_pipe + 1..i])?);
                        last_pipe = i;
                    }
                }
                alternatives.push(parse_lexer_lexemes(&lexemes[last_pipe + 1..])?);
                index = lexemes.len();
                group.push(Ast::Or(alternatives));
            }
            Some((DiscriminantLexeme::Literal(lit), _)) => {
                match lexemes.get(index + 1) {
                    Some((DiscriminantLexeme::Star, _)) => {
                        index += 2;
                        group.push(Ast::Star(Box::new(Ast::Literal(
                            lit.to_string(),
                        ))));
                    }
                    Some((DiscriminantLexeme::Plus, _)) => {
                        index += 2;
                        group.push(Ast::Plus(Box::new(Ast::Literal(
                            lit.to_string(),
                        ))));
                    }
                    Some((DiscriminantLexeme::Question, _)) => {
                        index += 2;
                        group.push(Ast::Question(Box::new(Ast::Literal(
                            lit.to_string(),
                        ))));
                    }
                    _ => {
                        index += 1;
                        group.push(Ast::Literal(lit.to_string()));
                    }
                }
            }
            Some((DiscriminantLexeme::Ident(ident), _)) => {
                match lexemes.get(index + 1) {
                    Some((DiscriminantLexeme::Star, _)) => {
                        index += 2;
                        group.push(Ast::Star(Box::new(Ast::Ident(
                            ident.to_string(),
                        ))));
                    }
                    Some((DiscriminantLexeme::Plus, _)) => {
                        index += 2;
                        group.push(Ast::Plus(Box::new(Ast::Ident(
                            ident.to_string(),
                        ))));
                    }
                    Some((DiscriminantLexeme::Question, _)) => {
                        index += 2;
                        group.push(Ast::Question(Box::new(Ast::Ident(
                            ident.to_string(),
                        ))));
                    }
                    _ => {
                        index += 1;
                        group.push(Ast::Ident(ident.to_string()));
                    }
                }
            }
            Some((DiscriminantLexeme::Group(stream), _)) => {
                let ast_group = syn::parse(stream.clone())?;
                match lexemes.get(index + 1) {
                    Some((DiscriminantLexeme::Star, _)) => {
                        index += 2;
                        group.push(Ast::Star(Box::new(ast_group)));
                    }
                    Some((DiscriminantLexeme::Plus, _)) => {
                        index += 2;
                        group.push(Ast::Plus(Box::new(ast_group)));
                    }
                    Some((DiscriminantLexeme::Question, _)) => {
                        index += 2;
                        group.push(Ast::Question(Box::new(ast_group)));
                    }
                    _ => {
                        index += 1;
                        group.push(ast_group);
                    }
                }
            }
            Some((DiscriminantLexeme::Star, tt)) => {
                abort!(tt, "Operator must come after literal or group")
            }
            Some((DiscriminantLexeme::Plus, tt)) => {
                abort!(tt, "Operator must come after literal or group")
            }
            Some((DiscriminantLexeme::Question, tt)) => {
                abort!(tt, "Operator must come after literal or group")
            }
            Some((DiscriminantLexeme::Dot, _)) => match lexemes.get(index + 1) {
                Some((DiscriminantLexeme::Star, _)) => {
                    index += 2;
                    group.push(Ast::Star(Box::new(Ast::Dot)));
                }
                Some((DiscriminantLexeme::Plus, _)) => {
                    index += 2;
                    group.push(Ast::Plus(Box::new(Ast::Dot)));
                }
                Some((DiscriminantLexeme::Question, _)) => {
                    index += 2;
                    group.push(Ast::Question(Box::new(Ast::Dot)));
                }
                _ => {
                    index += 1;
                    group.push(Ast::Dot);
                }
            },
            None => break,
        }
    }

    if group.len() > 1 {
        Ok(Ast::Group(group))
    } else if group.len() == 1 {
        Ok(group.pop().unwrap())
    } else {
        abort_call_site!("Discriminant cannot be empty")
    }
}


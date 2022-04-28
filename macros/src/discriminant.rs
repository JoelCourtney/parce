use crate::LexerAst;
use crate::ParserAst;
use proc_macro::TokenStream;
use proc_macro2::{Delimiter, TokenTree};
use proc_macro_error::{abort, abort_call_site};
use syn::buffer::Cursor;
use syn::parse::{Parse, ParseStream};

#[derive(Debug)]
enum LexerDiscriminantLexeme {
    Literal(String),
    Ident(String),
    Group(TokenStream),
    Star,
    Plus,
    Question,
    Dot,
    Pipe,
}

impl Parse for LexerAst {
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
                                LexerDiscriminantLexeme::Literal(
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
                            LexerDiscriminantLexeme::Ident(ident.to_string())
                        }
                        TokenTree::Punct(punct) => match punct.as_char() {
                            '*' => LexerDiscriminantLexeme::Star,
                            '+' => LexerDiscriminantLexeme::Plus,
                            '?' => LexerDiscriminantLexeme::Question,
                            '.' => LexerDiscriminantLexeme::Dot,
                            '|' => LexerDiscriminantLexeme::Pipe,
                            _ => abort!(tt.span(), "Unsupported punctuation"),
                        },
                        TokenTree::Group(group) => match group.delimiter() {
                            Delimiter::Parenthesis => {
                                LexerDiscriminantLexeme::Group(group.stream().clone().into())
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

        fn parse_lexer_lexemes(
            lexemes: &[(LexerDiscriminantLexeme, TokenTree)],
        ) -> syn::Result<LexerAst> {
            let mut index = 0;
            let mut group = vec![];
            loop {
                match lexemes.get(index) {
                    Some((LexerDiscriminantLexeme::Pipe, tt)) => {
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
                            if let (LexerDiscriminantLexeme::Pipe, _) = lexemes[i] {
                                alternatives.push(parse_lexer_lexemes(&lexemes[last_pipe + 1..i])?);
                                last_pipe = i;
                            }
                        }
                        alternatives.push(parse_lexer_lexemes(&lexemes[last_pipe + 1..])?);
                        index = lexemes.len();
                        group.push(LexerAst::Or(alternatives));
                    }
                    Some((LexerDiscriminantLexeme::Literal(lit), _)) => {
                        match lexemes.get(index + 1) {
                            Some((LexerDiscriminantLexeme::Star, _)) => {
                                index += 2;
                                group.push(LexerAst::Star(Box::new(LexerAst::Literal(
                                    lit.to_string(),
                                ))));
                            }
                            Some((LexerDiscriminantLexeme::Plus, _)) => {
                                index += 2;
                                group.push(LexerAst::Plus(Box::new(LexerAst::Literal(
                                    lit.to_string(),
                                ))));
                            }
                            Some((LexerDiscriminantLexeme::Question, _)) => {
                                index += 2;
                                group.push(LexerAst::Question(Box::new(LexerAst::Literal(
                                    lit.to_string(),
                                ))));
                            }
                            _ => {
                                index += 1;
                                group.push(LexerAst::Literal(lit.to_string()));
                            }
                        }
                    }
                    Some((LexerDiscriminantLexeme::Ident(ident), _)) => {
                        match lexemes.get(index + 1) {
                            Some((LexerDiscriminantLexeme::Star, _)) => {
                                index += 2;
                                group.push(LexerAst::Star(Box::new(LexerAst::Ident(
                                    ident.to_string(),
                                ))));
                            }
                            Some((LexerDiscriminantLexeme::Plus, _)) => {
                                index += 2;
                                group.push(LexerAst::Plus(Box::new(LexerAst::Ident(
                                    ident.to_string(),
                                ))));
                            }
                            Some((LexerDiscriminantLexeme::Question, _)) => {
                                index += 2;
                                group.push(LexerAst::Question(Box::new(LexerAst::Ident(
                                    ident.to_string(),
                                ))));
                            }
                            _ => {
                                index += 1;
                                group.push(LexerAst::Ident(ident.to_string()));
                            }
                        }
                    }
                    Some((LexerDiscriminantLexeme::Group(stream), _)) => {
                        let ast_group = syn::parse(stream.clone())?;
                        match lexemes.get(index + 1) {
                            Some((LexerDiscriminantLexeme::Star, _)) => {
                                index += 2;
                                group.push(LexerAst::Star(Box::new(ast_group)));
                            }
                            Some((LexerDiscriminantLexeme::Plus, _)) => {
                                index += 2;
                                group.push(LexerAst::Plus(Box::new(ast_group)));
                            }
                            Some((LexerDiscriminantLexeme::Question, _)) => {
                                index += 2;
                                group.push(LexerAst::Question(Box::new(ast_group)));
                            }
                            _ => {
                                index += 1;
                                group.push(ast_group);
                            }
                        }
                    }
                    Some((LexerDiscriminantLexeme::Star, tt)) => {
                        abort!(tt, "Operator must come after literal or group")
                    }
                    Some((LexerDiscriminantLexeme::Plus, tt)) => {
                        abort!(tt, "Operator must come after literal or group")
                    }
                    Some((LexerDiscriminantLexeme::Question, tt)) => {
                        abort!(tt, "Operator must come after literal or group")
                    }
                    Some((LexerDiscriminantLexeme::Dot, _)) => match lexemes.get(index + 1) {
                        Some((LexerDiscriminantLexeme::Star, _)) => {
                            index += 2;
                            group.push(LexerAst::Star(Box::new(LexerAst::Dot)));
                        }
                        Some((LexerDiscriminantLexeme::Plus, _)) => {
                            index += 2;
                            group.push(LexerAst::Plus(Box::new(LexerAst::Dot)));
                        }
                        Some((LexerDiscriminantLexeme::Question, _)) => {
                            index += 2;
                            group.push(LexerAst::Question(Box::new(LexerAst::Dot)));
                        }
                        _ => {
                            index += 1;
                            group.push(LexerAst::Dot);
                        }
                    },
                    None => break,
                }
            }

            if group.len() > 1 {
                Ok(LexerAst::Group(group))
            } else if group.len() == 1 {
                Ok(group.pop().unwrap())
            } else {
                abort_call_site!("Discriminant cannot be empty")
            }
        }

        parse_lexer_lexemes(&lexemes)
    }
}

impl Parse for ParserAst {
    fn parse(_input: ParseStream) -> syn::Result<Self> {
        todo!()
    }
}

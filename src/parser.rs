#![allow(dead_code)]

use crate::expr::Expr;
use crate::tokens::Token;
use thiserror::Error;

#[derive(Debug, Default)]
struct Parser {
    ast: Vec<Expr>,
}

impl Parser {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn parse<T>(&mut self, token_stream: &mut T) -> Result<&mut Self, ParseError>
    where
        T: Iterator<Item = Token>,
    {
        while let Some(expr) = self.parse_block(token_stream)? {
            self.ast.push(expr);
        }
        Ok(self)
    }

    fn parse_block<T>(&self, token_stream: &mut T) -> Result<Option<Expr>, ParseError>
    where
        T: Iterator<Item = Token>,
    {
        match token_stream.next() {
            Some(Token::LParen) => {
                let mut stack = Vec::new();
                while let Ok(Some(body)) = self.parse_block(token_stream) {
                    if body == Expr::Atom(Token::RParen) {
                        let mut cons_list = Expr::Pair(None, None);
                        while let Some(e) = stack.pop() {
                            if Expr::Pair(None, None) == cons_list {
                                //  base case
                                cons_list = Expr::Pair(Some(Box::new(e)), None);
                            } else {
                                cons_list =
                                    Expr::Pair(Some(Box::new(e)), Some(Box::new(cons_list)));
                            }
                        }

                        return Ok(Some(cons_list));
                    }
                    stack.push(body);
                }

                Err(ParseError::EOF)
            }
            Some(Token::RParen) => Ok(Some(Expr::Atom(Token::RParen))),
            Some(token) if token.is_atom() => Ok(Some(Expr::Atom(token))),
            Some(token) => unreachable!("unhandled token {:?}!", token),
            None => Ok(None),
        }
    }
}

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Unmatched braces")]
    UnmatchedBraces,

    #[error("reached EOF!!!")]
    EOF,
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_atoms() {
        // 3 a 2
        let mut tokens = vec![Token::Number(3), Token::Char('a'), Token::Bool(true)].into_iter();
        let mut parser = Parser::new();
        parser.parse(&mut tokens).unwrap();

        assert_eq!(
            parser.ast,
            vec![
                Expr::Atom(Token::Number(3)),
                Expr::Atom(Token::Char('a')),
                Expr::Atom(Token::Bool(true)),
            ]
        );
    }

    #[test]
    fn test_lists() {
        // ((1 #f) a #t ('c' "a b "))
        let mut tokens = vec![
            Token::LParen,
            Token::LParen,
            Token::Number(1),
            Token::Bool(false),
            Token::RParen,
            Token::Ident("a".to_string()),
            Token::Bool(true),
            Token::LParen,
            Token::Char('c'),
            Token::String("a b ".to_string()),
            Token::RParen,
            Token::RParen,
        ]
        .into_iter();
        let mut parser = Parser::new();
        parser.parse(&mut tokens).unwrap();

        assert_eq!(
            parser.ast,
            vec![Expr::Pair(
                Some(Box::new(Expr::Pair(
                    Some(Box::new(Expr::Atom(Token::Number(1)))),
                    Some(Box::new(Expr::Pair(
                        Some(Box::new(Expr::Atom(Token::Bool(false)))),
                        None
                    )))
                ))),
                Some(Box::new(Expr::Pair(
                    Some(Box::new(Expr::Atom(Token::Ident("a".to_string())))),
                    Some(Box::new(Expr::Pair(
                        Some(Box::new(Expr::Atom(Token::Bool(true)))),
                        Some(Box::new(Expr::Pair(
                            Some(Box::new(Expr::Pair(
                                Some(Box::new(Expr::Atom(Token::Char('c')))),
                                Some(Box::new(Expr::Pair(
                                    Some(Box::new(Expr::Atom(Token::String("a b ".to_string())))),
                                    None
                                )))
                            )),),
                            None
                        )))
                    )))
                )))
            )]
        );
    }
}

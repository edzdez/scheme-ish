#![allow(dead_code)]

use crate::expr::{Atom, Expr};
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
                let mut cons_list = Expr::Pair(None, None);
                let mut stack = Vec::new();
                while let Ok(Some(body)) = self.parse_block(token_stream) {
                    if body == Expr::Atom(Atom::Primitive(Token::RParen)) {
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
            Some(Token::RParen) => Ok(Some(Expr::Atom(Atom::Primitive(Token::RParen)))),
            Some(token) if token.is_atom() => Ok(Some(Expr::Atom(Atom::Primitive(token)))),
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
                Expr::Atom(Atom::Primitive(Token::Number(3))),
                Expr::Atom(Atom::Primitive(Token::Char('a'))),
                Expr::Atom(Atom::Primitive(Token::Bool(true))),
            ]
        );
    }

    #[test]
    fn test_lists() {
        // '( '(1 #f) a #t '( 'c' "a b "))
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

        // assert_eq!(parser.ast, vec![])
        dbg!(&parser.ast);
    }
}

#![allow(dead_code)]

use crate::tokens::Token;
use thiserror::Error;

#[derive(Debug, Clone, Default)]
pub struct Lexer {
    pub tokens: Vec<Token>,
    line: Vec<char>,
    curr_line: usize,
    curr_col: usize,
}

impl Lexer {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn tokenize(&mut self, program: &str) -> Result<&mut Self, LexError> {
        self.line = program.trim().chars().collect();
        self.curr_col = 0;

        while self.curr_col != self.line.len() {
            match self.lex() {
                Some((t, n)) => {
                    self.tokens.push(t);
                    self.curr_col += n;
                }
                None => return Err(LexError::InvalidToken(self.curr_line, self.curr_col)),
            }
            self.skip_whitespace(); // there is no leading whitespace
        }

        self.curr_line += 1;
        Ok(self)
    }

    // @requires self.curr_col != self.line.len()
    fn lex(&mut self) -> Option<(Token, usize)> {
        // parse order: char > quote >> ident (> implies higher precedence)
        // kill me...
        if let Some(res) = self.try_paren() {
            Some(res)
        } else if let Some(res) = self.try_bool() {
            Some(res)
        } else if let Some(res) = self.try_num() {
            Some(res)
        } else if let Some(res) = self.try_char() {
            Some(res)
        } else if let Some(res) = self.try_quote() {
            Some(res)
        } else if let Some(res) = self.try_string() {
            Some(res)
        } else if let Some(res) = self.try_ident() {
            Some(res)
        } else {
            None
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(true) = self.line.get(self.curr_col).map(|c| c.is_whitespace()) {
            self.curr_col += 1;
        }
    }

    // @requires self.curr_col != self.line.len()
    fn try_paren(&self) -> Option<(Token, usize)> {
        let c = self.line.get(self.curr_col);
        match c {
            Some('(') => Some((Token::LParen, 1)),
            Some(')') => Some((Token::RParen, 1)),
            _ => None,
        }
    }

    // @requires self.curr_col != self.line.len()
    fn try_quote(&self) -> Option<(Token, usize)> {
        let c = self.line.get(self.curr_col);
        // NOTE THIS DIES FOR ESCAPES CHARS
        match c {
            Some('\'') => Some((Token::Quote, 1)),
            _ => None,
        }
    }

    // @requires self.curr_col != self.line.len()
    fn try_string(&self) -> Option<(Token, usize)> {
        // start with "
        // take until another "
        // skip \\"

        if Some(&'"') != self.line.get(self.curr_col) {
            return None;
        }

        let mut s = String::new();

        for i in (self.curr_col + 1)..self.line.len() {
            if Some(&'"') == self.line.get(i) {
                let inc = s.len() + 2;
                return Some((Token::String(s), inc));
            }
            s.push(*self.line.get(i).unwrap());
        }

        // error
        None
    }

    // @requires self.curr_col != self.line.len()
    fn try_bool(&self) -> Option<(Token, usize)> {
        if Some(&'#') == self.line.get(self.curr_col) {
            match self.line.get(self.curr_col + 1) {
                Some('t') => Some((Token::Bool(true), 2)),
                Some('f') => Some((Token::Bool(false), 2)),
                _ => None,
            }
        } else {
            None
        }
    }

    // @requires self.curr_col != self.line.len()
    fn try_num(&self) -> Option<(Token, usize)> {
        let str = self
            .line
            .clone()
            .into_iter()
            .skip(self.curr_col)
            .take_while(|c| !c.is_whitespace() && !Self::stop_on_me(*c)) // we only work with unsigned now :)
            .collect::<String>();

        let len = str.len();

        if let Ok(num) = str.parse::<i64>() {
            Some((Token::Number(num), len))
        } else {
            None
        }
    }

    // @requires self.curr_col != self.line.len()
    fn try_char(&self) -> Option<(Token, usize)> {
        // 'c', '\\', ' ', ...
        if self.line.len() < self.curr_col + 3 {
            return None;
        }

        if !(self.line.get(self.curr_col) == Some(&'\''))
            || !(self.line.get(self.curr_col + 2) == Some(&'\''))
        {
            return None;
        }

        Some((Token::Char(*self.line.get(self.curr_col + 1).unwrap()), 3))
    }

    // @requires self.curr_col != self.line.len()
    fn try_ident(&self) -> Option<(Token, usize)> {
        let s = self
            .line
            .clone()
            .into_iter()
            .skip(self.curr_col)
            .take_while(|c| !c.is_whitespace() && !Self::stop_on_me(*c)) // we only work with unsigned now :)
            .collect::<String>();

        let len = s.len();

        if Self::legal_ident(&s) {
            Some((Token::Ident(s), len))
        } else {
            None
        }
    }

    fn stop_on_me(c: char) -> bool {
        "()'\"".contains(c)
    }

    fn legal_ident(s: &str) -> bool {
        dbg!(s);
        "1234567890".contains(s.chars().next().unwrap()) // this is hopefully always valid :)
    }
}

#[derive(Error, Debug)]
pub enum LexError {
    #[error("line {0}:{1} data matches no tokens")]
    InvalidToken(usize, usize),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn correctly_parses_valid_streams() {
        let mut l = Lexer::new();
        l.tokenize("'(#t #f)")
            .unwrap()
            .tokenize("(10 2)")
            .unwrap()
            .tokenize("(\'2\'")
            .unwrap()
            .tokenize("(\"a 2 j()\"")
            .unwrap();

        let expected = vec![
            Token::Quote,
            Token::LParen,
            Token::Bool(true),
            Token::Bool(false),
            Token::RParen,
            Token::LParen,
            Token::Number(10),
            Token::Number(2),
            Token::RParen,
            Token::LParen,
            Token::Char('2'),
            Token::LParen,
            Token::String("a 2 j()".to_string()),
        ];

        assert_eq!(expected, l.tokens);
    }
}

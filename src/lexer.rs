use thiserror::Error;
use crate::tokens::Token;

fn lex(word: &str) -> Option<Token> {
    if let Some(token) = concrete_tokens(word) {
        Some(token)
    } else if let Some(token) = abstract_tokens(word) {
        Some(token)
    } else {
        None
    }
}

fn concrete_tokens(word: &str) -> Option<Token> {
    match word {
        "(" => Some(Token::LParen),
        ")" => Some(Token::RParen),
        "#t" => Some(Token::Bool(true)),
        "#f" => Some(Token::Bool(false)),
        "'" => Some(Token::Quote),
        _ => None,
    }
}

fn abstract_tokens(word: &str) -> Option<Token> {
    if let Ok(num) = word.parse::<i64>() {
        Some(Token::Number(num))
    } else if let Ok(char) = word.parse::<char>() {
        Some(Token::Char(char))
    }
    // else if let Some(str) = to_scm_string(word) {
    //     Some(Token::String(str))
    // } else if let Some(ident) = to_scm_ident(word) {
    //     Some(Token::Ident(ident))
    // }
    else {
        None
    }
}

fn to_scm_string(word: &str) -> Option<String> {
    let word = word.strip_prefix('"')?
        .strip_suffix('"')?.to_string();
    todo!()
}

fn to_scm_ident(word: &str) -> Option<String> {
    todo!()
}

#[derive(Debug, Clone, Default)]
pub struct Lexer {
    pub tokens: Vec<Token>,
}

impl Lexer {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn tokenize(&mut self, program: &str) -> Result<&mut Self, LexError> {
        let program = program.replace('(', " ( ").replace(')', " ) ").replace("'", " ' ");
        for word in program.split_whitespace() {
            match lex(word) {
                Some(t) => self.tokens.push(t),
                None => return Err(LexError::InvalidToken(word.to_string()))
            }
        }

        Ok(self)
    }
}

#[derive(Error, Debug)]
pub enum LexError {
    #[error("data `{0}` matches no tokens")]
    InvalidToken(String)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn correctly_parses_valid_programs() {
        let mut l = Lexer::new();
        l.tokenize("'(#t #f)").unwrap()
            .tokenize("(10 -2 a \\)").unwrap();

        let expected = vec![
            Token::Quote, Token::LParen, Token::Bool(true), Token::Bool(false), Token::RParen,
            Token::LParen, Token::Number(10), Token::Number(-2), Token::Char('a'), Token::Char('\\'), Token::RParen,
        ];

        assert_eq!(expected, l.tokens);
    }
}
#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum Token {
    Ident(String),
    Bool(bool),
    Number(i64),
    Char(char),
    String(String),
    LParen,
    RParen,
}

impl Token {
    pub fn is_atom(&self) -> bool {
        matches!(
            self,
            Token::Number(_) | Token::Bool(_) | Token::Char(_) | Token::String(_) | Token::Ident(_)
        )
    }
}

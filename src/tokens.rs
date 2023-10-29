#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum Token {
    Ident(String),
    Bool(bool),
    Number(i64),
    // TODO: floats?
    Char(char),
    String(String),
    LParen,
    RParen,
    Quote,
    // TODO: comments?
}

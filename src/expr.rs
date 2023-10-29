use crate::tokens::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Atom(Atom),
    Pair(Option<Box<Expr>>, Option<Box<Expr>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Primitive(Token),
    Quoted(Box<Expr>),
}

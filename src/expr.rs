use crate::tokens::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Atom(Token),
    Pair(Option<Box<Expr>>, Option<Box<Expr>>),
}

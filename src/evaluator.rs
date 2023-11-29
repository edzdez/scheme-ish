#![allow(dead_code)]

use crate::expr::*;
use crate::tokens::Token;
use std::collections::{HashMap, LinkedList};

#[derive(Debug, Default, Clone)]
struct Environment {
    vtable: HashMap<String, Value>,
    parent: Option<Box<Environment>>,
}

impl Environment {
    pub fn find(&self, s: &String) -> Option<Value> {
        if let Some(v) = self.vtable.get(s) {
            Some(v.clone())
        } else {
            self.parent.as_ref().map(|env| env.find(s))?
        }
    }
}

#[derive(Debug, Clone)]
struct Func {
    params: Vec<Token>, // these should only be identifiers
    env: Box<Environment>,
    body: Expr,
}

impl Func {
    pub fn apply(&self, args: Environment) -> Value {
        // creates a copy of env
        // substitutes each argument in for each identifier
        // errors if it is not the correct # of args

        // evaluates the expression, substituting in the arg for the idents in body
        todo!()
    }
}

#[derive(Debug, Clone)]
enum Value {
    String(String),
    Char(char),
    Number(i64),
    Bool(bool),
    List(LinkedList<Value>),
    Function(Func),
    Nil,
}

#[derive(Debug, Default)]
struct Evaluator {
    global: Environment,
}

impl Evaluator {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn eval(&mut self, expr: Expr) -> Value {
        // if its an atom, then return value
        // otherwise apply the function

        match expr {
            Expr::Atom(t) => self.eval_atom(t),
            Expr::Pair(Some(f), args) => self.eval_func(f, args),
            Expr::Pair(None, _) => Value::Nil,
        }
    }

    pub fn eval_func(&self, f: Box<Expr>, args: Option<Box<Expr>>) -> Value {
        // find func object in vtable
        // flatten the args -> Vec
        // create new environment from params (and parent)
        if let Expr::Atom(Token::Ident(func)) = *f {
            let func = self.global.find(&func).unwrap(); // TODO handle unwrap failure
                                                         // let args = vec![];
        } else {
            // error
            todo!()
        };
        todo!()
    }

    pub fn eval_atom(&self, t: Token) -> Value {
        match t {
            Token::Bool(b) => Value::Bool(b),
            Token::Number(n) => Value::Number(n),
            Token::Char(c) => Value::Char(c),
            Token::String(s) => Value::String(s),
            Token::Ident(id) => {
                if let Some(v) = self.global.find(&id) {
                    v
                } else {
                    // raise error
                    todo!()
                }
            }
            Token::LParen | Token::RParen => unreachable!("not atoms"),
        }
    }
}

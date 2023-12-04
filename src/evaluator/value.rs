use crate::evaluator::environment::Environment;
use crate::evaluator::func::Func;
use crate::evaluator::EvalError;
use crate::expr::Expr;
use std::collections::LinkedList;
use std::fmt::{Debug, Display, Formatter};

#[derive(Clone)]
pub enum Value {
    String(String),
    Char(char),
    Number(i64),
    Bool(bool),
    List(LinkedList<Value>),
    Function(Func),
    Builtin(&'static dyn Fn(Vec<Value>) -> Result<Value, EvalError>),
    Special(&'static dyn Fn(Option<Box<Expr>>, &mut Environment) -> Result<Value, EvalError>),
    Nil,
    Unit,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Char(l0), Self::Char(r0)) => l0 == r0,
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::List(l0), Self::List(r0)) => l0 == r0,
            (Self::Function(l0), Self::Function(r0)) => l0 == r0,
            (Self::Builtin(_), Self::Builtin(_)) => false,
            (Self::Special(_), Self::Special(_)) => false,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Char(c) => write!(f, "'{}'", c),
            Value::Number(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", if *b { "#t" } else { "#f" }),
            Value::List(l) => {
                let back = l.back().expect("the world is ending");
                let mut idx = 1;
                let len = l.len();

                write!(f, "( ")?;
                for v in l {
                    if idx == len {
                        if *back != Value::Nil {
                            write!(f, ". {} ", v)?;
                        }
                    } else {
                        write!(f, "{} ", v)?;
                    }
                    idx += 1;
                }
                write!(f, ")")
            }
            Value::Function(func) => write!(f, "#function_{:p}", func),
            Value::Builtin(func) => write!(f, "#function_{:p}", func),
            Value::Special(func) => write!(f, "#function_{:p}", func),
            Value::Nil => write!(f, "nil"),
            Value::Unit => write!(f, "Unit"),
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

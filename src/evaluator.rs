#![allow(dead_code)]

use crate::expr::*;
use crate::tokens::Token;
use std::collections::{HashMap, LinkedList};
use std::iter::zip;
use thiserror::Error;

#[derive(Debug, Clone)]
struct Environment<'a> {
    pub vtable: HashMap<String, Value>,
    pub vtable_f: HashMap<String, Func<'a>>,
    pub parent: Option<&'a Environment<'a>>,
}

impl Default for Environment<'_> {
    fn default() -> Self {
        Self {
            // todo: implement this
            vtable: Default::default(),
            vtable_f: Default::default(),
            parent: None,
        }
    }
}

impl<'a> Environment<'a> {
    pub fn find(&self, s: &String) -> Option<&Value> {
        self.vtable
            .get(s)
            .or_else(|| self.parent.map(|env| env.find(s))?)
    }

    pub fn find_f(&self, s: &String) -> Option<&Func> {
        self.vtable_f
            .get(s)
            .or_else(|| self.parent.map(|env| env.find_f(s))?)
    }

    pub fn add(&mut self, name: String, value: Value) {
        self.vtable.insert(name, value);
    }

    pub fn add_f(&mut self, name: String, body: Expr) {
        // self.vtable.insert(name, value);
        todo!()
    }
}

#[derive(Debug, Clone)]
struct Func<'a> {
    params: Vec<Token>, // these should only be identifiers
    env: &'a Environment<'a>,
    body: Expr,
}

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Char(char),
    Number(i64),
    Bool(bool),
    List(LinkedList<Value>),
    Nil,
    Unit,
}

#[derive(Debug, Default)]
pub struct Evaluator {
    global: Environment<'static>,
}

impl Evaluator {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn evaluate(&mut self, expr: Expr) -> Result<Value, EvalError> {
        Self::eval(expr, &mut self.global)
    }

    fn eval(expr: Expr, env: &mut Environment) -> Result<Value, EvalError> {
        match expr {
            Expr::Atom(t) => Self::eval_atom(t, env),
            Expr::Pair(Some(func), args) => match *func {
                Expr::Atom(Token::Ident(f)) => match f.as_str() {
                    "define" => {
                        Self::define(args, env);
                        Ok(Value::Unit)
                    }
                    "+" => {
                        let args = Self::flatten_args(args, env)?;
                        Self::plus(args)
                    }
                    // add other special forms here
                    _ => {
                        let args = Self::flatten_args(args, env)?;
                        Self::eval_func(f, args, env)
                    }
                },
                _ => unreachable!("function name not an ident"),
            },
            Expr::Pair(None, Some(_)) => unreachable!("pair with none and some"),
            Expr::Pair(None, _) => Ok(Value::Nil),
        }
    }

    fn eval_func(
        func: String,
        args: Vec<Value>,
        env: &mut Environment,
    ) -> Result<Value, EvalError> {
        env.find_f(&func)
            .map(|func| {
                let mut env = Self::process_args(&func.params, args)?;
                env.parent = Some(&func.env);
                Self::eval(func.body.clone(), &mut env)
            })
            .unwrap_or(Err(EvalError::UnknownIdent))
    }

    fn eval_atom(t: Token, env: &mut Environment) -> Result<Value, EvalError> {
        match t {
            Token::Bool(b) => Ok(Value::Bool(b)),
            Token::Number(n) => Ok(Value::Number(n)),
            Token::Char(c) => Ok(Value::Char(c)),
            Token::String(s) => Ok(Value::String(s)),
            Token::Ident(id) => env
                .find(&id)
                .map(|x| x.clone())
                .ok_or(EvalError::UnknownIdent),
            Token::LParen | Token::RParen => unreachable!("not atoms"),
        }
    }

    fn flatten_args(
        args: Option<Box<Expr>>,
        env: &mut Environment,
    ) -> Result<Vec<Value>, EvalError> {
        let mut out = Vec::<Value>::new();
        let mut args = args;

        while let Some(Expr::Pair(Some(car), cdr)) = args.take().as_deref() {
            let expr = Self::eval(*car.clone(), env)?;
            out.push(expr);
            args = cdr.clone();
        }

        Ok(out)
    }

    fn process_args(params: &Vec<Token>, args: Vec<Value>) -> Result<Environment, EvalError> {
        let mut out = Environment::default();
        let mut iter = zip(params, args);
        while let Some((Token::Ident(param), arg)) = iter.next() {
            out.add(param.to_string(), arg);
        }

        if iter.next().is_none() {
            Err(EvalError::TooManyArgs)
        } else {
            Ok(out)
        }
    }

    fn define(args: Option<Box<Expr>>, env: &mut Environment) {
        todo!()
    }

    fn plus(args: Vec<Value>) -> Result<Value, EvalError> {
        let mut sum = 0;
        for val in args {
            if let Value::Number(x) = val {
                sum += x;
            } else {
                return Err(EvalError::ArithmeticError);
            }
        }
        Ok(Value::Number(sum))
    }
}

#[derive(Error, Debug)]
pub enum EvalError {
    #[error("Unknown Identifier")]
    UnknownIdent,

    #[error("Too many arguments")]
    TooManyArgs,

    #[error("Arithmetic Error")]
    ArithmeticError,
}

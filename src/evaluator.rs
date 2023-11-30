#![allow(dead_code)]

use crate::expr::*;
use crate::tokens::Token;
use std::collections::{HashMap, LinkedList};
use std::iter::zip;
use thiserror::Error;

macro_rules! eager {
    ($args:expr,$env:expr,$f:expr) => {{
        let args = Self::flatten_args($args)?;
        let args = Self::eval_args(args, $env)?;
        $f(args)
    }};
}

#[derive(Debug, Clone)]
struct Environment<'a> {
    pub vtable: HashMap<String, Value>,
    pub parent: Option<&'a Environment<'a>>,
}

impl Default for Environment<'_> {
    fn default() -> Self {
        let mut vtable = HashMap::new();
        vtable.insert(String::from("nil"), Value::Nil);

        Self {
            vtable,
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

    pub fn add(&mut self, name: String, value: Value) {
        self.vtable.insert(name, value);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    params: Vec<Token>, // these should only be identifiers
    body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    String(String),
    Char(char),
    Number(i64),
    Bool(bool),
    List(LinkedList<Value>),
    Function(Func),
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
                    // special forms
                    "define" => Self::define_(args, env),
                    "lambda" => Self::lambda_(args),
                    "cond" => Self::cond_(args, env),
                    "if" => Self::if_(args, env),

                    // builtins
                    "cons" => eager!(args, env, Self::cons),
                    "car" => eager!(args, env, Self::car),
                    "cdr" => eager!(args, env, Self::cdr),
                    "+" => eager!(args, env, Self::plus),
                    "-" => eager!(args, env, Self::subtract),
                    _ => {
                        if let Some(Value::Function(func)) = env.find(&f) {
                            let func = func.clone();
                            let args = Self::flatten_args(args)?;
                            let args = Self::eval_args(args, env)?;
                            Self::eval_func(func, args, env)
                        } else {
                            Err(EvalError::InvalidArguments)
                        }
                    }
                },
                _ => {
                    let func = Self::eval(*func, env)?;
                    if let Value::Function(func) = func {
                        let args = Self::flatten_args(args)?;
                        let args = Self::eval_args(args, env)?;
                        Self::eval_func(func, args, env)
                    } else {
                        Err(EvalError::InvalidArguments)
                    }
                }
            },
            Expr::Pair(None, Some(_)) => unreachable!("pair with none and some"),
            Expr::Pair(None, _) => Ok(Value::Nil),
        }
    }

    fn eval_func(func: Func, args: Vec<Value>, env: &mut Environment) -> Result<Value, EvalError> {
        let mut new_env = Self::process_args(&func.params, args)?;
        new_env.parent = Some(&env);
        Self::eval(func.body, &mut new_env)
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

    fn flatten_args(args: Option<Box<Expr>>) -> Result<Vec<Expr>, EvalError> {
        let mut out = Vec::<Expr>::new();
        let mut args = args;

        while let Some(Expr::Pair(Some(car), cdr)) = args.take().as_deref() {
            out.push(*car.clone());
            args = cdr.clone();
        }

        Ok(out)
    }

    fn eval_args(args: Vec<Expr>, env: &mut Environment) -> Result<Vec<Value>, EvalError> {
        let mut out = Vec::<Value>::new();
        let mut iter = args.into_iter();

        while let Some(expr) = iter.next() {
            let arg = Self::eval(expr, env)?;
            out.push(arg);
        }

        Ok(out)
    }

    fn process_args<'a>(
        params: &Vec<Token>,
        args: Vec<Value>,
    ) -> Result<Environment<'a>, EvalError> {
        Self::validate_arity(&args, params.len())?;

        let mut out = Environment::default();
        let mut iter = zip(params, args);
        while let Some((Token::Ident(param), arg)) = iter.next() {
            out.add(param.to_string(), arg);
        }

        Ok(out)
    }

    fn validate_arity<T>(args: &Vec<T>, arity: usize) -> Result<(), EvalError> {
        if args.len() != arity {
            Err(EvalError::WrongNoArgs)
        } else {
            Ok(())
        }
    }

    fn define_(args: Option<Box<Expr>>, env: &mut Environment) -> Result<Value, EvalError> {
        let mut args = Self::flatten_args(args)?;
        Self::validate_arity(&args, 2)?;

        let value = Self::eval(args.pop().unwrap(), env)?;
        match args.pop().unwrap() {
            Expr::Atom(Token::Ident(name)) => {
                env.add(name, value);
            }
            _ => return Err(EvalError::InvalidArguments),
        }

        Ok(Value::Unit)
    }

    fn lambda_(args: Option<Box<Expr>>) -> Result<Value, EvalError> {
        let mut args = Self::flatten_args(args)?;
        Self::validate_arity(&args, 2)?;

        let body = args.pop().unwrap();

        let params = args.pop().unwrap();
        let mut params_iter = Self::flatten_args(Some(Box::new(params)))?.into_iter();
        let mut params = Vec::new();

        while let Some(Expr::Atom(Token::Ident(id))) = params_iter.next() {
            params.push(Token::Ident(id));
        }

        if params_iter.next().is_some() {
            return Err(EvalError::InvalidArguments);
        }

        Ok(Value::Function(Func { params, body }))
    }

    fn if_(args: Option<Box<Expr>>, env: &mut Environment) -> Result<Value, EvalError> {
        let mut args = Self::flatten_args(args)?;

        Self::validate_arity(&args, 3)?;

        let alternative = args.pop().unwrap();
        let consequent = args.pop().unwrap();
        let predicate = args.pop().unwrap();

        match Self::eval(predicate, env)? {
            Value::Bool(true) => Self::eval(consequent, env),
            Value::Bool(false) => Self::eval(alternative, env),
            _ => Err(EvalError::InvalidArguments),
        }
    }

    fn cond_(args: Option<Box<Expr>>, env: &mut Environment) -> Result<Value, EvalError> {
        let mut args_iter = Self::flatten_args(args)?.into_iter();
        while let Some(Expr::Pair(Some(pred), Some(cons))) = args_iter.next() {
            if *pred == Expr::Atom(Token::Ident(String::from("else"))) {
                if let Expr::Pair(Some(cons), None) = *cons {
                    return Self::eval(*cons, env);
                } else {
                    return Err(EvalError::InvalidArguments);
                }
            }

            let pred = Self::eval(*pred, env)?;
            if pred == Value::Bool(true) {
                if let Expr::Pair(Some(cons), None) = *cons {
                    return Self::eval(*cons, env);
                } else {
                    return Err(EvalError::InvalidArguments);
                }
            } else if pred != Value::Bool(false) {
                return Err(EvalError::InvalidArguments);
            }
        }

        if args_iter.next().is_some() {
            return Err(EvalError::InvalidArguments);
        }

        Ok(Value::Unit)
    }

    fn cons(args: Vec<Value>) -> Result<Value, EvalError> {
        Self::validate_arity(&args, 2)?;
        Ok(Value::List(args.into_iter().collect()))
    }

    fn car(args: Vec<Value>) -> Result<Value, EvalError> {
        Self::validate_arity(&args, 1)?;
        if let Value::List(l) = &args[0] {
            if l.is_empty() {
                Err(EvalError::InvalidArguments)
            } else {
                Ok(l.front().unwrap().clone())
            }
        } else {
            Err(EvalError::InvalidArguments)
        }
    }

    fn cdr(args: Vec<Value>) -> Result<Value, EvalError> {
        Self::validate_arity(&args, 1)?;
        if let Value::List(mut l) = args[0].clone() {
            if l.is_empty() {
                Err(EvalError::InvalidArguments)
            } else {
                l.pop_front();
                Ok(Value::List(l))
            }
        } else {
            Err(EvalError::InvalidArguments)
        }
    }

    fn subtract(args: Vec<Value>) -> Result<Value, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::InvalidArguments);
        }

        let mut sum = 0;
        if let Value::Number(x) = &args[0] {
            sum += 2 * x;
        }

        for val in args {
            if let Value::Number(x) = val {
                sum -= x;
            } else {
                return Err(EvalError::ArithmeticError);
            }
        }

        Ok(Value::Number(sum))
    }

    fn plus(args: Vec<Value>) -> Result<Value, EvalError> {
        if args.len() < 2 {
            return Err(EvalError::InvalidArguments);
        }

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

#[derive(Error, Debug, PartialEq)]
pub enum EvalError {
    #[error("Unknown Identifier")]
    UnknownIdent,

    #[error("Wrong number of arguments")]
    WrongNoArgs,

    #[error("Arithmetic Error")]
    ArithmeticError,

    #[error("Argument Error")]
    InvalidArguments,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn define() {
        // on atoms
        {
            let mut tokenizer = Lexer::new();
            let mut parser = Parser::new();

            tokenizer.tokenize("(define a 5) a").unwrap();
            parser.parse(&mut tokenizer.tokens.into_iter()).unwrap();

            let mut evaluator = Evaluator::new();
            evaluator.evaluate(parser.ast[0].clone()).expect("what");
            let out = evaluator.evaluate(parser.ast[1].clone());

            let expected = Ok(Value::Number(5));
            assert_eq!(expected, out);
        }

        // on not atoms
        {
            let mut tokenizer = Lexer::new();
            let mut parser = Parser::new();

            tokenizer
                .tokenize("(define succ (lambda (x) (+ x 1))) (succ 12344)")
                .unwrap();
            parser.parse(&mut tokenizer.tokens.into_iter()).unwrap();

            let mut evaluator = Evaluator::new();
            evaluator.evaluate(parser.ast[0].clone()).expect("what");
            let out = evaluator.evaluate(parser.ast[1].clone());

            let expected = Ok(Value::Number(12345));
            assert_eq!(expected, out);
        }
    }

    #[test]
    fn eval_lambda() {
        // we assume tokenizer and parser are working correctly!
        let mut tokenizer = Lexer::new();
        let mut parser = Parser::new();

        tokenizer.tokenize("((lambda (x) (+ 42 x)) 2)").unwrap();
        parser.parse(&mut tokenizer.tokens.into_iter()).unwrap();

        let mut evaluator = Evaluator::new();
        let out = evaluator.evaluate(parser.ast[0].clone());

        let expected = Ok(Value::Number(44));
        assert_eq!(expected, out);
    }
}

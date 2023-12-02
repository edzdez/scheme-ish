#![allow(dead_code)]

use crate::expr::*;
use crate::lexer::{LexError, Lexer};
use crate::parser::{ParseError, Parser};
use crate::tokens::Token;
use std::collections::{HashMap, LinkedList};
use std::fmt::{Debug, Display, Formatter};
use std::iter::zip;
use std::{fs, io};
use thiserror::Error;

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Environment<'a> {
    pub vtable: HashMap<String, Value>,
    pub parent: Option<&'a Environment<'a>>,
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

impl<'a> From<HashMap<String, Value>> for Environment<'a> {
    fn from(value: HashMap<String, Value>) -> Self {
        Self {
            vtable: value,
            parent: None,
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct Func {
    params: Vec<Token>,
    body: Vec<Expr>,
    closed_over: HashMap<String, Value>, // the environment that is closed over
}

impl Func {
    pub fn new(params: Vec<Token>, body: Vec<Expr>, env: &Environment) -> Self {
        Self {
            params,
            body,
            closed_over: Self::copy_environment(env).vtable,
        }
    }

    fn copy_environment<'a>(env: &'a Environment<'a>) -> Environment<'a> {
        println!("copy environment");
        if let Some(parent) = env.parent {
            let mut parent = Self::copy_environment(parent);
            parent.vtable.extend(env.vtable.clone());
            parent
        } else {
            env.clone()
        }
    }
}

#[derive(Clone)]
pub enum Value {
    String(String),
    Char(char),
    Number(i64),
    Bool(bool),
    List(LinkedList<Value>),
    Function(Func),
    Builtin(&'static dyn Fn(Vec<Value>) -> Result<Value, EvalError>),
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
                    "cond" => Self::cond_(args, env),
                    "if" => Self::if_(args, env),
                    "begin" => Self::begin_(args, env),
                    "load" => Self::load_(args, env),
                    "lambda" => Self::lambda_(args, env),

                    _ => {
                        let val = env.find(&f);
                        match val {
                            Some(Value::Function(func)) => {
                                let func = func.clone();
                                let args = Self::flatten_args(args)?;
                                let args = Self::eval_args(args, env)?;
                                Self::eval_func(func, args, env)
                            }
                            Some(&Value::Builtin(func)) => {
                                let args = Self::flatten_args(args)?;
                                let args = Self::eval_args(args, env)?;
                                func(args)
                            }
                            _ => Err(EvalError::NotAFunction),
                        }
                    }
                },
                _ => {
                    let func = Self::eval(*func, env)?;
                    match &func {
                        Value::Function(func) => {
                            let func = func.clone();
                            let args = Self::flatten_args(args)?;
                            let args = Self::eval_args(args, env)?;
                            Self::eval_func(func, args, env)
                        }
                        &Value::Builtin(func) => {
                            let args = Self::flatten_args(args)?;
                            let args = Self::eval_args(args, env)?;
                            func(args)
                        }
                        _ => Err(EvalError::NotAFunction),
                    }
                }
            },
            Expr::Pair(None, Some(_)) => unreachable!("pair with none and some"),
            Expr::Pair(None, _) => Ok(Value::Nil),
        }
    }

    fn eval_func(func: Func, args: Vec<Value>, env: &mut Environment) -> Result<Value, EvalError> {
        let mut new_env = Environment::from(func.closed_over);
        new_env.parent = Some(env);
        new_env
            .vtable
            .extend(Self::process_args(&func.params, args)?);

        let mut out = Value::Unit;
        for expr in func.body {
            out = Self::eval(expr, &mut new_env)?;
        }
        Ok(out)
    }

    fn eval_atom(t: Token, env: &mut Environment) -> Result<Value, EvalError> {
        match t {
            Token::Bool(b) => Ok(Value::Bool(b)),
            Token::Number(n) => Ok(Value::Number(n)),
            Token::Char(c) => Ok(Value::Char(c)),
            Token::String(s) => Ok(Value::String(s)),
            Token::Ident(id) => env.find(&id).cloned().ok_or(EvalError::UnknownIdent(id)),
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

        for expr in args {
            let arg = Self::eval(expr, env)?;
            out.push(arg);
        }

        Ok(out)
    }

    fn process_args<'a>(
        params: &Vec<Token>,
        args: Vec<Value>,
    ) -> Result<HashMap<String, Value>, EvalError> {
        let mut out = HashMap::new();

        if params.len() >= 2 && params[params.len() - 2] == Token::Ident(String::from(".")) {
            Self::validate_arity(&args, params.len() - 2, &usize::ge)?;

            let mut args = args;
            let mut iter = zip(
                &params[0..params.len() - 2],
                args.drain(0..params.len() - 2),
            );
            while let Some((Token::Ident(param), arg)) = iter.next() {
                out.insert(param.to_string(), arg);
            }

            drop(iter);
            if let Some(Token::Ident(param)) = params.last() {
                let mut var = args.into_iter().collect::<LinkedList<_>>();
                let var = if var.is_empty() {
                    Value::Nil
                } else {
                    var.push_back(Value::Nil);
                    Value::List(var)
                };

                out.insert(param.to_owned(), var);
            }
        } else {
            Self::validate_arity(&args, params.len(), &usize::eq)?;
            let mut iter = zip(params, args);
            while let Some((Token::Ident(param), arg)) = iter.next() {
                out.insert(param.to_string(), arg);
            }
        }

        Ok(out)
    }

    fn validate_arity<T>(
        args: &Vec<T>,
        arity: usize,
        good_comp: &'static dyn Fn(&usize, &usize) -> bool,
    ) -> Result<(), EvalError> {
        if !good_comp(&args.len(), &arity) {
            Err(EvalError::ArityError)
        } else {
            Ok(())
        }
    }

    fn define_(args: Option<Box<Expr>>, env: &mut Environment) -> Result<Value, EvalError> {
        let args = Self::flatten_args(args)?;
        Self::validate_arity(&args, 2, &usize::ge)?;

        let name;
        if let Expr::Atom(Token::Ident(n)) = &args[0] {
            name = n.clone();
        } else {
            return Err(EvalError::DefineArgMustBeIdent);
        }

        let mut args = args.into_iter();
        args.next();

        let mut last = Value::Unit;
        for arg in args {
            last = Self::eval(arg, env)?;
        }

        env.add(name, last);
        Ok(Value::Unit)
    }

    fn lambda_(args: Option<Box<Expr>>, env: &mut Environment) -> Result<Value, EvalError> {
        let mut args = Self::flatten_args(args)?;
        Self::validate_arity(&args, 2, &usize::ge)?;

        let params = args[0].clone();
        let body = args.drain(1..).collect();

        let mut params_iter = Self::flatten_args(Some(Box::new(params)))?.into_iter();
        let mut params = Vec::new();

        let mut dot = false;
        while let Some(Expr::Atom(Token::Ident(id))) = params_iter.next() {
            dot |= id == String::from(".");
            params.push(Token::Ident(id));
        }

        if params_iter.next().is_some() {
            return Err(EvalError::ExpectedArgumentList);
        }

        if dot && params[params.len() - 2] != Token::Ident(String::from(".")) {
            return Err(EvalError::ExpectedArgumentList);
        }

        Ok(Value::Function(Func::new(params, body, env)))
    }

    fn if_(args: Option<Box<Expr>>, env: &mut Environment) -> Result<Value, EvalError> {
        let mut args = Self::flatten_args(args)?;
        Self::validate_arity(&args, 3, &usize::eq)?;

        let alternative = args.pop().unwrap();
        let consequent = args.pop().unwrap();
        let predicate = args.pop().unwrap();

        match Self::eval(predicate, env)? {
            Value::Bool(true) => Self::eval(consequent, env),
            Value::Bool(false) => Self::eval(alternative, env),
            _ => Err(EvalError::InvalidArguments),
        }
    }

    fn load_(args: Option<Box<Expr>>, env: &mut Environment) -> Result<Value, EvalError> {
        let args = Self::flatten_args(args)?;
        let mut args = Self::eval_args(args, env)?;
        Self::validate_arity(&args, 1, &usize::eq)?;

        if let Value::String(filename) = args.pop().unwrap() {
            let contents = fs::read_to_string(filename)?;
            let mut tokenizer = Lexer::new();
            let mut parser = Parser::new();

            tokenizer.tokenize(&contents)?;
            parser.parse(&mut tokenizer.tokens.into_iter())?;

            for expr in parser.ast {
                Self::eval(expr, env)?;
            }
            Ok(Value::Unit)
        } else {
            Err(EvalError::InvalidArguments)
        }
    }

    fn begin_(args: Option<Box<Expr>>, env: &mut Environment) -> Result<Value, EvalError> {
        let args = Self::flatten_args(args)?;
        let mut out = Value::Unit;
        for arg in args {
            out = Self::eval(arg, env)?;
        }

        Ok(out)
    }

    fn cond_(args: Option<Box<Expr>>, env: &mut Environment) -> Result<Value, EvalError> {
        let mut args_iter = Self::flatten_args(args)?.into_iter();
        while let Some(Expr::Pair(Some(pred), Some(cons))) = args_iter.next() {
            if *pred == Expr::Atom(Token::Ident(String::from("else"))) {
                return if let Expr::Pair(Some(cons), None) = *cons {
                    Self::eval(*cons, env)
                } else {
                    Err(EvalError::InvalidArguments)
                };
            }

            let pred = Self::eval(*pred, env)?;
            if pred == Value::Bool(true) {
                return if let Expr::Pair(Some(cons), None) = *cons {
                    Self::eval(*cons, env)
                } else {
                    Err(EvalError::InvalidArguments)
                };
            } else if pred != Value::Bool(false) {
                return Err(EvalError::InvalidArguments);
            }
        }

        if args_iter.next().is_some() {
            return Err(EvalError::InvalidArguments);
        }

        Ok(Value::Unit)
    }

    fn cons(mut args: Vec<Value>) -> Result<Value, EvalError> {
        Self::validate_arity(&args, 2, &usize::eq)?;
        let cdr = args.pop().unwrap();
        let car = args.pop().unwrap();

        Ok(if let Value::List(mut l) = cdr {
            l.push_front(car);
            Value::List(l)
        } else {
            Value::List(LinkedList::from([car, cdr]))
        })
    }

    fn car(args: Vec<Value>) -> Result<Value, EvalError> {
        Self::validate_arity(&args, 1, &usize::eq)?;
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

    fn cdr(mut args: Vec<Value>) -> Result<Value, EvalError> {
        Self::validate_arity(&args, 1, &usize::eq)?;

        if let Value::List(mut l) = args.pop().unwrap() {
            if l.is_empty() {
                Err(EvalError::InvalidArguments)
            } else {
                l.pop_front();
                if l.front().is_some_and(|v| *v == Value::Nil) {
                    Ok(Value::Nil)
                } else {
                    Ok(Value::List(l))
                }
            }
        } else {
            Err(EvalError::InvalidArguments)
        }
    }

    fn subtract(args: Vec<Value>) -> Result<Value, EvalError> {
        Self::validate_arity(&args, 2, &usize::ge)?;

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
        Self::validate_arity(&args, 2, &usize::ge)?;

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

    fn equal(mut args: Vec<Value>) -> Result<Value, EvalError> {
        Self::validate_arity(&args, 2, &usize::eq)?;
        Ok(Value::Bool(args.pop() == args.pop()))
    }

    fn less(mut args: Vec<Value>) -> Result<Value, EvalError> {
        Self::validate_arity(&args, 2, &usize::eq)?;
        if let (Some(Value::Number(a)), Some(Value::Number(b))) = (args.pop(), args.pop()) {
            Ok(Value::Bool(b < a))
        } else {
            Err(EvalError::InvalidArguments)
        }
    }

    fn greater(mut args: Vec<Value>) -> Result<Value, EvalError> {
        Self::validate_arity(&args, 2, &usize::eq)?;
        if let (Some(Value::Number(a)), Some(Value::Number(b))) = (args.pop(), args.pop()) {
            Ok(Value::Bool(b > a))
        } else {
            Err(EvalError::InvalidArguments)
        }
    }
}

impl Default for Evaluator {
    fn default() -> Self {
        let mut global = Environment::default();
        global.add(String::from("nil"), Value::Nil);
        global.add(String::from("+"), Value::Builtin(&Evaluator::plus));
        global.add(String::from("-"), Value::Builtin(&Evaluator::subtract));
        global.add(String::from("<"), Value::Builtin(&Evaluator::less));
        global.add(String::from(">"), Value::Builtin(&Evaluator::greater));
        global.add(String::from("cons"), Value::Builtin(&Evaluator::cons));
        global.add(String::from("car"), Value::Builtin(&Evaluator::car));
        global.add(String::from("cdr"), Value::Builtin(&Evaluator::cdr));
        global.add(String::from("equal?"), Value::Builtin(&Evaluator::equal));

        Self { global }
    }
}

#[derive(Error, Debug)]
pub enum EvalError {
    #[error("Unknown Identifier: `{0}`")]
    UnknownIdent(String),

    #[error("Arity mismatch")]
    ArityError,

    #[error("Arithmetic Error")]
    ArithmeticError,

    #[error("Argument Error")]
    InvalidArguments,

    #[error("Not a function")]
    NotAFunction,

    #[error("The first argument to define must be an ident")]
    DefineArgMustBeIdent,

    #[error("Expected argument list, found none")]
    ExpectedArgumentList,

    #[error("{0}")]
    Lex(#[from] LexError),

    #[error("{0}")]
    Parse(#[from] ParseError),

    #[error("{0}")]
    IO(#[from] io::Error),
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

            let expected = Value::Number(5);
            assert!(expected == out.unwrap()); // we use assert instead of assert_eq because Value
                                               // doesn't impl debug
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

            let expected = Value::Number(12345);
            assert!(expected == out.unwrap()); // we use assert instead of assert_eq because Value
                                               // doesn't impl debug
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

        let expected = Value::Number(44);
        assert!(expected == out.unwrap()); // we use assert instead of assert_eq because Value
                                           // doesn't impl debug
    }
}

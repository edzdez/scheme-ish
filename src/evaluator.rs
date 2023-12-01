#![allow(dead_code)]

use crate::expr::*;
use crate::tokens::Token;
use std::collections::{HashMap, LinkedList};
use std::fmt::{Display, Formatter};
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
    body: Vec<Expr>,
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

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::String(s) => {
                write!(f, "\"{}\"", s)
            }
            Value::Char(c) => {
                write!(f, "'{}'", c)
            }
            Value::Number(n) => {
                write!(f, "{}", n)
            }
            Value::Bool(b) => {
                write!(f, "{}", if *b { "#t" } else { "#f" })
            }
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
            Value::Function(func) => {
                write!(f, "#function_{:p}", func)
            }
            Value::Nil => {
                write!(f, "nil")
            }
            Value::Unit => {
                write!(f, "Unit")
            }
        }
    }
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
                    "begin" => Self::begin_(args, env),

                    // builtins
                    "cons" => eager!(args, env, Self::cons),
                    "car" => eager!(args, env, Self::car),
                    "cdr" => eager!(args, env, Self::cdr),
                    "equal?" => eager!(args, env, Self::equal),
                    "+" => eager!(args, env, Self::plus),
                    "-" => eager!(args, env, Self::subtract),
                    "<" => eager!(args, env, Self::less),
                    ">" => eager!(args, env, Self::greater),
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
            Token::Ident(id) => env.find(&id).cloned().ok_or(EvalError::UnknownIdent),
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
            Err(EvalError::ArityError)
        } else {
            Ok(())
        }
    }

    fn define_(args: Option<Box<Expr>>, env: &mut Environment) -> Result<Value, EvalError> {
        let args = Self::flatten_args(args)?;
        if args.len() < 2 {
            return Err(EvalError::InvalidArguments);
        }

        let name;
        if let Expr::Atom(Token::Ident(n)) = &args[0] {
            name = n.clone();
        } else {
            return Err(EvalError::InvalidArguments);
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

    fn lambda_(args: Option<Box<Expr>>) -> Result<Value, EvalError> {
        let mut args = Self::flatten_args(args)?;
        if args.len() < 2 {
            return Err(EvalError::InvalidArguments);
        }

        let params = args[0].clone();
        let body = args.drain(1..).collect();

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

    fn cons(args: Vec<Value>) -> Result<Value, EvalError> {
        Self::validate_arity(&args, 2)?;

        let mut args = args;
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
        let mut args = args;

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

    fn equal(args: Vec<Value>) -> Result<Value, EvalError> {
        Self::validate_arity(&args, 2)?;
        let mut args = args;

        Ok(Value::Bool(args.pop() == args.pop()))
    }

    fn less(args: Vec<Value>) -> Result<Value, EvalError> {
        Self::validate_arity(&args, 2)?;
        let mut args = args;

        if let (Some(Value::Number(a)), Some(Value::Number(b))) = (args.pop(), args.pop()) {
            Ok(Value::Bool(b < a))
        } else {
            Err(EvalError::InvalidArguments)
        }
    }

    fn greater(args: Vec<Value>) -> Result<Value, EvalError> {
        Self::validate_arity(&args, 2)?;
        let mut args = args;

        if let (Some(Value::Number(a)), Some(Value::Number(b))) = (args.pop(), args.pop()) {
            Ok(Value::Bool(b > a))
        } else {
            Err(EvalError::InvalidArguments)
        }
    }
}

#[derive(Error, Debug, PartialEq)]
pub enum EvalError {
    #[error("Unknown Identifier")]
    UnknownIdent,

    #[error("Wrong number of arguments")]
    ArityError,

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

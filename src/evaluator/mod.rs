pub mod environment;
pub mod func;
pub mod library;
pub mod value;

use crate::evaluator::environment::Environment;
use crate::evaluator::func::Func;
use crate::evaluator::library::*;
use crate::evaluator::value::Value;
use crate::expr::*;
use crate::lexer::LexError;
use crate::parser::ParseError;
use crate::tokens::Token;
use std::fmt::Debug;
use std::io;
use thiserror::Error;

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

    pub fn eval(expr: Expr, env: &mut Environment) -> Result<Value, EvalError> {
        match expr {
            Expr::Atom(t) => Self::eval_atom(t, env),
            Expr::Pair(Some(func), args) => match *func {
                Expr::Atom(Token::Ident(f)) => {
                    let val = env.find(&f);
                    match val {
                        Some(Value::Function(func)) => {
                            let func = func.clone();
                            let args = flatten_args(args)?;
                            let args = eval_args(args, env)?;
                            Self::eval_func(func, args, env)
                        }
                        Some(&Value::Builtin(func)) => {
                            let args = flatten_args(args)?;
                            let args = eval_args(args, env)?;
                            func(args)
                        }
                        Some(&Value::Special(func)) => func(args, env),
                        _ => Err(EvalError::NotAFunction),
                    }
                }
                _ => {
                    let func = Self::eval(*func, env)?;
                    match &func {
                        Value::Function(func) => {
                            let func = func.clone();
                            let args = flatten_args(args)?;
                            let args = eval_args(args, env)?;
                            Self::eval_func(func, args, env)
                        }
                        &Value::Builtin(func) => {
                            let args = flatten_args(args)?;
                            let args = eval_args(args, env)?;
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
        new_env.vtable.extend(process_args(&func.params, args)?);

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
}

impl Default for Evaluator {
    fn default() -> Self {
        let mut global = Environment::default();

        // special forms
        global.add(String::from("define"), Value::Special(&define_));
        global.add(String::from("cond"), Value::Special(&cond_));
        global.add(String::from("if"), Value::Special(&if_));
        global.add(String::from("begin"), Value::Special(&begin_));
        global.add(String::from("lambda"), Value::Special(&lambda_));

        // directives
        global.add(String::from("load"), Value::Special(&load_));
        global.add(String::from("exit"), Value::Special(&exit_));

        // builtin functions
        global.add(String::from("+"), Value::Builtin(&add));
        global.add(String::from("-"), Value::Builtin(&sub));
        global.add(String::from("*"), Value::Builtin(&mul));
        global.add(String::from("/"), Value::Builtin(&div));
        global.add(String::from("<"), Value::Builtin(&less));
        global.add(String::from(">"), Value::Builtin(&greater));
        global.add(String::from("cons"), Value::Builtin(&cons));
        global.add(String::from("car"), Value::Builtin(&car));
        global.add(String::from("cdr"), Value::Builtin(&cdr));
        global.add(String::from("equal?"), Value::Builtin(&equal));

        // values
        global.add(String::from("nil"), Value::Nil);

        // optional features
        global.add(String::from("display"), Value::Builtin(&display));
        global.add(String::from("random"), Value::Builtin(&random));

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

    #[error("Exit")]
    Exit,

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
            assert_eq!(expected, out.unwrap());
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
            assert_eq!(expected, out.unwrap());
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
        assert_eq!(expected, out.unwrap());
    }
}

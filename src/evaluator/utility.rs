use crate::expr::Expr;
use crate::tokens::Token;
use core::iter::zip;
use std::collections::HashMap;
use std::collections::LinkedList;

use super::{environment::Environment, value::Value, EvalError, Evaluator};

pub fn flatten_args(args: Option<Box<Expr>>) -> Result<Vec<Expr>, EvalError> {
    let mut out = Vec::<Expr>::new();
    let mut args = args;

    while let Some(Expr::Pair(Some(car), cdr)) = args.take().as_deref() {
        out.push(*car.clone());
        args = cdr.clone();
    }

    Ok(out)
}

pub fn eval_args(args: Vec<Expr>, env: &mut Environment) -> Result<Vec<Value>, EvalError> {
    let mut out = Vec::<Value>::new();

    for expr in args {
        let arg = Evaluator::eval(expr, env)?;
        out.push(arg);
    }

    Ok(out)
}

pub fn process_args<'a>(
    params: &Vec<Token>,
    args: Vec<Value>,
) -> Result<HashMap<String, Value>, EvalError> {
    let mut out = HashMap::new();

    if params.len() >= 2 && params[params.len() - 2] == Token::Ident(String::from(".")) {
        validate_arity(&args, params.len() - 2, &usize::ge)?;

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
        validate_arity(&args, params.len(), &usize::eq)?;
        let mut iter = zip(params, args);
        while let Some((Token::Ident(param), arg)) = iter.next() {
            out.insert(param.to_string(), arg);
        }
    }

    Ok(out)
}

pub fn validate_arity<T>(
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

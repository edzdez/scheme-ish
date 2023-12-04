use crate::evaluator::environment::Environment;
use crate::evaluator::func::Func;
use crate::evaluator::utility::*;
use crate::evaluator::value::Value;
use crate::evaluator::EvalError;
use crate::evaluator::Evaluator;
use crate::expr::*;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::tokens::Token;
use rand::Rng;
use std::collections::LinkedList;
use std::fs::File;
use std::io::{prelude::*, BufReader};

pub fn define_(args: Option<Box<Expr>>, env: &mut Environment) -> Result<Value, EvalError> {
    let args = flatten_args(args)?;
    validate_arity(&args, 2, &usize::ge)?;

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
        last = Evaluator::eval(arg, env)?;
    }

    env.add(name, last);
    Ok(Value::Unit)
}

pub fn lambda_(args: Option<Box<Expr>>, env: &mut Environment) -> Result<Value, EvalError> {
    let mut args = flatten_args(args)?;
    validate_arity(&args, 2, &usize::ge)?;

    let params = args[0].clone();
    let body = args.drain(1..).collect();

    let mut params_iter = flatten_args(Some(Box::new(params)))?.into_iter();
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

pub fn if_(args: Option<Box<Expr>>, env: &mut Environment) -> Result<Value, EvalError> {
    let mut args = flatten_args(args)?;
    validate_arity(&args, 3, &usize::eq)?;

    let alternative = args.pop().unwrap();
    let consequent = args.pop().unwrap();
    let predicate = args.pop().unwrap();

    match Evaluator::eval(predicate, env)? {
        Value::Bool(true) => Evaluator::eval(consequent, env),
        Value::Bool(false) => Evaluator::eval(alternative, env),
        _ => Err(EvalError::InvalidArguments),
    }
}

pub fn load_(args: Option<Box<Expr>>, env: &mut Environment) -> Result<Value, EvalError> {
    let args = flatten_args(args)?;
    let mut args = eval_args(args, env)?;
    validate_arity(&args, 1, &usize::eq)?;

    if let Value::String(filename) = args.pop().unwrap() {
        let mut tokenizer = Lexer::new();
        let mut parser = Parser::new();

        let file = File::open(filename)?;
        let contents = BufReader::new(file).lines();

        for s in contents.into_iter() {
            tokenizer.tokenize(&s?)?;
        }
        parser.parse(&mut tokenizer.tokens.into_iter())?;

        for expr in parser.ast {
            Evaluator::eval(expr, env)?;
        }
        Ok(Value::Unit)
    } else {
        Err(EvalError::InvalidArguments)
    }
}

pub fn exit_(_: Option<Box<Expr>>, _: &mut Environment) -> Result<Value, EvalError> {
    Err(EvalError::Exit)
}

pub fn begin_(args: Option<Box<Expr>>, env: &mut Environment) -> Result<Value, EvalError> {
    let args = flatten_args(args)?;
    let mut out = Value::Unit;
    for arg in args {
        out = Evaluator::eval(arg, env)?;
    }

    Ok(out)
}

pub fn cond_(args: Option<Box<Expr>>, env: &mut Environment) -> Result<Value, EvalError> {
    let mut args_iter = flatten_args(args)?.into_iter();
    while let Some(Expr::Pair(Some(pred), Some(cons))) = args_iter.next() {
        if *pred == Expr::Atom(Token::Ident(String::from("else"))) {
            return if let Expr::Pair(Some(cons), None) = *cons {
                Evaluator::eval(*cons, env)
            } else {
                Err(EvalError::InvalidArguments)
            };
        }

        let pred = Evaluator::eval(*pred, env)?;
        if pred == Value::Bool(true) {
            return if let Expr::Pair(Some(cons), None) = *cons {
                Evaluator::eval(*cons, env)
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

pub fn cons(mut args: Vec<Value>) -> Result<Value, EvalError> {
    validate_arity(&args, 2, &usize::eq)?;
    let cdr = args.pop().unwrap();
    let car = args.pop().unwrap();

    Ok(if let Value::List(mut l) = cdr {
        l.push_front(car);
        Value::List(l)
    } else {
        Value::List(LinkedList::from([car, cdr]))
    })
}

pub fn car(args: Vec<Value>) -> Result<Value, EvalError> {
    validate_arity(&args, 1, &usize::eq)?;
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

pub fn cdr(mut args: Vec<Value>) -> Result<Value, EvalError> {
    validate_arity(&args, 1, &usize::eq)?;

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

pub fn add(args: Vec<Value>) -> Result<Value, EvalError> {
    validate_arity(&args, 2, &usize::ge)?;

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

pub fn sub(args: Vec<Value>) -> Result<Value, EvalError> {
    validate_arity(&args, 2, &usize::ge)?;

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

pub fn mul(args: Vec<Value>) -> Result<Value, EvalError> {
    validate_arity(&args, 2, &usize::ge)?;

    let mut prod = 1;

    for val in args {
        if let Value::Number(x) = val {
            prod *= x;
        } else {
            return Err(EvalError::ArithmeticError);
        }
    }

    Ok(Value::Number(prod))
}

pub fn div(args: Vec<Value>) -> Result<Value, EvalError> {
    validate_arity(&args, 2, &usize::ge)?;

    let mut quotient = 1;
    if let Value::Number(x) = &args[0] {
        quotient *= x * x;
    }

    for val in args {
        if let Value::Number(x) = val {
            quotient /= x;
        } else {
            return Err(EvalError::ArithmeticError);
        }
    }

    Ok(Value::Number(quotient))
}

pub fn equal(mut args: Vec<Value>) -> Result<Value, EvalError> {
    validate_arity(&args, 2, &usize::eq)?;
    Ok(Value::Bool(args.pop() == args.pop()))
}

pub fn random(mut args: Vec<Value>) -> Result<Value, EvalError> {
    validate_arity(&args, 2, &usize::eq)?;
    if let (Some(Value::Number(upper)), Some(Value::Number(lower))) = (args.pop(), args.pop()) {
        Ok(Value::Number(rand::thread_rng().gen_range(lower..upper)))
    } else {
        Err(EvalError::InvalidArguments)
    }
}

pub fn display(mut args: Vec<Value>) -> Result<Value, EvalError> {
    validate_arity(&args, 1, &usize::eq)?;
    println!("{}", args.pop().unwrap());
    Ok(Value::Unit)
}

pub fn less(mut args: Vec<Value>) -> Result<Value, EvalError> {
    validate_arity(&args, 2, &usize::eq)?;
    if let (Some(Value::Number(a)), Some(Value::Number(b))) = (args.pop(), args.pop()) {
        Ok(Value::Bool(b < a))
    } else {
        Err(EvalError::InvalidArguments)
    }
}

pub fn greater(mut args: Vec<Value>) -> Result<Value, EvalError> {
    validate_arity(&args, 2, &usize::eq)?;
    if let (Some(Value::Number(a)), Some(Value::Number(b))) = (args.pop(), args.pop()) {
        Ok(Value::Bool(b > a))
    } else {
        Err(EvalError::InvalidArguments)
    }
}

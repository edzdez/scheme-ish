use crate::evaluator::environment::Environment;
use crate::evaluator::value::Value;
use crate::expr::Expr;
use crate::tokens::Token;
use std::collections::HashMap;

#[derive(Clone, PartialEq)]
pub struct Func {
    pub params: Vec<Token>,
    pub body: Vec<Expr>,
    pub closed_over: HashMap<String, Value>, // the environment that is closed over
}

impl Func {
    pub fn new(params: Vec<Token>, body: Vec<Expr>, env: &Environment) -> Self {
        let closed_over = Self::move_in(&body, env);
        Self {
            params,
            body,
            closed_over,
        }
    }

    fn find_idents(body: &Expr) -> Vec<String> {
        match body {
            Expr::Atom(Token::Ident(x)) => vec![x.clone()],
            Expr::Pair(Some(car), Some(cdr)) => Self::find_idents(car)
                .into_iter()
                .chain(Self::find_idents(cdr))
                .collect(),
            Expr::Pair(Some(car), None) => Self::find_idents(car),
            _ => vec![],
        }
    }

    fn move_in<'a>(body: &Vec<Expr>, env: &'a Environment<'a>) -> HashMap<String, Value> {
        body.into_iter()
            .flat_map(|expr| {
                Self::find_idents(expr)
                    .into_iter()
                    .map(|x| env.find(&x).map(|val| (x, val.clone())))
            })
            .filter_map(|x| x)
            .collect()
    }
}

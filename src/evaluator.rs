use crate::expr::Expr;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, Default)]
struct Evaluator {
    env: HashMap<String, Expr>,
}

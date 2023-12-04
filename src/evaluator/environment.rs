use crate::evaluator::value::Value;
use std::collections::HashMap;

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

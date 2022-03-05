use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;
use crate::object::Object;

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct Environment {
    store: BTreeMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment { store: BTreeMap::new(), outer: None }
    }

    pub fn virtual_environment(host: Rc<RefCell<Environment>>) -> Environment {
        Environment { store: BTreeMap::new(), outer: Some(host) }
    }

    pub fn get(&self, key: &str) -> Option<Object> {
        match self.store.get(key) {
            Some(obj) => Some(obj.clone()),
            None => match &self.outer {
                Some(env) => env.borrow().get(key),
                None      => None,
            },
        }
    }

    pub fn set(&mut self, key: String, value: Object) {
        self.store.insert(key, value);
    }
}

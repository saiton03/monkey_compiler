use std::collections::BTreeMap;
use std::fmt;
use std::fmt::Formatter;
use crate::ast::{Expression, Statement};
use crate::builtin::BuiltinFunction;
use crate::environment::Environment;
use crate::evaluator::Evaluator;

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct KeyValue {
    pub key: HashKey,
    pub value: Box<Object>,
}


#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum  HashKey {
    Integer(i64),
    String(String),
    Boolean(bool),
    Null
}

impl fmt::Display for HashKey {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            HashKey::Integer(i) => write!(f, "{}", i),
            HashKey::String(s) => write!(f, "{}", s),
            HashKey::Boolean(b) => write!(f, "{}", b),
            HashKey::Null => write!(f, "null"),
        }
    }
}

impl HashKey {
    pub fn get_key(obj: &Object) -> Self {
        match obj {
            Object::Integer(i) => HashKey::Integer(*i),
            Object::String(st) => HashKey::String(st.to_string()),
            Object::Boolean(b) => HashKey::Boolean(*b),
            _ => HashKey::Null,
        }
    }
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum Object {
    Integer(i64),
    String(String),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
    Function{
        parameters: Vec<Expression>,
        body: Statement,
        env: Environment,
    },
    Builtin(BuiltinFunction),
    Array(Vec<Object>),
    Hash(BTreeMap<HashKey, KeyValue>)
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let msg = match self {
            Object::Integer(i) => format!("{}", i),
            Object::String(s) => s.to_string(),
            Object::Boolean(b) => format!("{}", b),
            Object::Null => "NULL".to_string(),
            Object::ReturnValue(obj) => format!("return {}", obj),
            Object::Error(msg) => format!("error: {}", msg),
            Object::Function { parameters, body, .. } => format!("fn ({}) {{ {} }}",
                                                                 parameters.iter().map(|p| format!("{}", p)).collect::<Vec<_>>().join(", "), body),
            Object::Builtin(bf) => format!("builtin function {}", bf),
            Object::Array(arr) => format!("[{}]",
                    arr.iter().map(|p| format!("{}", p)).collect::<Vec<_>>().join(", ")),
            Object::Hash(hash) => format!("{{{}}}",
                    hash.iter().map(|(_, kv)| format!("{}: {}", kv.key, kv.value)).collect::<Vec<_>>().join(", "))
        };
        write!(f, "{}", msg)
    }
}

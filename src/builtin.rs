use std::fmt;
use std::fmt::Formatter;
use crate::evaluator::new_error;
use crate::object::Object;

pub type BuiltinFn = fn(args: Vec<Object>) -> Object;

#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct BuiltinFunction {
    name: String,
    func: BuiltinFn
}

impl fmt::Display for BuiltinFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl BuiltinFunction {
    pub fn look_up(name: &str) -> Option<Self> {
        match name {
            "len" => Some(Self {
                name: "len".to_string(),
                func: |args: Vec<Object>| -> Object {
                    if args.len() != 1 {
                        return new_error(format!("the number of arguments is not 1, got {}", args.len()))
                    }

                    match &args[0] {
                        Object::Array(v) => Object::Integer(v.len() as i64),
                        Object::Hash(h) => Object::Integer(h.len() as i64),
                        other => new_error(format!("{} is not collection type", other))
                    }
                }
            }),
            "puts" => Some(Self{
                name: "puts".to_string(),
                func: |args: Vec<Object>| -> Object {
                    for arg in args {
                        println!("{}", arg)
                    }
                    Object::Null
                }
            }),
            _ => None,
        }
    }

    pub fn call(&self, args: Vec<Object>) -> Object {
        (self.func)(args)
    }
}


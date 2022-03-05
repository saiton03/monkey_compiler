use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;
use crate::ast::{Expression, Node, Program, Statement};
use crate::builtin::BuiltinFunction;
use crate::object::{HashKey, KeyValue, Object};
use crate::environment::Environment;

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct Evaluator {
    env: Rc<RefCell<Environment>>
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(Environment::new()))
        }
    }

    pub fn from(env: Environment) -> Self {
        Self {
            env: Rc::new(RefCell::new(env))
        }
    }

    pub fn get(&self, key: &str) -> Option<Object> {
        self.env.borrow().get(key)
    }

    fn set(&mut self, key: String, value: Object) {
        self.env.borrow_mut().set(key, value)
    }

    pub fn eval(&mut self, node: Node) -> Option<Object> {
        match node {
            Node::Program(program) => self.eval_program(program),
            Node::Statement(stmt) => match stmt {
                Statement::BlockStatement(b) => self.eval_block_statement(b),
                Statement::ExpressionStatement(exp) => self.eval(Node::Expression(exp)),
                Statement::ReturnStatement(r) => {
                    let val = self.eval(Node::Expression(r))?;
                    if is_error(&val) {
                        return Some(val);
                    }
                    Some(Object::ReturnValue(Box::new(val)))
                }
                Statement::LetStatement {identifier, value} => {
                    let val = self.eval(Node::Expression(value))?;
                    if is_error(&val) {
                        return Some(val);
                    }
                    match identifier {
                        Expression::Identifier(s) => {
                            self.set(s, val.clone());
                            Some(val)
                        },
                        _ => None,
                    }
                }
            },
            Node::Expression(exp) => match exp {
                Expression::Integer(i) => Some(Object::Integer(i)),
                Expression::String(s) => Some(Object::String(s)),
                Expression::Boolean(b) => Some(Object::Boolean(b)),
                Expression::PrefixExpression {operation, right} => {
                    let right = self.eval(Node::Expression(*right))?;
                    if is_error(&right) {
                        return Some(right);
                    }
                    Some(eval_prefix_expression(operation, right))
                },
                Expression::InfixExpression {operation, left, right} => {
                    let left = self.eval(Node::Expression(*left))?;
                    if is_error(&left) {
                        return Some(left)
                    }
                    let right = self.eval(Node::Expression(*right))?;
                    if is_error(&right) {
                        return Some(right)
                    }
                    Some(eval_infix_expression(operation, left, right))
                },
                Expression::IfExpression{..} => self.eval_if_expression(exp),
                Expression::Identifier(s) => self.eval_identifier(s),
                Expression::Function {parameters, body} => {
                    let env = Rc::clone(&self.env);
                    Some(Object::Function {
                        parameters,
                        body: *body,
                        env: Environment::virtual_environment(env),
                    })
                },
                Expression::CallExpression {function, arguments} => {
                    let function = self.eval(Node::Expression(*function))?;
                    if is_error(&function) {
                        return Some(function);
                    }
                    let args = self.eval_expressions(arguments)?;
                    if args.len() == 1 && is_error(&args[0]) {
                        return Some(args[0].clone());
                    }
                    apply_function(function, args)
                },
                Expression::Array(exps) => {
                    let elements = self.eval_expressions(exps)?;
                    if elements.len() == 1 && is_error(&elements[0]) {
                        return Some(elements[0].clone());
                    }
                    Some(Object::Array(elements))
                },
                Expression::IndexExpression {left, index} => {
                    let left = self.eval(Node::Expression(*left))?;
                    if is_error(&left) {
                        return Some(left);
                    }
                    let index = self.eval(Node::Expression(*index))?;
                    if is_error(&index) {
                        return Some(index);
                    }
                    Some(self.eval_index_expressions(left, index))
                },
                Expression::Hash(kv) => {
                    self.eval_hash_literal(kv)
                }
            },
        }
    }

    fn eval_program(&mut self, program: Program) -> Option<Object>{
        let mut result = Object::Null;

        for stmt in program.statements {
            result = self.eval(Node::Statement(stmt))?;

            match result {
                Object::ReturnValue(ret) => {
                    return Some(*ret);
                },
                Object::Error(err) => {
                    return Some(Object::Error(err.clone()));
                }
                _ => {}
            }
        }
        Some(result)
    }

    fn eval_block_statement(&mut self, block: Vec<Statement>) -> Option<Object>{
        let mut result = Object::Null;

        for stmt in block {
            result = self.eval(Node::Statement(stmt))?;

            match result {
                Object::ReturnValue(_) | Object::Error(_) => {
                    return Some(result);
                },
                _ => {},
            }
        }

        Some(result)
    }


    fn eval_if_expression(&mut self, expression: Expression) -> Option<Object> {
        if let Expression::IfExpression {condition, consequence, alternative} = expression {
            let condition = self.eval(Node::Expression(*condition))?;
            if is_error(&condition) {
                return Some(condition);
            }

            if is_truthy(&condition) {
                self.eval(Node::Statement(*consequence))
            } else {
                match alternative {
                    None => Some(Object::Null),
                    Some(st) => self.eval(Node::Statement(*st))
                }
            }
        } else {
            None
        }
    }

    fn eval_identifier(&mut self, identifier: String) -> Option<Object> {
        match  self.get(&identifier) {
            Some(obj) => Some(obj.clone()),
            None => match BuiltinFunction::look_up(&identifier) {
                Some(builtin) => Some(Object::Builtin(builtin)),
                None => Some(new_error(format!("identifier {} not found", identifier)))
            }
        }
    }

    fn eval_expressions(&mut self, args: Vec<Expression>) -> Option<Vec<Object>> {
        let mut result = Vec::new();

        for arg in args {
            let evaluated = self.eval(Node::Expression(arg))?;
            if is_error(&evaluated) {
                return Some(vec![evaluated]);
            }
            result.push(evaluated);
        }
        Some(result)
    }


    fn eval_index_expressions(&self, left: Object, index: Object) -> Object {
        match left {
            Object::Array(arr) => match index {
                Object::Integer(i) => eval_array_index(arr, i),
                _ => new_error(format!("index {} is not integer", index)),
            },
            Object::Hash(hash) => {
                eval_hash_index(hash, index)
            }
            _ => new_error(format!("{} is not a collection type object", left)),
        }
    }

    fn eval_hash_literal(&mut self, kv: BTreeMap<Box<Expression>, Box<Expression>>) -> Option<Object> {
        let mut pairs = BTreeMap::<HashKey, KeyValue>::new();
        for (key, value) in kv {
            let key = self.eval(Node::Expression(*key))?;
            if is_error(&key) {
                return Some(key);
            }

            let value = self.eval(Node::Expression(*value))?;
            if is_error(&value) {
                return Some(value);
            }

            let hashed_key = HashKey::get_key(&key);
            pairs.insert(hashed_key.clone(), KeyValue{ key: hashed_key, value: Box::new(value) });
        }
        Some(Object::Hash(pairs))
    }

}

fn is_error( val: &Object) -> bool {
    if let Object::Error(_) = val {
        true
    } else {
        false
    }
}

pub fn new_error(msg: String) -> Object {
    Object::Error(msg)
}

fn native_bool_to_object(input: bool) -> Object {
    if input {
        Object::Boolean(true)
    } else {
        Object::Boolean(false)
    }
}

fn is_truthy(obj: &Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Boolean(b) => *b,
        _ => true,
    }
}

fn eval_prefix_expression(operation: String, right: Object) -> Object {
    match operation.as_str() {
        "!" => eval_bang_expression(right),
        "-" => eval_minus_prefix_expression(right),
        _ => new_error(format!("unknown prefix operator: {}{}", operation, right)),
    }
}

fn eval_infix_expression(operation: String, left: Object, right: Object) -> Object {
    if let (Object::Integer(left),  Object::Integer(right)) = (&left, &right) {
        eval_integer_infix_expression(operation, *left,*right)
    } else if let (Object::String(left), Object::String(right)) = (&left, &right) {
        eval_string_infix_expression(operation, left, right)
    } else {
        match operation.as_str() {
            "==" => native_bool_to_object(left == right),
            "!=" => native_bool_to_object(left != right),
            _ => new_error(format!("invalid operation: op {}, left {}, right {}", operation, left, right))
        }
    }
}

fn eval_bang_expression(right: Object) -> Object {
    match right {
        Object::Boolean(b) => Object::Boolean(!b),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false)
    }
}

fn eval_minus_prefix_expression(right: Object) -> Object {
    match right {
        Object::Integer(i) => Object::Integer(-i),
        _ => new_error(format!("unknown operator: -{}", right)),
    }
}

fn eval_integer_infix_expression(operation: String, left: i64, right: i64) -> Object {
    match operation.as_str() {
        "+" => Object::Integer(left+right),
        "-" => Object::Integer(left-right),
        "*" => Object::Integer(left*right),
        "/" => Object::Integer(left/right),
        "<" => Object::Boolean(left<right),
        ">" => Object::Boolean(left>right),
        "==" => Object::Boolean(left==right),
        "!=" => Object::Boolean(left!=right),
        _ => new_error(format!("invalid operation: op {}, left {}, right {}", operation, left, right))
    }
}

fn eval_string_infix_expression(operation: String, left: &String, right: &String) -> Object {
    match operation.as_str() {
        "+" => Object::String(left.to_string() + right.as_str()),
        "==" => Object::Boolean(left.to_string() == right.to_string() ),
        "!=" => Object::Boolean(left.to_string() != right.to_string() ),
        _ => new_error(format!("invalid operation: op {}, left {}, right {}", operation, left, right))
    }
}

fn apply_function( function: Object, args: Vec<Object>) -> Option<Object> {
    match function {
        Object::Function {parameters, body, env} => {
            let mut eval = Evaluator::from(env);
            // argument expansion
            for (i, param) in parameters.iter().enumerate() {
                if let Expression::Identifier(param) = param {
                    eval.set(param.to_string(), args[i].clone());
                }
            }

            match eval.eval(Node::Statement(body)) {
                None => None,
                Some(obj) => match obj {
                    Object::ReturnValue(exp) => {
                        Some(*exp)
                    },
                    other => Some(other)
                }
            }
        }
        Object::Builtin(bf) => {
             Some(bf.call(args))
        },
        _ => unreachable!()
    }
}

fn eval_array_index(arr: Vec<Object>, index: i64) -> Object {
    if index < 0 || index >= arr.len() as i64 {
        Object::Null
    } else {
        arr[index as usize].clone()
    }
}

fn eval_hash_index(hash: BTreeMap<HashKey, KeyValue>, index: Object) -> Object {
    let key = HashKey::get_key(&index);
    let pair = hash.get(&key);
    if let Some(pair) = pair {
        *pair.value.clone()
    } else {
        Object::Null
    }
}

#[cfg(test)]
mod test {
    use crate::ast::Node;
    use crate::evaluator::Evaluator;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;

    #[test]
    fn test_eval_integer() {
        struct Test<'a> {
            input: &'a  str,
            expected: i64
        }
        let tests = vec![
            Test{input: "-5", expected: -5},
            Test{input: "5 + 5+5 -7", expected: 8},
            Test{input: "5 + 5*5", expected: 30},
            Test{input: "(5 + 10 * 2 + 15 / 3) * 2 + -10", expected: 50},
        ];

        for tt in tests {
            match test_eval(tt.input) {
                None => panic!("None returned"),
                Some(obj) => {
                    if let Object::Integer(val) = obj {
                        assert_eq!(val, tt.expected);
                    } else {
                        panic!("returned object is not an integer")
                    }
                }
            }

        }
    }

    #[test]
    fn test_eval_bool() {
        struct Test<'a> {
            input: &'a  str,
            expected: bool
        }
        let tests = vec![
            Test{input: "true", expected: true},
            Test{input: "1<2", expected: true},
            Test{input: "1>2", expected: false},
            Test{input: "1>1", expected: false},
            Test{input: "1==2", expected: false},
            Test{input: "1!=2", expected: true},
            Test{input: "true==true", expected: true},
            Test{input: "true!=true", expected: false},
            Test{input: "(1<2)==true", expected: true },
            Test{input: "(1>2)==true", expected: false },
        ];

        for tt in tests {
            match test_eval(tt.input) {
                None => panic!("None returned"),
                Some(obj) => {
                    if let Object::Boolean(val) = obj {
                        assert_eq!(val, tt.expected);
                    } else {
                        panic!("returned object is not a boolean")
                    }
                }
            }

        }
    }

    #[test]
    fn test_eval_bang_op() {
        struct Test<'a> {
            input: &'a  str,
            expected: bool
        }
        let tests = vec![
            Test{input: "!true", expected: false},
            Test{input: "!false", expected: true},
            Test{input: "!5", expected: false},
            Test{input: "!!true", expected: true},
        ];

        for tt in tests {
            match test_eval(tt.input) {
                None => panic!("None returned"),
                Some(obj) => {
                    if let Object::Boolean(val) = obj {
                        assert_eq!(val, tt.expected);
                    } else {
                        panic!("returned object is not a boolean")
                    }
                }
            }

        }
    }

    #[test]
    fn test_eval_if_expression() {
        struct Test<'a> {
            input: &'a  str,
            expected: Option<i64>,
        }
        let tests = vec![
            Test{input: "if(true){10}", expected: Some(10)},
            Test{input: "if(false){10}", expected: None},
            Test{input: "if(1<2){10}", expected: Some(10)},
            Test{input: "if(1>2){10}else{20}", expected: Some(20)},
        ];

        for tt in tests {
            match test_eval(tt.input) {
                None => panic!("None returned"),
                Some(obj) => {
                    match obj {
                        Object::Integer(val) => match tt.expected {
                            None => panic!("expected value is Null, got integer"),
                            Some(i) => assert_eq!(val, i)
                        },
                        Object::Null => if let Some(_) = tt.expected {
                            panic!("expected returned value is integer, got nil")
                        }
                        _ => panic!("returned object is not an integer nor nil")
                    }
                }
            }

        }
    }

    #[test]
    fn test_eval_return_expression() {
        struct Test<'a> {
            input: &'a  str,
            expected: i64,
        }
        let tests = vec![
            Test{input: "return 10", expected: 10},
            Test{input: "return 10; 9", expected: 10},
            Test{input: "return 4+3; 9", expected: 7},
            Test{input: "if(10>1){return 4+3;} ", expected: 7},
            Test{input: r#"if (10>1) {
                if (true) {
                    return 5;
                }
                return 2;
            }"#, expected: 5},
            Test{input: r#"let a = 3; a+5"#, expected: 8},
            Test{input: r#"let f = fn(x){return x+10;};
            13;"#, expected: 13},
            Test{input: r#"let f = fn(x){return x+10;};
            f(5);"#, expected: 15}
        ];

        for tt in tests {
            test_integer(tt.input, tt.expected);
        }
    }

    #[test]
    fn test_eval_let_expression() {
        struct Test<'a> {
            input: &'a  str,
            expected: i64,
        }
        let tests = vec![
            Test{input: r#"let a = 3; a+5"#, expected: 8},
            Test{input: r#"let a = 2+3*5; a"#, expected: 17},
            Test{input: r#"let a = 5; let b = a; b"#, expected: 5},
            Test{input: r#"let a = 5; let b = a; a=2; b"#, expected: 5},
            Test{input: r#"let a = 5; a=2; a"#, expected: 5},
        ];

        for tt in tests {
            test_integer(tt.input, tt.expected);
        }
    }

    #[test]
    fn test_eval_enclosed() {
        struct Test<'a> {
            input: &'a  str,
            expected: i64,
        }
        let tests = vec![
            Test{input: r#"let one = 1; let two = 2; let func = fn(x){x+one;}; func(3)"#, expected: 4},
            Test{input: r#"let one = 1; let two = 2; let func = fn(x){let two = 20; x+one+two;}; func(3)"#, expected: 24},
            Test{input: r#"let one = 1; let two = 2; let func = fn(x){let two = 20; x+one+two;}; func(3)+two"#, expected: 26},
        ];

        for tt in tests {
            test_integer(tt.input, tt.expected);
        }
    }

    #[test]
    fn test_eval_string() {
        struct Test<'a> {
            input: &'a  str,
            expected: String,
        }
        let tests = vec![
            Test{input: r#""hello"+" "+"world""#, expected: "hello world".to_string()},
            Test{input: r#"let hi = "hello"; hi+" "+"world""#, expected: "hello world".to_string()},
        ];

        for tt in tests {
            test_string(tt.input, tt.expected);
        }
    }

    #[test]
    fn test_array_literal() {
        struct Test<'a> {
            input: &'a  str,
            expected: Vec<i64>,
        }
        let tests = vec![
            Test{input: r#"[1, 2*2, 3]"#, expected: vec![1,4,3]},
        ];

        for tt in tests {
            match test_eval(tt.input) {
                None => panic!("None returned"),
                Some(obj) => {
                    match obj {
                        Object::Array(arr) =>  {
                            assert_eq!(arr.len(), tt.expected.len());
                            for (i, v) in arr.iter().enumerate() {
                                match v {
                                    Object::Integer(val) => assert_eq!(*val, tt.expected[i]),
                                    _ => panic!("not integer value is contained")
                                }
                            }
                        },
                        _ => panic!("returned object is not an integer, got {}", obj)
                    }
                }
            }
        }
    }

    #[test]
    fn test_eval_array_index() {
        struct Test<'a> {
            input: &'a  str,
            expected: i64,
        }
        let tests = vec![
            Test{input: r#"[1, 2, 3][0]"#, expected: 1},
            Test{input: r#"[1, 2, 3][1]"#, expected: 2},
            Test{input: r#"[1, 2, 3][1+1]"#, expected: 3},
            Test{input: r#"let arr = [1, 2, 3+2]; arr[1+1]"#, expected: 5},
        ];

        for tt in tests {
            test_integer(tt.input, tt.expected);
        }
    }

    #[test]
    fn test_eval_hash_index() {
        struct Test<'a> {
            input: &'a  str,
            expected: i64,
        }
        let tests = vec![
            Test{input: r#"{"foo": 5}["foo"]"#, expected: 5},
            Test{input: r#"{"foo": 5, "bar": 7}["foo"]"#, expected: 5},
            Test{input: r#"let key = "bar"; {"foo": 5, "bar": 7}[key]"#, expected: 7},
        ];

        for tt in tests {
            test_integer(tt.input, tt.expected);
        }
    }

    /*
    #[test]
    fn test_eval_builtin_function() {
        enum Expect {
            Int(i64),
            String(String),
            Boolean(bool),
            Null
        }
        struct Test<'a> {
            input: &'a  str,
            expected: Expect,
        }
        let tests = vec![
            Test{input: r#"{"foo": 5}["foo"]"#, expected: Expect::Int(5)},
            Test{input: r#"let a = [1, 2]; len(a);"#, expected: Expect::Int(2)},
        ];

        for tt in tests {
            match test_eval(tt.input) {
                None => panic!("None returned"),
                Some(obj) => match obj {
                    Object::Integer(val) =>  {
                        if let Expect::Int(exp) = tt.expected {
                            assert_eq!(val, exp);
                        } else {
                            panic!("returned type is wrong")
                        }
                    },
                    Object::String(st) => {
                        if let Expect::String(exp) = tt.expected {
                            assert_eq!(st, exp);
                        } else {
                            panic!("returned type is wrong")
                        }
                    },
                    Object::Boolean(b) => {
                        if let Expect::Boolean(exp) = tt.expected {
                            assert_eq!(b, exp);
                        } else {
                            panic!("returned type is wrong")
                        }
                    },
                    Object::Null => {
                        match tt.expected {
                            Expect::Null => {},
                            _ => panic!("returned type is wrong")
                        }
                    },
                    _ => panic!("returned object is not an integer, got {}", obj)
                }
            }
        }
    }
     */

    //utils
    fn test_eval(input: &str) -> Option<Object> {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        let mut eval = Evaluator::new();
        eval.eval(Node::Program(program))
    }

    fn test_integer(input: &str, expected: i64) {
        match test_eval(input) {
            None => panic!("None returned"),
            Some(obj) => {
                match obj {
                    Object::Integer(val) =>  assert_eq!(val, expected),
                    _ => panic!("returned object is not an integer, got {}", obj)
                }
            }
        }
    }
    fn test_string(input: &str, expected: String) {
        match test_eval(input) {
            None => panic!("None returned"),
            Some(obj) => {
                match obj {
                    Object::String(val) =>  assert_eq!(val, expected),
                    _ => panic!("returned object is not a string, got {}", obj)
                }
            }
        }
    }
}
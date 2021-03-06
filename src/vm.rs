use byteorder::{BigEndian, ByteOrder};
use crate::code::{Instructions, Opcode, Operation};
use crate::compiler::ByteCode;
use crate::object::Object;

const STACK_SIZE: usize = 2048;

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

pub struct VM {
    constants: Vec<Object>,
    instructions: Instructions,

    stack: Vec<Object>,
    sp: usize, // stack pointer
}

impl VM {
    pub fn new(byte_code: ByteCode) -> Self {
        Self {
            constants: byte_code.constants,
            instructions: byte_code.instructions,

            stack: vec![Object::Null; STACK_SIZE],
            sp: 0,
        }
    }

    pub fn stack_top(&self) -> Option<Object> {
        if self.sp == 0 {
            None
        } else {
            Some(self.stack[self.sp - 1].clone())
        }
    }

    fn push(&mut self, object: Object) -> Result<(),String> {
        if self.sp >= STACK_SIZE {
            Err("stack overflow occurred".to_string())
        } else {
            self.stack[self.sp] = object;
            self.sp += 1;
            Ok(())
        }
    }

    fn pop(&mut self) -> Result<Object,String> {
        if self.sp == 0 {
            Err("stack underflow".to_string())
        } else {
            let o = self.stack[self.sp - 1].clone();
            self.sp -= 1;
            Ok(o)
        }
    }

    pub fn last_popped_stack_elem(&self) -> Object {
        self.stack[self.sp].clone()
    }

    pub fn run(&mut self) -> Result<(), String> {
        //println!("{:?}", self.instructions.to_string());
        let mut ip: usize = 0;
        while ip < self.instructions.len() {
            let op = self.instructions[ip] as Opcode;


            match Operation::from_byte(op) {
                Some(op) => match op {
                    Operation::OpConstant =>{
                        let const_idx = BigEndian::read_u16(&self.instructions[(ip+1)..(ip+3)].to_vec());
                        ip += 2;

                        self.push(self.constants[const_idx as usize].clone())?;
                    },
                    Operation::OpAdd| Operation::OpSub | Operation::OpMul | Operation::OpDiv => {
                        self.execute_binary_operation(op)?;
                    },
                    Operation::OpPop => {
                        self.pop()?;
                    },
                    Operation::OpTrue => {
                        self.push(TRUE)?;
                    },
                    Operation::OpFalse => {
                        self.push(FALSE)?;
                    },
                    Operation::OpEqual | Operation::OpNotEqual | Operation::OpGreaterThan => {
                        self.execute_comparison(op)?;
                    },
                    Operation::OpBang => {
                        self.execute_bang_operator()?;
                    },
                    Operation::OpMinus => {
                        self.execute_minus_operator()?;
                    },
                    Operation::OpJump => {
                        let pos = BigEndian::read_u16(&self.instructions[ip+1..ip+3]);
                        ip = pos as usize - 1;
                    },
                    Operation::OpJumpNotTruthy => {
                        let pos = BigEndian::read_u16(&self.instructions[ip+1..ip+3]);
                        // ??????consequence??????????????????????????????????????????
                        ip += 2;

                        // stack???top???????????????condition????????????
                        let condition = self.pop()?;
                        if !is_truthy(condition) {
                            ip = pos as usize -1;
                        }
                    },
                    Operation::OpNull => {
                        self.push(NULL)?;
                    },
                    _ => unimplemented!(),
                },
                None => return Err(format!("op code {} is invalid: pos {}", self.instructions[ip], ip)),
            }
            ip += 1;
        }
        Ok(())
    }

    fn execute_binary_operation(&mut self, op: Operation) -> Result<(),String> {
        let right = self.pop()?;
        let left = self.pop()?;
        if let (Object::Integer(right), Object::Integer(left)) = (&right, &left) {
            let result = match op {
                Operation::OpAdd => left+right,
                Operation::OpSub => left-right,
                Operation::OpMul => left*right,
                Operation::OpDiv => left/right,
                other => return Err(format!("operator {} is invalid for infix expression between integers", other))
            };
            //println!("{} {} {} {}", right, op, left, result);
            self.push(Object::Integer(result))
        } else {
            Err(format!("invalid operand: left {}, right {}", left, right))
        }
    }

    fn execute_comparison(&mut self, op: Operation) -> Result<(),String> {
        let right = self.pop()?;
        let left = self.pop()?;
        if let (Object::Integer(right), Object::Integer(left)) = (&right, &left) {
            self.execute_integer_comparison(op, *left, *right)
        } else {
            match op {
                Operation::OpEqual => self.push(native_boolean_object(right == left)),
                Operation::OpNotEqual => self.push(native_boolean_object(right != left)),
                other => Err(format!("invalid operand  {}", other)),
            }
        }

    }

    fn execute_integer_comparison(&mut self, op: Operation, left: i64, right: i64) -> Result<(),String>{
        match op {
            Operation::OpEqual => self.push(native_boolean_object(left == right)),
            Operation::OpNotEqual => self.push(native_boolean_object(left != right)),
            Operation::OpGreaterThan => self.push(native_boolean_object(left > right)),
            _ => Err(format!("unknown operator {}", op))
        }
    }


    fn execute_bang_operator(&mut self) -> Result<(), String> {
        let operand = self.pop()?;
        match operand {
            TRUE => self.push(FALSE),
            FALSE => self.push(TRUE),
            NULL => self.push(TRUE),
            _ => self.push(FALSE)
        }
    }

    fn execute_minus_operator(&mut self) -> Result<(), String> {
        let operand = self.pop()?;
        match operand {
            Object::Integer(i) => self.push(Object::Integer(-i)),
            _ => Err(format!("unsupported type for negation: {}", operand))
        }
    }

}

fn native_boolean_object(input: bool) -> Object {
    if input {
        Object::Boolean(true)
    } else {
        Object::Boolean(false)
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Boolean(b) => b,
        Object::Null => false,
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Node, Program};
    use crate::compiler::Compiler;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::object::Object;
    use crate::vm::{NULL, VM};

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            VMTestCase {input: "1", expected: Object::Integer(1)},
            VMTestCase {input: "2", expected: Object::Integer(2)},
            VMTestCase {input: "1 + 2", expected: Object::Integer(3)},
            VMTestCase {input: "1 - 2", expected: Object::Integer(-1)},
            VMTestCase {input: "3 * 2", expected: Object::Integer(6)},
            VMTestCase {input: "6 / 2", expected: Object::Integer(3)},
            VMTestCase {input: "6 + 2 * 2", expected: Object::Integer(10)},
            VMTestCase {input: "-5", expected: Object::Integer(-5)},
            VMTestCase {input: "-5 + 10", expected: Object::Integer(5)},
            VMTestCase {input: "(5 + 10 * 2 + 15 /3) * 2 + -10", expected: Object::Integer(50)},
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_boolean_expressions() {
        let tests = vec![
            VMTestCase { input: "true", expected: Object::Boolean(true)},
            VMTestCase { input: "false", expected: Object::Boolean(false)},
            VMTestCase { input: "1 < 2", expected: Object::Boolean(true)},
            VMTestCase { input: "1 > 2", expected: Object::Boolean(false)},
            VMTestCase { input: "1 < 1", expected: Object::Boolean(false)},
            VMTestCase { input: "1 > 1", expected: Object::Boolean(false)},
            VMTestCase { input: "1 == 1", expected: Object::Boolean(true)},
            VMTestCase { input: "1 != 1", expected: Object::Boolean(false)},
            VMTestCase { input: "true == true", expected: Object::Boolean(true)},
            VMTestCase { input: "false == false", expected: Object::Boolean(true)},
            VMTestCase { input: "true == false", expected: Object::Boolean(false)},
            VMTestCase { input: "true != false", expected: Object::Boolean(true)},
            VMTestCase { input: "(1 < 2) == true", expected: Object::Boolean(true)},
            VMTestCase { input: "(1 < 2) == false", expected: Object::Boolean(false)},
            VMTestCase { input: "(1 > 2) == true", expected: Object::Boolean(false)},
            VMTestCase { input: "(1 > 2) == false", expected: Object::Boolean(true)},
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_conditionals() {
        let tests = vec![
            VMTestCase {input: "if (true) { 10 }", expected: Object::Integer(10)},
            VMTestCase {input: "if (true) { 10 } else { 20 }", expected: Object::Integer(10)},
            VMTestCase {input: "if (false) { 10 } else { 20 }", expected: Object::Integer(20)},
            VMTestCase {input: "if (1) { 10 }", expected: Object::Integer(10)},
            VMTestCase {input: "if (1 < 2) { 10 }", expected: Object::Integer(10)},
            VMTestCase {input: "if (1 < 2) { 10 } else { 20 }", expected: Object::Integer(10)},
            VMTestCase {input: "if (1 > 2) { 10 } else { 20 }", expected: Object::Integer(20)},
            VMTestCase {input: "if (1 > 2) { 10 }", expected: NULL},
            VMTestCase {input: "if (false) { 10 }", expected: NULL},
            VMTestCase {input: "!(if (false) { 5; })", expected: Object::Boolean(true)},
            VMTestCase {input: "if (if (false) { 5 }) { 10 } else { 20 }", expected: Object::Integer(20)},
        ];
        run_vm_tests(tests);
    }

    fn parse(input: &str) -> Program {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        p.parse_program()
    }

    struct VMTestCase<'a> {
        input: &'a str,
        expected: Object,
    }

    fn run_vm_tests(tests: Vec<VMTestCase>) {
        for tt in tests {
            let program = parse(tt.input);

            let mut comp = Compiler::new();
            if let Err(err) = comp.compile(Node::Program(program)) {
                panic!("failed to compile: {}", err);
            }

            let mut vm = VM::new(comp.byte_code());
            if let Err(err) = vm.run() {
                panic!("failed to run vm: {}", err);
            }

            //let stack_elem = vm.stack_top().expect("failed to get the top object in the stack, because the sp is 0");
            let stack_elem = vm.last_popped_stack_elem();
            test_expected_object(tt.expected, stack_elem);
        }
    }

    fn test_expected_object(expected: Object, actual: Object) {
        assert_eq!(actual, expected);
    }
}

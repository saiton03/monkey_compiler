use byteorder::{BigEndian, ByteOrder};
use crate::code::{Instructions, Opcode, Operation};
use crate::compiler::ByteCode;
use crate::object::Object;

const STACK_SIZE: usize = 2048;

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
            Some(self.stack[self.sp-1].clone())
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
                    Operation::OpAdd => {
                        let right = self.pop()?;
                        let left = self.pop()?;
                        if let (Object::Integer(right), Object::Integer(left)) = (&right, &left) {
                            self.push(Object::Integer(right+left))?;
                        } else {
                            return Err(format!("invalid operand for add: left {}, right {}", left, right));
                        }
                    }
                },
                None => return Err(format!("op code {} is invalid: pos {}", self.instructions[ip], ip)),
            }
            ip += 1;
        }
        Ok(())
    }


}

#[cfg(test)]
mod tests {
    use crate::ast::{Node, Program};
    use crate::compiler::Compiler;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::object::Object;
    use crate::vm::VM;

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            VMTestCase {input: "1", expected: Object::Integer(1)},
            VMTestCase {input: "2", expected: Object::Integer(2)},
            VMTestCase {input: "1 + 2", expected: Object::Integer(3)},
        ];
        run_vm_tests(tests);
    }


    fn parse(input: &str) -> Program {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        p.parse_program()
    }

    fn test_integer_object(expected: i64, actual: Object) {
        let integer = match actual {
            Object::Integer(i) => i,
            other => panic!("object is not integer, got {}", other)
        };
        assert_eq!(integer, expected);
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

            let stack_elem = vm.stack_top().expect("failed to get the top object in the stack, because the sp is 0");
            test_expected_object(tt.expected, stack_elem);
        }
    }

    fn test_expected_object(expected: Object, actual: Object) {
        assert_eq!(actual, expected);
    }
}

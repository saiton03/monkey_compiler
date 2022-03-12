use crate::ast::{Expression, Node, Statement};
use crate::code::{Instructions, make, Operation};
use crate::object::Object;

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: Instructions::new(vec![]),
            constants: vec![]
        }
    }

    pub fn compile(&mut self, node: Node) -> Result<(), String> {
        match node {
            Node::Program(p) => {
                for s in p.statements {
                    self.compile(Node::Statement(s))?
                }
                Ok(())
            },
            Node::Statement(stmt) => match stmt {
                Statement::ExpressionStatement(exp) => {
                    self.compile(Node::Expression(exp))?;
                    self.emit(Operation::OpPop, vec![]);
                    Ok(())
                },
                _ => unimplemented!()
            }
            Node::Expression(exp) => match exp {
                Expression::InfixExpression {operation, left, right} => {
                    if &operation == "<" {
                        self.compile(Node::Expression(*right))?;
                        self.compile(Node::Expression(*left))?;

                        self.emit(Operation::OpGreaterThan, vec![]);
                        return Ok(());
                    }


                    self.compile(Node::Expression(*left))?;
                    self.compile(Node::Expression(*right))?;

                    match operation.as_ref() {
                        "+" => {
                            self.emit(Operation::OpAdd, vec![]);
                        },
                        "-" => {
                            self.emit(Operation::OpSub, vec![]);
                        },
                        "*" => {
                            self.emit(Operation::OpMul, vec![]);
                        },
                        "/" => {
                            self.emit(Operation::OpDiv, vec![]);
                        },
                        ">" => {
                            self.emit(Operation::OpGreaterThan, vec![]);
                        },
                        "==" => {
                            self.emit(Operation::OpEqual, vec![]);
                        },
                        "!=" => {
                            self.emit(Operation::OpNotEqual, vec![]);
                        },
                        _ => return Err(format!("unknown operator {}", operation))
                    }
                    Ok(())
                },
                Expression::PrefixExpression {operation, right} => {
                    self.compile(Node::Expression(*right))?;

                    match operation.as_ref() {
                        "!" => {
                            self.emit(Operation::OpBang, vec![]);
                        },
                        "-" => {
                            self.emit(Operation::OpMinus, vec![]);
                        },
                        _ => return Err(format!("unknown operator {}", operation)),
                    }
                    Ok(())
                }
                Expression::Integer(i) => {
                    let integer = Object::Integer(i);
                    let pos = self.add_constant(integer) as i32;
                    self.emit(Operation::OpConstant, vec![pos]);
                    Ok(())
                },
                Expression::Boolean(b) => {
                    if b {
                        self.emit(Operation::OpTrue, vec![]);
                    } else {
                        self.emit(Operation::OpFalse, vec![]);
                    }
                    Ok(())
                }
                _ => unimplemented!(),
            },
        }
    }

    fn add_constant(&mut self, obj: Object) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1 // returns the object's index
    }

    fn emit(&mut self, op: Operation, operands: Vec<i32>) -> usize {
        let ins = make(op.as_byte(), &operands).expect("make instruction is failed");
        self.add_instruction(ins)
    }

    fn add_instruction(&mut self, mut ins: Vec<u8>) -> usize {
        let pos_new_instruction = self.instructions.len();
        self.instructions.append_vec(&mut ins);
        pos_new_instruction
    }

    pub fn byte_code(self) -> ByteCode {
        ByteCode{
            instructions: self.instructions,
            constants: self.constants,
        }
    }
}


pub struct ByteCode {
    pub instructions: Instructions,
    pub constants: Vec<Object>
}


#[cfg(test)]
mod test {
    use itertools::concat;
    use crate::code::{Instructions, make, Operation};
    use Object::Integer;
    use crate::ast::{Node, Program};
    use crate::compiler::Compiler;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            CompilerTestCase {
                input: "1 + 2",
                exp_constants: vec![Integer(1), Integer(2)],
                exp_instructions: vec![
                    Instructions::new(make(Operation::OpConstant.as_byte(), &vec![0]).unwrap()),
                    Instructions::new(make(Operation::OpConstant.as_byte(), &vec![1]).unwrap()),
                    Instructions::new(make(Operation::OpAdd.as_byte(), &vec![]).unwrap()),
                    Instructions::new(make(Operation::OpPop.as_byte(), &vec![]).unwrap())
                ]
            },
            CompilerTestCase {
                input: "1 - 2",
                exp_constants: vec![Integer(1), Integer(2)],
                exp_instructions: vec![
                    Instructions::new(make(Operation::OpConstant.as_byte(), &vec![0]).unwrap()),
                    Instructions::new(make(Operation::OpConstant.as_byte(), &vec![1]).unwrap()),
                    Instructions::new(make(Operation::OpSub.as_byte(), &vec![]).unwrap()),
                    Instructions::new(make(Operation::OpPop.as_byte(), &vec![]).unwrap())
                ]
            },
            CompilerTestCase {
                input: "1 * 2",
                exp_constants: vec![Integer(1), Integer(2)],
                exp_instructions: vec![
                    Instructions::new(make(Operation::OpConstant.as_byte(), &vec![0]).unwrap()),
                    Instructions::new(make(Operation::OpConstant.as_byte(), &vec![1]).unwrap()),
                    Instructions::new(make(Operation::OpMul.as_byte(), &vec![]).unwrap()),
                    Instructions::new(make(Operation::OpPop.as_byte(), &vec![]).unwrap())
                ]
            },
            CompilerTestCase {
                input: "1 / 2",
                exp_constants: vec![Integer(1), Integer(2)],
                exp_instructions: vec![
                    Instructions::new(make(Operation::OpConstant.as_byte(), &vec![0]).unwrap()),
                    Instructions::new(make(Operation::OpConstant.as_byte(), &vec![1]).unwrap()),
                    Instructions::new(make(Operation::OpDiv.as_byte(), &vec![]).unwrap()),
                    Instructions::new(make(Operation::OpPop.as_byte(), &vec![]).unwrap())
                ]
            },
            CompilerTestCase {
                input: "-1",
                exp_constants: vec![Integer(1)],
                exp_instructions: vec![
                    Instructions::new(make(Operation::OpConstant.as_byte(), &vec![0]).unwrap()),
                    Instructions::new(make(Operation::OpMinus.as_byte(), &vec![]).unwrap()),
                    Instructions::new(make(Operation::OpPop.as_byte(), &vec![]).unwrap())
                ]
            },
            CompilerTestCase {
                input: "1; 2",
                exp_constants: vec![Integer(1), Integer(2)],
                exp_instructions: vec![
                    Instructions::new(make(Operation::OpConstant.as_byte(), &vec![0]).unwrap()),
                    Instructions::new(make(Operation::OpPop.as_byte(), &vec![]).unwrap()),
                    Instructions::new(make(Operation::OpConstant.as_byte(), &vec![1]).unwrap()),
                    Instructions::new(make(Operation::OpPop.as_byte(), &vec![]).unwrap())
                ]
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_boolean_expressions() {
        let tests = vec![
            CompilerTestCase {
                input: "true",
                exp_constants: vec![],
                exp_instructions: vec![
                    Instructions::new(make(Operation::OpTrue.as_byte(), &vec![]).unwrap()),
                    Instructions::new(make(Operation::OpPop.as_byte(), &vec![]).unwrap()),
                ]
            },
            CompilerTestCase {
                input: "false",
                exp_constants: vec![],
                exp_instructions: vec![
                    Instructions::new(make(Operation::OpFalse.as_byte(), &vec![]).unwrap()),
                    Instructions::new(make(Operation::OpPop.as_byte(), &vec![]).unwrap()),
                ]
            },
            CompilerTestCase {
                input: "1 > 2",
                exp_constants: vec![Integer(1), Integer(2)],
                exp_instructions: vec![
                    Instructions::new(make(Operation::OpConstant.as_byte(), &vec![0]).unwrap()),
                    Instructions::new(make(Operation::OpConstant.as_byte(), &vec![1]).unwrap()),
                    Instructions::new(make(Operation::OpGreaterThan.as_byte(), &vec![]).unwrap()),
                    Instructions::new(make(Operation::OpPop.as_byte(), &vec![]).unwrap()),
                ]
            },
            CompilerTestCase {
                input: "1 < 2",
                exp_constants: vec![Integer(2), Integer(1)],
                exp_instructions: vec![
                    Instructions::new(make(Operation::OpConstant.as_byte(), &vec![0]).unwrap()),
                    Instructions::new(make(Operation::OpConstant.as_byte(), &vec![1]).unwrap()),
                    Instructions::new(make(Operation::OpGreaterThan.as_byte(), &vec![]).unwrap()),
                    Instructions::new(make(Operation::OpPop.as_byte(), &vec![]).unwrap()),
                ]
            },
            CompilerTestCase {
                input: "1 == 2",
                exp_constants: vec![Integer(1), Integer(2)],
                exp_instructions: vec![
                    Instructions::new(make(Operation::OpConstant.as_byte(), &vec![0]).unwrap()),
                    Instructions::new(make(Operation::OpConstant.as_byte(), &vec![1]).unwrap()),
                    Instructions::new(make(Operation::OpEqual.as_byte(), &vec![]).unwrap()),
                    Instructions::new(make(Operation::OpPop.as_byte(), &vec![]).unwrap()),
                ]
            },
            CompilerTestCase {
                input: "1 != 2",
                exp_constants: vec![Integer(1), Integer(2)],
                exp_instructions: vec![
                    Instructions::new(make(Operation::OpConstant.as_byte(), &vec![0]).unwrap()),
                    Instructions::new(make(Operation::OpConstant.as_byte(), &vec![1]).unwrap()),
                    Instructions::new(make(Operation::OpNotEqual.as_byte(), &vec![]).unwrap()),
                    Instructions::new(make(Operation::OpPop.as_byte(), &vec![]).unwrap()),
                ]
            },
            CompilerTestCase {
                input: "true == false",
                exp_constants: vec![],
                exp_instructions: vec![
                    Instructions::new(make(Operation::OpTrue.as_byte(), &vec![]).unwrap()),
                    Instructions::new(make(Operation::OpFalse.as_byte(), &vec![]).unwrap()),
                    Instructions::new(make(Operation::OpEqual.as_byte(), &vec![]).unwrap()),
                    Instructions::new(make(Operation::OpPop.as_byte(), &vec![]).unwrap()),
                ]
            },
            CompilerTestCase {
                input: "true != false",
                exp_constants: vec![],
                exp_instructions: vec![
                    Instructions::new(make(Operation::OpTrue.as_byte(), &vec![]).unwrap()),
                    Instructions::new(make(Operation::OpFalse.as_byte(), &vec![]).unwrap()),
                    Instructions::new(make(Operation::OpNotEqual.as_byte(), &vec![]).unwrap()),
                    Instructions::new(make(Operation::OpPop.as_byte(), &vec![]).unwrap()),
                ]
            },
            CompilerTestCase {
                input: "!true",
                exp_constants: vec![],
                exp_instructions: vec![
                    Instructions::new(make(Operation::OpTrue.as_byte(), &vec![]).unwrap()),
                    Instructions::new(make(Operation::OpBang.as_byte(), &vec![]).unwrap()),
                    Instructions::new(make(Operation::OpPop.as_byte(), &vec![]).unwrap()),
                ]
            },
        ];
        run_compiler_tests(tests);
    }

    struct  CompilerTestCase<'a> {
        input: &'a str,
        exp_constants: Vec<Object>,
        exp_instructions: Vec<Instructions>
    }

    fn run_compiler_tests(tests: Vec<CompilerTestCase>) {
        for tt in tests {
            let program = parse(tt.input);

            let mut compiler = Compiler::new();
            match compiler.compile(Node::Program(program)) {
                Err(e) => panic!("compile error {}", e),
                Ok(_) => {},
            }

            let byte_code = compiler.byte_code();
            test_instructions(tt.exp_instructions, byte_code.instructions);

            test_constant(tt.exp_constants, byte_code.constants);

        }
    }

    fn parse(input: &str) -> Program {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        p.parse_program()
    }

    fn test_instructions(expected: Vec<Instructions>, actual: Instructions) {
        let concatted = concat(expected.into_iter().map(|i| i.to_vec()).collect::<Vec<_>>());
        let concatted = Instructions::new(concatted);
        assert_eq!(concatted, actual, "\ninstruction differs:\nwant={}got={}", concatted.to_string().unwrap(), actual.to_string().unwrap());
    }

    fn test_constant(expected: Vec<Object>, actual: Vec<Object>) {
        if expected.len() !=  actual.len() {
            panic!("the number of objects is differs: expected {}, actual {}", expected.len(), actual.len())
        }
        assert_eq!(expected, actual);
    }
}

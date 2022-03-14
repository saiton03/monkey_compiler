use std::rc::Rc;
use crate::ast::{Expression, Node, Statement};
use crate::code::{Instructions, make, Opcode, Operation};
use crate::object::Object;

#[derive(Default)]
struct EmittedInstruction {
    op_code: Opcode,
    position: usize
}

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
    
    last_instruction: Rc<EmittedInstruction>,
    previous_instruction: Rc<EmittedInstruction>
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: Instructions::new(vec![]),
            constants: vec![],

            last_instruction: Default::default(),
            previous_instruction: Default::default(),
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
                Statement::BlockStatement(stmts) => {
                    for stmt in stmts {
                        self.compile(Node::Statement(stmt))?;
                    }
                    Ok(())
                }
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
                },
                Expression::IfExpression {condition, consequence, alternative} => {
                    self.compile(Node::Expression(*condition))?;

                    let jump_not_truthy_pos =self.emit(Operation::OpJumpNotTruthy, vec![9999]);

                    self.compile(Node::Statement(*consequence))?;

                    if self.last_instruction_is_pop() {
                        self.remove_last_pop();
                    }

                    let jump_pos = self.emit(Operation::OpJump, vec![9999]);

                    let after_consequence_pos = self.instructions.len();
                    self.change_operand(jump_not_truthy_pos, after_consequence_pos as i32)?;

                    match alternative {
                        None => {
                            self.emit(Operation::OpNull, vec![]);
                        },
                        Some(alt) => {
                            self.compile(Node::Statement(*alt))?;

                            if self.last_instruction_is_pop() {
                                self.remove_last_pop();
                            }
                        }
                    }
                    let after_alternative_pos = self.instructions.len();
                    self.change_operand(jump_pos, after_alternative_pos as i32)?;

                    Ok(())
                },
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
        let pos = self.add_instruction(ins);
        self.set_last_instruction(op, pos);
        pos
    }

    fn add_instruction(&mut self, mut ins: Vec<u8>) -> usize {
        let pos_new_instruction = self.instructions.len();
        self.instructions.append_vec(&mut ins);
        pos_new_instruction
    }

    fn set_last_instruction(&mut self, op: Operation, pos: usize) {
        self.previous_instruction = Rc::clone(&self.last_instruction);
        self.last_instruction = Rc::new(EmittedInstruction{ op_code: op.as_byte(), position: pos });
    }

    fn last_instruction_is_pop(&self) -> bool {
        self.last_instruction.op_code == Operation::OpPop.as_byte()
    }


    fn remove_last_pop(&mut self) {
        self.instructions.pop();
        self.last_instruction = Rc::clone(&self.previous_instruction);
    }

    fn replace_instructions(&mut self, pos: usize, new_instruction: Instructions) -> Result<(),String> {
        if pos + new_instruction.len() > self.instructions.len() {
            return Err("invalid position".to_string());
        }
        for i in 0..new_instruction.len() {
            self.instructions[pos+i] = new_instruction[i];
        }
        Ok(())
    }

    fn change_operand(&mut self, pos: usize, operand: i32) -> Result<(), String> {
        let op = self.instructions[pos];
        let new_instruction = make(op, &vec![operand]).ok_or("making new instruction failed".to_string())?;

        self.replace_instructions(pos, Instructions::new(new_instruction))
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

    #[test]
    fn test_conditions() {
        let tests = vec![
            CompilerTestCase {
                input: "if (true) { 10 }; 3333;",
                exp_constants: vec![Integer(10), Integer(3333)],
                exp_instructions: vec![
                    Instructions::new(make(Operation::OpTrue.as_byte(), &vec![]).unwrap()),
                    Instructions::new(make(Operation::OpJumpNotTruthy.as_byte(), &vec![10]).unwrap()),
                    Instructions::new(make(Operation::OpConstant.as_byte(), &vec![0]).unwrap()),
                    Instructions::new(make(Operation::OpJump.as_byte(), &vec![11]).unwrap()),
                    Instructions::new(make(Operation::OpNull.as_byte(), &vec![]).unwrap()),
                    Instructions::new(make(Operation::OpPop.as_byte(), &vec![]).unwrap()),
                    Instructions::new(make(Operation::OpConstant.as_byte(), &vec![1]).unwrap()),
                    Instructions::new(make(Operation::OpPop.as_byte(), &vec![]).unwrap()),
                ]
            },
            CompilerTestCase {
                input: "if (true) { 10 } else { 20 }; 3333;",
                exp_constants: vec![Integer(10), Integer(20), Integer(3333)],
                exp_instructions: vec![
                    Instructions::new(make(Operation::OpTrue.as_byte(), &vec![]).unwrap()),
                    Instructions::new(make(Operation::OpJumpNotTruthy.as_byte(), &vec![10]).unwrap()),
                    Instructions::new(make(Operation::OpConstant.as_byte(), &vec![0]).unwrap()),
                    Instructions::new(make(Operation::OpJump.as_byte(), &vec![13]).unwrap()),
                    Instructions::new(make(Operation::OpConstant.as_byte(), &vec![1]).unwrap()),
                    Instructions::new(make(Operation::OpPop.as_byte(), &vec![]).unwrap()),
                    Instructions::new(make(Operation::OpConstant.as_byte(), &vec![2]).unwrap()),
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

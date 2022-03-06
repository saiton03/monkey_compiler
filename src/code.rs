use std::{fmt, ops};
use std::fmt::Formatter;
use std::ops::Range;
use byteorder::{BigEndian, ByteOrder};

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Instructions(Vec<u8>);

impl Instructions {
    pub fn new(vec: Vec<u8>) -> Self {
        Instructions(vec)
    }

    fn format_instruction(def: &Definition, operands: Vec<i32>) -> Result<String, String> {
        let op_count = def.operand_width.len();

        if operands.len() != op_count {
            return Err(format!("operand len {} does not match defined {}", operands.len(), op_count));
        }

        match op_count {
            0 => Ok(def.name.to_string()),
            1 => Ok(format!("{} {}", def.name, operands[0])),
            _ => Err(format!("unhandled op_count for {}", def.name))
        }
    }

    pub fn to_string(&self) -> Result<String, String> {
        let mut out = String::new();

        let mut pos = 0;
        while pos < self.0.len() {
            let def = match DEFINITIONS.get(self.0[pos] as usize) {
                None => return Err(format!("invalid opcode appeared: pos {}, value {}", pos, self.0[pos])),
                Some(def) => def,
            };

            let (operands, read) = read_operands(&def, &Instructions(self.0[(pos+1)..].to_vec()))?;

            out += format!("{:0>4} {}\n", pos, Self::format_instruction(&def, operands)?).as_str();
            pos += 1 + read as usize;
        }
        Ok(out)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn append_vec(&mut self, other: &mut Vec<u8>) {
        self.0.append(other)
    }

    pub fn append(&mut self, other: &mut Self) {
        self.append_vec(&mut other.0);
    }

    pub fn to_vec(self) -> Vec<u8> {
        self.0
    }
}

impl ops::Index<usize> for Instructions {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl ops::Index<ops::Range<usize>> for Instructions {
    type Output = [u8];

    fn index(&self, index: Range<usize>) -> &Self::Output {
        &self.0[index]
    }
}


pub type Opcode = u8;

#[derive(Eq, PartialEq)]
pub struct Definition<'a> {
    name: &'a str,
    operand_width: &'a [i32],
}

pub const DEFINITIONS: &[Definition] = &[
    Definition {
        name: "OpConstant",
        operand_width: &[2],
    },
    Definition {
        name: "OpAdd",
        operand_width: &[],
    }
];

pub fn lookup(op_code: &Operation) -> &Definition {
    &DEFINITIONS[op_code.as_byte() as usize]
}

// rustでは値を明示しないと0から順番に数字がつく
#[derive(Copy, Clone)]
pub enum Operation {
    OpConstant,
    OpAdd,
}

impl Operation {
    pub fn from_byte(byte: u8) -> Option<Self> {
        match byte {
            0 => Some(Operation::OpConstant),
            1 => Some(Operation::OpAdd),
            _ => None,
        }
    }

    pub fn as_byte(&self) -> Opcode {
        *self as Opcode
    }

}

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Operation::OpConstant => write!(f, "OpConstant"),
            Operation::OpAdd => write!(f, "OpAdd"),
        }
    }
}

pub fn make(op: Opcode, operands: &Vec<i32>) -> Option<Vec<u8>> {
    let def = DEFINITIONS.get(op as usize)?;

    let mut instruction_len = 1;
    for w in def.operand_width {
        instruction_len += *w;
    }

    let mut instruction = Vec::with_capacity(instruction_len as usize);
    instruction.push(op as u8);

    for (i, o) in operands.iter().enumerate() {
        let width =  def.operand_width[i];
        match width{
            2 => {
                let mut buf: Vec<u8> = Vec::from([0; 2]);
                BigEndian::write_u16(&mut buf, *o as u16);
                instruction.append(&mut buf);
            },
            _ => unreachable!(),
        }
    }

    Some(instruction)
}

// read_operands parses operand part in an instruction and returns a list of operands and an operands part length
pub fn read_operands(def: &Definition, ins: &Instructions) -> Result<(Vec<i32>, i32), String> {
    let mut operands = Vec::with_capacity(def.operand_width.len() as usize);
    let mut offset: usize = 0;

    for width in def.operand_width {
        match width {
            2 => {
                operands.push(BigEndian::read_u16(&ins.0[offset..]) as i32)
            },
            other => return Err(format!("unsupported width: {}", other))
        }
        offset += *width as usize;
    }

    Ok((operands, offset as i32))
}

#[cfg(test)]
mod test{
    use itertools::concat;
    use crate::code::{Instructions, lookup, make, Opcode, Operation, read_operands};

    #[test]
    fn test_make() {
        //let def = lookup(op);
        struct Test{
            op: Opcode,
            operands: Vec<i32>,
            expected: Vec<u8>,
        }
        let tests = vec![
            Test{op: Operation::OpConstant.as_byte(),  operands: Vec::from([65534]), expected: Vec::from([Operation::OpConstant.as_byte(), 255, 254])},
            Test{op: Operation::OpAdd.as_byte(),  operands: Vec::new(), expected: Vec::from([Operation::OpAdd.as_byte()])}
        ];

        for tt in tests {
            let instruction = make(tt.op, &tt.operands);
            match instruction {
                None => panic!("instruction is None"),
                Some(i) => {
                    assert_eq!(i.len(), tt.expected.len());
                    assert_eq!(i, tt.expected);
                }
            }
        }
    }

    #[test]
    fn test_instructions_string() {
        let instructions = vec![
            make(Operation::OpAdd.as_byte(), &vec![]).unwrap(),
            make(Operation::OpConstant.as_byte(), &vec![2]).unwrap(),
            make(Operation::OpConstant.as_byte(), &vec![65535]).unwrap(),
        ];

        let expected = r#"0000 OpAdd
0001 OpConstant 2
0004 OpConstant 65535
"#;

        let concatted = concat(instructions);
        let concatted = Instructions(concatted);
        assert_eq!(concatted.to_string().expect("failed to converting an instructions to a string"), expected.to_string());
    }

    #[test]
    fn test_read_operands() {
        struct Test {
            op: Operation,
            operands: Vec<i32>,
            byte_read: i32,
        }
        let tests = vec![
            Test{op: Operation::OpConstant, operands: vec![65535], byte_read: 2},
        ];

        for tt in tests {
            let instruction = make(tt.op.as_byte(), &tt.operands).expect("make returned None");
            let def = lookup(&tt.op);

            let (operands, n) = read_operands(&def, &Instructions(instruction[1..].to_vec())).unwrap();

            assert_eq!(n, tt.byte_read);

            assert_eq!(operands, tt.operands);

        }

    }
}
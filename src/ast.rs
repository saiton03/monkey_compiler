use std::string::String;
use std::collections::BTreeMap;
use std::fmt;
use std::fmt::Formatter;
use crate::token::Token;

pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for stmt in self.statements.iter() {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum Statement {
    LetStatement{identifier: Expression, value: Expression},
    ReturnStatement(Expression),
    ExpressionStatement(Expression),
    BlockStatement(Vec<Statement>)
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Statement::LetStatement { identifier, value } => {
                write!(f, "let {} = {}", identifier, value)?
            },
            Statement::ReturnStatement(ret) => {
                write!(f, "return {}", ret)?
            },
            Statement::ExpressionStatement(exp) => {
                write!(f, "{}", exp)?
            },
            Statement::BlockStatement(block) => {
                for stmt in block.iter() {
                    write!(f, "{}", stmt)?
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum Expression {
    Identifier(String),
    Boolean(bool),
    Integer(i64),
    String(String),
    Array(Vec<Expression>),
    Hash(BTreeMap<Box<Expression>, Box<Expression>>),
    PrefixExpression{
        operation: String,
        right: Box<Expression>
    },
    InfixExpression {
        operation: String,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    IfExpression{
        condition: Box<Expression>,
        consequence: Box<Statement>,
        alternative: Option<Box<Statement>>,
    },
    Function{
        parameters: Vec<Expression>,
        body: Box<Statement>,
    },
    CallExpression{
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
    IndexExpression{
        left: Box<Expression>,
        index: Box<Expression>,
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier(ident) => {write!(f, "{}", ident)},
            Expression::Boolean(b) => {write!(f, "{}", b)},
            Expression::Integer(int) => {write!(f, "{}", int)},
            Expression::String(st) => {write!(f, "{}", st)},
            Expression::Array(exps) => {
                write!(f, "[{}]", exps.iter().
                    map(|exp| format!("{}", exp)).
                    collect::<Vec<_>>().join(", "))
            },
            Expression::Hash(hash) => {
                write!(f, "{{{}}}", hash.iter().
                    map(|kv| format!("{}: {}", kv.0, kv.1)).
                    collect::<Vec<_>>().join(", "))
            },
            Expression::PrefixExpression { operation, right } => {
                write!(f, "({}{})", operation, right)
            },
            Expression::InfixExpression { operation, left, right } => {
                write!(f, "({} {} {})", left, operation, right)
            }
            Expression::IfExpression { condition, consequence, alternative } => {
                write!(f, "if {} {{{}}}{}", condition, consequence,
                       match alternative {
                           None => {
                               "".to_string()
                           },
                           Some(alt) => {
                               format!(" else {{{}}}", alt)
                           }
                       })
            },
            Expression::Function { parameters, body } => {
                write!(f, "fn ({}) {{{}}}",
                    parameters.iter().map(|exp| format!("{}", exp)).
                        collect::<Vec<_>>().join(", "),
                    body
                )
            },
            Expression::CallExpression { function, arguments } => {
                write!(f, "{}({})", function,
                    arguments.iter().map(|arg| format!("{}", arg)).
                        collect::<Vec::<_>>().join(", ")
                )
            }
            Expression::IndexExpression { left, index} => {
                write!(f, "({}[{}])", left, index)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::ast::{Expression, Program, Statement};

    #[test]
    fn test_ast() {
        let program = Program {
            statements: vec![
                Statement::LetStatement {
                    identifier: Expression::Identifier("a".to_string()),
                    value: Expression::Integer(12),
                }
            ],
        };

        let out = format!("{}", program);
        assert_eq!(out, "let a = 12")
    }
}


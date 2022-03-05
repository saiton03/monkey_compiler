use core::fmt;
use std::collections::BTreeMap;
use std::fmt::{format, Formatter, write};
use std::ops::Deref;
use crate::ast::{Expression, Program, Statement};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone)]
enum Precedence {
    LOWEST,
    EQUALS, // ==
    LessGreater, // > <
    SUM, // +, -
    PRODUCT, // *, /
    PREFIX, // -X, !X
    CALL,
    INDEX
}

impl fmt::Display for Precedence {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            Precedence::LOWEST => write!(f, "LOWEST"),
            Precedence::EQUALS => write!(f, "EQUALS"),
            Precedence::LessGreater => write!(f, "LessGreater"),
            Precedence::SUM => write!(f, "SUM"),
            Precedence::PRODUCT => write!(f, "PRODUCT"),
            Precedence::PREFIX => write!(f, "PREFIX"),
            Precedence::CALL => write!(f, "CALL"),
            Precedence::INDEX => write!(f, "INDEX")
        }
    }
}

fn get_precedence(token: TokenType) -> Option<Precedence> {
    match token {
        TokenType::EQ | TokenType::NotEq => Some(Precedence::EQUALS),
        TokenType::LT | TokenType::GT => Some(Precedence::LessGreater),
        TokenType::PLUS | TokenType::MINUS => Some(Precedence::SUM),
        TokenType::ASTERISK | TokenType::SLASH => Some(Precedence::PRODUCT),
        TokenType::LPAREN => Some(Precedence::CALL),
        TokenType::LBRACKET => Some(Precedence::INDEX),
        _ => None
    }
}

type PrefixParseFn = fn(&mut Parser) -> Option<Expression>;
type InfixParseFn = fn(Expression) -> Option<Expression>;

pub struct Parser {
    l: Lexer,
    errors: Vec<String>,

    cur_token: Option<Token>,
    peek_token: Option<Token>,
}

impl Parser {

    pub fn new(l: Lexer) -> Self {
        let mut p = Parser {
            l,
            errors: Vec::new(),

            cur_token: None,
            peek_token: None,
        };

        // filling the cur_token
        p.next_token();
        p.next_token();

        p
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = Some(self.l.next_token());
    }

    fn cur_token_is(&self, t: TokenType) -> bool {
        match &self.cur_token {
            None => false,
            Some(ct) => ct.token_type == t,
        }
    }

    fn peek_token_is(&self, t: TokenType) -> bool {
        match &self.peek_token {
            None => false,
            Some(pt) => pt.token_type == t,
        }
    }

    fn expect_peek(&mut self, t: TokenType) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            self.peek_error(t);
            false
        }
    }

    pub fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn peek_error(&mut self, t: TokenType) {
        self.errors.push(format!("expected next token to be {}, got {} instead",
            t, match &self.peek_token {
                None => "<none>".to_string(),
                Some(pt) => pt.token_type.to_string(),
            }))
    }

    fn no_prefix_fn_error(&mut self, t: TokenType) {
        self.errors.push(format!("no prefix function for {} found", t))
    }

    pub fn parse_program(&mut self) -> Program {
        let mut p = Program{
            statements: Vec::new(),
        };

        while !self.cur_token_is(TokenType::EOF) {
            let stmt = self.parse_statement();
            match stmt {
                None => {},
                Some(s) => p.statements.push(s)
            }
            self.next_token();
        }
        p
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match &self.cur_token{
            None => None,
            Some(ct) => match ct.token_type {
                TokenType::LET => self.parse_let_statement(),
                TokenType::RETURN => self.parse_return_statement(),
                _ => self.parse_expression_statement()
            }
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        if !self.expect_peek(TokenType::IDENT) {
            return None;
        }

        let identifier = Expression::Identifier(self.cur_token.as_ref()?.literal.clone());

        if !self.expect_peek(TokenType::ASSIGN) {
            return None;
        }
        self.next_token();

        let value = self.parse_expression(Precedence::LOWEST)?;

        let stmt = Statement::LetStatement {
            identifier,
            value
        };
        Some(stmt)
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();
        let ret_val = self.parse_expression(Precedence::LOWEST)?;
        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }
        Some(Statement::ReturnStatement(ret_val))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expression = self.parse_expression(Precedence::LOWEST)?;

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }
        Some(Statement::ExpressionStatement(expression))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let mut left_exp = match &self.cur_token.as_ref()?.token_type {
            TokenType::IDENT => self.parse_identifier(),
            TokenType::INT => self.parse_integer_literal(),
            TokenType::STRING => self.parse_string_literal(),
            TokenType::MINUS | TokenType::BANG => self.parse_prefix_expression(),
            TokenType::LBRACKET => self.parse_array_literal(),
            TokenType::LPAREN => self.parse_grouped_expression(),
            TokenType::LBRACE => self.parse_hash_literal(),
            TokenType::TRUE | TokenType::FALSE => self.parse_boolean(),
            TokenType::IF => self.parse_if_expression(),
            TokenType::FUNCTION => self.parse_function_literal(),
            _ => None,
        }?;

        //println!("leftexp: {}", left_exp);
        //println!("{}, {}: {}", precedence, self.peek_precedence(), self.peek_token.as_ref()?);

        while !self.peek_token_is(TokenType::SEMICOLON) && precedence < self.peek_precedence() {
            left_exp = match &self.peek_token.as_ref()?.token_type {
                TokenType::PLUS | TokenType ::MINUS | TokenType::ASTERISK | TokenType:: SLASH |
                    TokenType::EQ | TokenType::NotEq | TokenType::LT | TokenType::GT => {
                    self.next_token();
                    self.parse_infix_expression(left_exp)?
                },
                TokenType::LPAREN => {
                    self.next_token();
                    self.parse_call_expression(left_exp)?
                },
                TokenType::LBRACKET => {
                    self.next_token();
                    self.parse_index_expression(left_exp)?
                },
                _ => left_exp
            }
        }
        Some(left_exp)
    }

    fn peek_precedence(&self) -> Precedence {
        let peek_token = match &self.peek_token {
            None => return Precedence::LOWEST,
            Some(pt) => pt,
        };
        match get_precedence(peek_token.token_type) {
            None => Precedence::LOWEST,
            Some(p) => p
        }
    }

    fn cur_precedence(&self) -> Precedence {
        let cur_token = match &self.cur_token {
            None => return Precedence::LOWEST,
            Some(ct) => ct,
        };
        match get_precedence(cur_token.token_type) {
            None => Precedence::LOWEST,
            Some(p) => p
        }
    }

    fn parse_identifier(&self) -> Option<Expression> {
        Some(Expression::Identifier(self.cur_token.as_ref()?.literal.clone()))
    }

    fn parse_integer_literal(&self) -> Option<Expression> {
        let lit = self.cur_token.as_ref().unwrap().literal.clone();
        let value: i64 = lit.parse().ok()?;
        Some(Expression::Integer(value))
    }

    fn parse_string_literal(&self) -> Option<Expression> {
        Some(Expression::String(self.cur_token.as_ref()?.literal.clone()))
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let op = self.cur_token.as_ref()?.literal.clone();
        self.next_token();
        let right = self.parse_expression(Precedence::PREFIX)?;
        Some(Expression::PrefixExpression {
            operation: op,
            right: Box::new(right)
        })
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let op = self.cur_token.as_ref()?.literal.clone();

        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;
        Some(Expression::InfixExpression {
            operation: op,
            left: Box::new(left),
            right: Box::new(right)
        })
    }

    fn parse_boolean(&self) -> Option<Expression> {
        if self.cur_token == None {
            return None;
        }
        Some(Expression::Boolean(self.cur_token_is(TokenType::TRUE)))
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();
        let exp = self.parse_expression(Precedence::LOWEST)?;
        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }
        Some(exp)
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        if !self.expect_peek(TokenType::LPAREN) {
            return None;
        }

        self.next_token();
        let condition = self.parse_expression(Precedence::LOWEST)?;
        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }
        if !self.expect_peek(TokenType::LBRACE) {
            return None;
        }

        let consequence = self.parse_block_statement()?;

        let stmt = if self.peek_token_is(TokenType::ELSE) {
            self.next_token();
            if !self.expect_peek(TokenType::LBRACE) {
                return None;
            }
            let alternative = self.parse_block_statement()?;
            Expression::IfExpression {
                condition: Box::new(condition),
                consequence: Box::new(consequence),
                alternative: Some(Box::new(alternative))
            }
        } else {
            Expression::IfExpression {
                condition: Box::new(condition),
                consequence: Box::new(consequence),
                alternative: None
            }
        };
        Some(stmt)
    }

    fn parse_block_statement(&mut self) -> Option<Statement> {
        let mut stmts = Vec::<Statement>::new();
        self.next_token();
        while !self.cur_token_is(TokenType::RBRACE) && !self.cur_token_is(TokenType::EOF) {
            let stmt = self.parse_statement();
            if let Some(st) = stmt {
                stmts.push(st);
            }
            self.next_token();
        }
        Some(Statement::BlockStatement(stmts))
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        if !self.expect_peek(TokenType::LPAREN) {
            return None;
        }
        let params = self.parse_function_parameters()?;

        if !self.expect_peek(TokenType::LBRACE) {
            return None;
        }

        let body = self.parse_block_statement()?;

        Some(Expression::Function {
            parameters: params,
            body: Box::new(body)
        })
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Expression>> {
        let mut params = Vec::new();
        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            return Some(params);
        }
        self.next_token();

        let ident = Expression::Identifier(self.cur_token.as_ref()?.literal.clone());
        params.push(ident);

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            let ident = Expression::Identifier(self.cur_token.as_ref()?.literal.clone());
            params.push(ident);
        }

        if !self.expect_peek(TokenType::RPAREN) {
            return None;
        }
        Some(params)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        let args = self.parse_expression_list(TokenType::RPAREN)?;
        Some(Expression::CallExpression { function: Box::new(function), arguments: args })
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Option<Vec<Expression>> {
        let mut list = Vec::new();
        if self.peek_token_is(end) {
            self.next_token();
            return Some(list)
        }
        self.next_token();
        list.push(self.parse_expression(Precedence::LOWEST)?);

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(Precedence::LOWEST)?);
        }
        if !self.expect_peek(end) {
            return None;
        }
        Some(list)
    }

    fn parse_array_literal(&mut self) -> Option<Expression> {
        let elements = self.parse_expression_list(TokenType::RBRACKET)?;
        Some(Expression::Array(elements))
    }

    fn parse_index_expression(&mut self, left: Expression) -> Option<Expression> {
        self.next_token();
        let index = self.parse_expression(Precedence::LOWEST)?;

        if !self.expect_peek(TokenType::RBRACKET) {
            return None;
        }
        
        Some(Expression::IndexExpression { left: Box::new(left), index: Box::new(index) })
    }

    fn parse_hash_literal(&mut self) -> Option<Expression> {
        let mut pairs = BTreeMap::<Box<Expression>, Box<Expression>>::new();

        while !self.peek_token_is(TokenType::RBRACE) {
            self.next_token();
            let key = self.parse_expression(Precedence::LOWEST)?;
            if !self.expect_peek(TokenType::COLON) {
                return None;
            }

            self.next_token();

            let value = self.parse_expression(Precedence::LOWEST)?;
            if !self.peek_token_is(TokenType::RBRACE) && !self.expect_peek(TokenType::COMMA) {
                return None;
            }

            pairs.insert(Box::new(key), Box::new(value));

        }

        if !self.expect_peek(TokenType::RBRACE) {
            return None;
        }
        Some(Expression::Hash(pairs))
    }
}


#[cfg(test)]
mod test {
    use std::collections::BTreeMap;
    use crate::ast::{Expression, Statement};
    use crate::ast::Expression::PrefixExpression;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_let_statement() {
        struct Test<'a> {
            input: &'a str,
            exp_identifier: Expression,
            exp_value: Expression
        }
        let tests = vec![
            Test {input: "let x = 5;", exp_identifier: Expression::Identifier("x".to_string()), exp_value: Expression::Integer(5)},
            Test {input: "let y = true;", exp_identifier: Expression::Identifier("y".to_string()), exp_value: Expression::Boolean(true)},
            Test {input: "let foobar = y;", exp_identifier: Expression::Identifier("foobar".to_string()), exp_value: Expression::Identifier("y".to_string())}
        ];

        for test in tests {
            let l = Lexer::new(test.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();

            assert_eq!(program.statements.len(), 1);

            let stmt = program.statements[0].clone();
            match stmt {
                Statement::LetStatement { identifier, value } => {
                    assert_eq!(identifier, test.exp_identifier);
                    assert_eq!(value, test.exp_value);
                },
                _ => {
                    panic!("not let statement")
                }
            }
        }
    }

    #[test]
    fn test_return_statement() {
        struct Test<'a> {
            input: &'a str,
            exp_value: Expression,
        }
        let tests = vec![
            Test {input: "return true;", exp_value: Expression::Boolean(true)},
            Test {input: "return 5;", exp_value: Expression::Integer(5)},
            Test {input: "return foobar;", exp_value: Expression::Identifier("foobar".to_string())},
        ];

        for test in tests {
            let l = Lexer::new(test.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();

            assert_eq!(program.statements.len(), 1);

            let stmt = program.statements[0].clone();
            match stmt {
                Statement::ReturnStatement(expression)=> {
                    assert_eq!(expression, test.exp_value);
                },
                _ => {
                    panic!("not return statement")
                }
            }
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        struct Test<'a> {
            input: &'a str,
            exp_operator: &'a str,
            exp_value: Expression,
        }
        let tests = vec![
            Test {input: "!5;", exp_operator: "!", exp_value: Expression::Integer(5)},
            Test {input: "-15;", exp_operator: "-", exp_value: Expression::Integer(15)},
            Test {input: "!true;", exp_operator: "!", exp_value: Expression::Boolean(true)},
        ];

        for test in tests {
            let l = Lexer::new(test.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();

            assert_eq!(program.statements.len(), 1);

            let stmt = program.statements[0].clone();
            match stmt {
                Statement::ExpressionStatement(expression)=> {
                    match expression {
                        Expression::PrefixExpression {operation, right} => {
                            assert_eq!(operation, test.exp_operator.to_string());
                            assert_eq!(right, Box::new(test.exp_value));
                        },
                        _ => {
                            panic!("not prefix expression")
                        }
                    }
                },
                _ => {
                    panic!("not expression statement")
                }
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        struct Test<'a> {
            input: &'a str,
            exp_left: Expression,
            exp_operator: &'a str,
            exp_right: Expression,
        }
        let tests = vec![
            Test {input: "5+3;", exp_left: Expression::Integer(5), exp_operator: "+", exp_right: Expression::Integer(3)},
            Test {input: "5-3;", exp_left: Expression::Integer(5), exp_operator: "-", exp_right: Expression::Integer(3)},
            Test {input: "5*3;", exp_left: Expression::Integer(5), exp_operator: "*", exp_right: Expression::Integer(3)},
            Test {input: "5/3;", exp_left: Expression::Integer(5), exp_operator: "/", exp_right: Expression::Integer(3)},
            Test {input: "5<3;", exp_left: Expression::Integer(5), exp_operator: "<", exp_right: Expression::Integer(3)},
            Test {input: "5>3;", exp_left: Expression::Integer(5), exp_operator: ">", exp_right: Expression::Integer(3)},
            Test {input: "5==3;", exp_left: Expression::Integer(5), exp_operator: "==", exp_right: Expression::Integer(3)},
            Test {input: "5!=3;", exp_left: Expression::Integer(5), exp_operator: "!=", exp_right: Expression::Integer(3)},
            Test {input: "foo!=bar", exp_left: Expression::Identifier("foo".to_string()), exp_operator: "!=", exp_right: Expression::Identifier("bar".to_string())},
            Test {input: "true==bar", exp_left: Expression::Boolean(true), exp_operator: "==", exp_right: Expression::Identifier("bar".to_string())},
        ];

        for test in tests {
            let l = Lexer::new(test.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();

            assert_eq!(program.statements.len(), 1);

            let stmt = program.statements[0].clone();
            match stmt {
                Statement::ExpressionStatement(expression)=> {
                    match expression {
                        Expression::InfixExpression {operation, left, right} => {
                            assert_eq!(operation, test.exp_operator.to_string());
                            assert_eq!(left, Box::new(test.exp_left));
                            assert_eq!(right, Box::new(test.exp_right));
                        },
                        _ => {
                            panic!("not prefix expression")
                        }
                    }
                },
                _ => {
                    panic!("not expression statement")
                }
            }
        }
    }

    #[test]
    fn test_operator_precedence() {
        struct Test<'a> {
            input: &'a str,
            expected: &'a str,
        }
        let tests = vec![
            Test {input: "-a*b", expected: "((-a) * b)"},
            Test {input: "!-a", expected: "(!(-a))"},
            Test {input: "a+b+c", expected: "((a + b) + c)"},

            Test {input: "a-b*c", expected: "(a - (b * c))"},
            Test {input: "a + b * c + d / e - f", expected: "(((a + (b * c)) + (d / e)) - f)"},
            Test {input: "0 + 4; -5 / 9", expected: "(0 + 4)((-5) / 9)"},
            Test {input: "5 > 4 == 3<4", expected: "((5 > 4) == (3 < 4))"},
            Test {input: "3 + 4 * 5 == 3 * 1 + 4 * 5", expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"},
            Test {input: "(5 + 5) * 2", expected: "((5 + 5) * 2)"},
            Test {input: "2 / (5 + 5)", expected: "(2 / (5 + 5))"},
            Test {input: "(5 + 5) * 2 * (5 + 5)", expected: "(((5 + 5) * 2) * (5 + 5))"},
            Test {input: "-(5 + 5)", expected: "(-(5 + 5))"},
            Test {input: "!(true == true)", expected: "(!(true == true))"},
            Test {input: "a + add(b * c) + d", expected: "((a + add((b * c))) + d)"},
            Test {input: "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", expected: "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"},
            Test {input: "add(a * b[2], b[1], 2 * [1, 2][1])", expected: "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))"},
        ];

        for test in tests {
            let l = Lexer::new(test.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();

            let got = format!("{}", program);
            assert_eq!(got, test.expected.to_string());

        }
    }

    #[test]
    fn test_if_literal() {
        struct Test<'a> {
            input: &'a str,
            expected: Expression,
        }
        let tests = vec![
            Test {
                input: "if (x <y) {x}",
                expected: Expression::IfExpression {
                    condition: Box::new(Expression::InfixExpression {
                        operation: "<".to_string(),
                        left: Box::new(Expression::Identifier("x".to_string())),
                        right: Box::new(Expression::Identifier("y".to_string()))
                    }),
                    consequence: Box::new(Statement::BlockStatement(vec![Statement::ExpressionStatement(Expression::Identifier("x".to_string()))])),
                    alternative: None
                }
            },
            Test {
                input: "if (x <y) {x} else {y}",
                expected: Expression::IfExpression {
                    condition: Box::new(Expression::InfixExpression {
                        operation: "<".to_string(),
                        left: Box::new(Expression::Identifier("x".to_string())),
                        right: Box::new(Expression::Identifier("y".to_string()))
                    }),
                    consequence: Box::new(Statement::BlockStatement(vec![Statement::ExpressionStatement(Expression::Identifier("x".to_string()))])),
                    alternative: Some(Box::new(Statement::BlockStatement(vec![Statement::ExpressionStatement(Expression::Identifier("y".to_string()))]))),
                }
            }
        ];

        for test in tests {
            let l = Lexer::new(test.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();

            assert_eq!(program.statements.len(), 1);

            let got = &program.statements[0];
            assert_eq!(got, &Statement::ExpressionStatement(test.expected));

        }
    }

    #[test]
    fn test_function_literal() {
        struct Test<'a> {
            input: &'a str,
            expected: Expression,
        }
        let tests = vec![
            Test {
                input: "fn(x, y) {x+y;}",
                expected: Expression::Function {
                    parameters: vec![Expression::Identifier("x".to_string()), Expression::Identifier("y".to_string())],
                    body: Box::new(Statement::BlockStatement(vec![Statement::ExpressionStatement(Expression::InfixExpression {
                        operation: "+".to_string(),
                        left: Box::new(Expression::Identifier("x".to_string())),
                        right: Box::new(Expression::Identifier("y".to_string())),
                    })]))
                }
            },
        ];

        for test in tests {
            let l = Lexer::new(test.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();

            assert_eq!(program.statements.len(), 1);

            let got = &program.statements[0];
            assert_eq!(got, &Statement::ExpressionStatement(test.expected));

        }
    }

    #[test]
    fn test_call_expression() {
        struct Test<'a> {
            input: &'a str,
            expected: Expression,
        }
        let tests = vec![
            Test {
                input: "add(1, 2*3, 4+a)",
                expected: Expression::CallExpression {
                    function: Box::new(Expression::Identifier("add".to_string())),
                    arguments: vec![Expression::Integer(1),
                    Expression::InfixExpression {
                        operation: "*".to_string(),
                        left: Box::new(Expression::Integer(2)),
                        right: Box::new(Expression::Integer(3))
                    },
                    Expression::InfixExpression {
                        operation: "+".to_string(),
                        left: Box::new(Expression::Integer(4)),
                        right: Box::new(Expression::Identifier("a".to_string()))
                    }
                    ]
                }
            },
        ];

        for test in tests {
            let l = Lexer::new(test.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();

            assert_eq!(program.statements.len(), 1);

            let got = &program.statements[0];
            assert_eq!(got, &Statement::ExpressionStatement(test.expected));

        }
    }

    #[test]
    fn test_string_literal() {
        let input = "\"hello world\"";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::ExpressionStatement(exp) => {
                match exp {
                    Expression::String(st) => assert_eq!(st, &"hello world".to_string()),
                    _ => panic!("expression is not string literal")
                }
            },
            _ => panic!("not expression statement")
        }
    }

    #[test]
    fn test_array_literal() {
        let input = "[1, a+2]";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::ExpressionStatement(exp) => {
                match exp {
                    Expression::Array(v) => assert_eq!(v, &vec![
                        Expression::Integer(1),
                        Expression::InfixExpression {
                            operation: "+".to_string(),
                            left: Box::new(Expression::Identifier("a".to_string())),
                            right: Box::new(Expression::Integer(2))
                        }
                    ]),
                    _ => panic!("expression is not array literal")
                }
            },
            _ => panic!("not expression statement")
        }
    }

    #[test]
    fn test_index_literal() {
        let input = "myArray[a+2]";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::ExpressionStatement(exp) => {
                match exp {
                    Expression::IndexExpression{left, index} =>{
                        assert_eq!(left, &Box::new(Expression::Identifier("myArray".to_string())));
                        assert_eq!(index,
                            &Box::new(Expression::InfixExpression {
                                operation: "+".to_string(),
                                left: Box::new(Expression::Identifier("a".to_string())),
                                right: Box::new(Expression::Integer(2))
                            })
                        );
                    },
                    _ => panic!("expression is not index literal")
                }
            },
            _ => panic!("not expression statement")
        }
    }

    //TODO: test hash
    #[test]
    fn test_hash_literal() {
        let input = r#"{"one": 1, "two": 2}"#;
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::ExpressionStatement(exp) => {
                match exp {
                    Expression::Hash(kv) =>{
                        let mut want = BTreeMap::new();
                        want.insert(Box::new(Expression::String("one".to_string())), Box::new(Expression::Integer(1)));
                        want.insert(Box::new(Expression::String("two".to_string())), Box::new(Expression::Integer(2)));
                        assert_eq!(kv, &want)
                    },
                    _ => panic!("expression is not hash literal")
                }
            },
            _ => panic!("not expression statement")
        }
    }

}

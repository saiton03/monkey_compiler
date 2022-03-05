use super::token::{TokenType, Token, look_up_ident};

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut l = Lexer {
            input: input.to_string(),
            position: 0,
            read_position: 0,
            ch: char::from(0),
        };
        l.read_char();
        l
    }

    pub fn next_token(&mut self) -> Token {
        //let mut tok: Token;

        self.skip_white_space();

        let tok = match self.ch {
            '=' => if self.peek_char() == '=' {
                    let ch = self.ch;
                    self.read_char();
                    let literal = ch.to_string() + &*self.ch.to_string();
                    Token {
                        token_type: TokenType::EQ,
                        literal
                    }
                } else {
                    new_token(TokenType::ASSIGN, self.ch)
                },
            '+' => new_token(TokenType::PLUS, self.ch),
            '-' => new_token(TokenType::MINUS, self.ch),
            '*' => new_token(TokenType::ASTERISK, self.ch),
            '/' => new_token(TokenType::SLASH, self.ch),
            '!' => if self.peek_char() == '=' {
                let ch = self.ch;
                self.read_char();
                let literal = ch.to_string() + &*self.ch.to_string();
                Token {
                    token_type: TokenType::NotEq,
                    literal
                }
            } else {
                new_token(TokenType::BANG, self.ch)
            },
            '<' => new_token(TokenType::LT, self.ch),
            '>' => new_token(TokenType::GT, self.ch),
            ';' => new_token(TokenType::SEMICOLON, self.ch),
            ':' => new_token(TokenType::COLON, self.ch),
            ',' => new_token(TokenType::COMMA, self.ch),
            '{' => new_token(TokenType::LBRACE, self.ch),
            '}' => new_token(TokenType::RBRACE, self.ch),
            '(' => new_token(TokenType::LPAREN, self.ch),
            ')' => new_token(TokenType::RPAREN, self.ch),
            '[' => new_token(TokenType::LBRACKET, self.ch),
            ']' => new_token(TokenType::RBRACKET, self.ch),
            '"' => Token{
                token_type: TokenType::STRING,
                literal: self.read_string(),
            },
            '\0' => new_token(TokenType::EOF, self.ch),
            _ => if is_letter(self.ch){
                let literal = self.read_identifier();
                return Token {
                    token_type: look_up_ident(&literal),
                    literal,
                }
            } else if is_digit(self.ch){
                let literal = self.read_number();
                return Token {
                    token_type: TokenType::INT,
                    literal,
                }
            } else {
                new_token(TokenType::ILLEGAL, self.ch)
            },
        };
        self.read_char();

        tok
    }

    fn skip_white_space(&mut self)  {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            self.read_char();
        }
    }

    fn read_char(&mut self) {
        self.ch = if self.read_position >= self.input.len() {
            char::from(0)
        } else {
            self.input.chars().nth(self.read_position).unwrap()
        };
        self.position = self.read_position;
        self.read_position+=1;
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            char::from(0)
        } else {
            self.input.chars().nth(self.read_position).unwrap()
        }
    }

    fn read_identifier(&mut self) -> String {
        let position= self.position ;
        while is_letter(self.ch) || is_digit(self.ch){ //先頭以外は数字も許容
            self.read_char();
        }
        let ident = &self.input[position..self.position];
        ident.to_string()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while is_digit(self.ch) {
            self.read_char();
        }
        let number = &self.input[position..self.position];
        number.to_string()
    }

    fn read_string(&mut self) -> String {
        let position = self.position + 1;
        loop {
            self.read_char();
            if self.ch == '"' || self.ch == char::from(0) {
                break;
            }
        }
        let literal = &self.input[position..self.position];
        literal.to_string()
    }

}

fn is_letter(ch: char) -> bool {
    ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z'
}

fn is_digit(ch: char) -> bool {
    ch >= '0' && ch <= '9'
}

fn new_token(token_type: TokenType, ch: char) -> Token{
    Token {
        token_type,
        literal: ch.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token::TokenType;

    #[test]
    fn test_lexer() {
        let input = r#"let a1 = 2+3;
let add = fn(x, y) {
    x + y;
}

let result = add(five, ten);
5 < 10 > 3;
if(5<10) {
    return true
} else {
    return false
}
10 == 10
10 != 9
"foo bar"
[1, 2];"#;

        struct ExpectToken {
            exp_token_type: TokenType,
            exp_literal: String,
        }

        let expected_tokens = vec![
            ExpectToken {
                exp_token_type: TokenType::LET,
                exp_literal: "let".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::IDENT,
                exp_literal: "a1".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::ASSIGN,
                exp_literal: "=".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::INT,
                exp_literal: "2".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::PLUS,
                exp_literal: "+".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::INT,
                exp_literal: "3".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::SEMICOLON,
                exp_literal: ";".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::LET,
                exp_literal: "let".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::IDENT,
                exp_literal: "add".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::ASSIGN,
                exp_literal: "=".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::FUNCTION,
                exp_literal: "fn".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::LPAREN,
                exp_literal: "(".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::IDENT,
                exp_literal: "x".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::COMMA,
                exp_literal: ",".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::IDENT,
                exp_literal: "y".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::RPAREN,
                exp_literal: ")".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::LBRACE,
                exp_literal: "{".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::IDENT,
                exp_literal: "x".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::PLUS,
                exp_literal: "+".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::IDENT,
                exp_literal: "y".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::SEMICOLON,
                exp_literal: ";".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::RBRACE,
                exp_literal: "}".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::LET,
                exp_literal: "let".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::IDENT,
                exp_literal: "result".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::ASSIGN,
                exp_literal: "=".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::IDENT,
                exp_literal: "add".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::LPAREN,
                exp_literal: "(".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::IDENT,
                exp_literal: "five".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::COMMA,
                exp_literal: ",".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::IDENT,
                exp_literal: "ten".to_string()
            },
            ExpectToken {
                exp_token_type: TokenType::RPAREN,
                exp_literal: ")".to_string()
            },

            // TODO: rest
        ];

        let mut l = Lexer::new(input);

        for (i, token) in expected_tokens.iter().enumerate() {
            let tok = l.next_token();
            println!("test {}: token_type {:?}, literal {:?}", i, token.exp_token_type, token.exp_literal);
            println!("got: token_type {:?}, literal {:?}", tok.token_type, tok.literal);
            assert_eq!(tok.token_type, token.exp_token_type);
            assert_eq!(tok.literal, token.exp_literal);
        }
    }
}
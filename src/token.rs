use std::fmt;
use std::fmt::{Formatter};

#[derive(Debug,Ord, PartialOrd, Eq, PartialEq, Clone, Copy)]
pub enum TokenType {
    ILLEGAL,
    EOF,

    IDENT,
    INT,
    STRING,

    ASSIGN,
    PLUS,
    MINUS,
    BANG, // !
    ASTERISK,
    SLASH,

    LT,
    GT,
    EQ,
    NotEq,

    COMMA,
    SEMICOLON,
    COLON,

    LPAREN, // (
    RPAREN, // )
    LBRACE, // {
    RBRACE, // }
    LBRACKET, // [
    RBRACKET, // ]

    // keywords
    FUNCTION, // fn
    LET, // let
    TRUE, // true
    FALSE, // false
    IF, // if
    ELSE, // else
    RETURN, // return
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}",
            match &self {
                TokenType::ILLEGAL => "ILLEGAL",
                TokenType::EOF => "EOF",
                TokenType::IDENT => "IDENT",
                TokenType::INT => "INT",
                TokenType::STRING => "STRING",
                TokenType::ASSIGN => "ASSIGN",
                TokenType::PLUS => "PLUS",
                TokenType::MINUS => "MINUS",
                TokenType::BANG => "BANG",
                TokenType::ASTERISK => "ASTERISK",
                TokenType::SLASH => "SLASH",
                TokenType::LT => "LT",
                TokenType::GT => "GT",
                TokenType::EQ => "EQ",
                TokenType::NotEq => "NotEq",
                TokenType::COMMA => "COMMA",
                TokenType::SEMICOLON => "SEMICOLON",
                TokenType::COLON => "COLON",
                TokenType::LPAREN => "LPAREN",
                TokenType::RPAREN => "RPAREN",
                TokenType::LBRACE => "LBRACE",
                TokenType::RBRACE => "RBRACE",
                TokenType::LBRACKET => "LBRACKET",
                TokenType::RBRACKET => "RBRACKET",
                TokenType::FUNCTION => "FUNCTION",
                TokenType::LET => "LET",
                TokenType::TRUE => "TRUE",
                TokenType::FALSE => "FALSE",
                TokenType::IF => "IF",
                TokenType::ELSE => "ELSE",
                TokenType::RETURN => "RETURN",
            }
        )
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{{{}: {}}}", self.token_type, self.literal)
    }
}

pub fn look_up_ident(ident: &str) -> TokenType {
    match ident {
        "fn" => TokenType::FUNCTION,
        "let" => TokenType::LET,
        "true" => TokenType::TRUE,
        "false" => TokenType::FALSE,
        "if" => TokenType::IF,
        "else" => TokenType::ELSE,
        "return" => TokenType::RETURN,
        _ => TokenType::IDENT,
    }
}

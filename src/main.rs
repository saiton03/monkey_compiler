extern crate core;

use std::io::{stdin, stdout};

mod token;
mod lexer;
mod ast;
mod parser;
mod object;
mod evaluator;
mod builtin;
mod environment;
mod code;
mod compiler;
mod vm;
mod repl;

fn main() -> std::io::Result<()> {
    let reader = stdin();
    let writer = stdout();
    repl::start(reader, writer)
}

extern crate core;

use std::io::{stdin, stdout};

mod token;
mod lexer;
mod ast;
mod parser;
mod object;
mod evaluator;
mod builtin;
mod repl;

fn main() {
    let reader = stdin();
    let writer = stdout();
    repl::start(reader, writer);
}

extern crate core;
extern crate monkey_compiler;

use std::io::{stdin, stdout};
use monkey_compiler::interpreter;


fn main() -> std::io::Result<()> {
    let reader = stdin();
    let writer = stdout();
    interpreter::start(reader, writer)
}



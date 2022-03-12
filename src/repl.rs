use std::io::{BufRead, BufReader, Read, Write};
use std::string::String;
use crate::ast::Node;
use crate::compiler::Compiler;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::vm::VM;

const PROMPT: &str = ">> ";

pub fn start<R: Read, W: Write>(reader: R, mut writer: W) -> std::io::Result<()> {
    let mut reader = BufReader::new(reader);
    loop {
        write!(writer, "{}", PROMPT)?;
        writer.flush().expect("flush parser error failed");
        let mut line = String::new();
        reader.read_line(&mut line)?;

        let l = Lexer::new(&line);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        if p.errors().len() > 0 {
            print_parser_error(&mut writer, p.errors())?;
            continue;
        }

        // compile part
        let mut comp = Compiler::new();
        if let Err(msg) = comp.compile(Node::Program(program)) {
            writer.write_all(format!("compile error occurred: {}\n", msg).as_ref())?;
            continue;
        }

        let mut machine = VM::new(comp.byte_code());

        let out = match machine.run() {
            Err(err) => format!("runtime error occurred: {}", err),
            Ok(_) => {
                format!("{}", machine.last_popped_stack_elem())
            },
        };
        writer.write_all(out.as_ref())?;
        writer.write_all("\n".as_ref())?;
    }

}

fn print_parser_error<W: Write>(mut writer: W, errs: Vec<String>) -> std::io::Result<()>{
    write!(writer, "Woops! We ran into some monkey business here!\n parser errors:\n")?;
    for msg in errs {
        write!(writer, "    {}\n", msg)?;
    }
    writer.flush()
}

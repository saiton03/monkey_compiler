use std::io::{BufRead, BufReader, Read, Write};
use crate::evaluator::Evaluator;
use std::string::String;
use crate::ast::Node;
use crate::lexer::Lexer;
use crate::parser::Parser;

const PROMPT: &str = ">> ";

const MONKEY_FACE: &str=
r#"
            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
"#;

pub fn start<R: Read, W: Write>(reader: R, mut writer: W) -> std::io::Result<()> {
    let mut env = Evaluator::new();
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

        let evaluated = env.eval(Node::Program(program));
        let out = match evaluated {
            None => "returned object is None".to_string(),
            Some(obj) => format!("{}\n", obj),
        };
        writer.write_all(out.as_ref())?;
    }

}

fn print_parser_error<W: Write>(mut writer: W, errs: Vec<String>) -> std::io::Result<()>{
    write!(writer, "{}", MONKEY_FACE)?;
    write!(writer, "Woops! We ran into some monkey business here!\n parser errors:\n")?;
    for msg in errs {
        write!(writer, "    {}\n", msg)?;
    }
    writer.flush()
}

use scheme_ish::{lexer::LexError, lexer::Lexer, parser::ParseError, parser::Parser};
use std::io;
use std::io::Write;
use thiserror::Error;

fn print_help_message() {
    println!("Help:");
    println!(":h\thelp");
    println!(":q\tquit");
}

fn evaluate(buffer: &str) -> Result<(), ReplError> {
    let mut tokenizer = Lexer::new();
    let mut parser = Parser::new();

    tokenizer.tokenize(buffer)?;
    parser.parse(&mut tokenizer.tokens.into_iter())?;

    println!("{:#?}", parser.ast);
    Ok(())
}

fn repl() -> Result<bool, ReplError> {
    print!("> ");
    io::stdout().flush()?;

    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer)?;

    let buffer = buffer.trim();
    if buffer == ":q" {
        Ok(false)
    } else if buffer == ":h" {
        print_help_message();
        Ok(true)
    } else {
        evaluate(buffer)?;
        Ok(true)
    }
}

fn main() {
    println!("Welcome to scheme-ish!");
    println!("Type :h for help");
    loop {
        match repl() {
            Ok(true) => {}
            Ok(false) => {
                println!("Goodbye!");
                break;
            }
            Err(e) => {
                println!("{}: {:?}", e, e);
            }
        }
    }
}

#[derive(Error, Debug)]
pub enum ReplError {
    #[error("Lexer error")]
    Lex(#[from] LexError),

    #[error("Parser error")]
    Parse(#[from] ParseError),

    #[error("IO error")]
    IO(#[from] io::Error),
}

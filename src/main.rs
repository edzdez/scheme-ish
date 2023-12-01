use scheme_ish::evaluator::{EvalError, Evaluator};
use scheme_ish::lexer::{LexError, Lexer};
use scheme_ish::parser::{ParseError, Parser};

use std::io::{self, Write};
use thiserror::Error;

fn print_help_message() {
    println!("Help:");
    println!(":h\thelp");
    println!(":q\tquit");
}

fn evaluate(buffer: &str, evaluator: &mut Evaluator) -> Result<(), ReplError> {
    let mut tokenizer = Lexer::new();
    let mut parser = Parser::new();

    tokenizer.tokenize(buffer)?;
    parser.parse(&mut tokenizer.tokens.into_iter())?;

    for expr in parser.ast {
        let out = evaluator.evaluate(expr)?;
        println!("{}", out);
    }

    Ok(())
}

fn repl(evaluator: &mut Evaluator) -> Result<bool, ReplError> {
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
        evaluate(buffer, evaluator)?;
        Ok(true)
    }
}

fn main() {
    let mut evaluator = Evaluator::new();

    println!("Welcome to scheme-ish!");
    println!("Type :h for help");
    println!("Don't bother trying to use arrow keys---MIT Scheme doesn't let you use them in the repl either");
    loop {
        match repl(&mut evaluator) {
            Ok(true) => {}
            Ok(false) => {
                println!("Goodbye!");
                break;
            }
            Err(e) => {
                println!("Error: {}", e);
            }
        }
    }
}

#[derive(Error, Debug)]
pub enum ReplError {
    #[error("{0}")]
    Lex(#[from] LexError),

    #[error("{0}")]
    Parse(#[from] ParseError),

    #[error("{0}")]
    Eval(#[from] EvalError),

    #[error("{0}")]
    IO(#[from] io::Error),
}

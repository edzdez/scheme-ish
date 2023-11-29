use scheme_ish::{lexer::LexError, lexer::Lexer, parser::ParseError, parser::Parser};
use std::io;
use thiserror::Error;

fn repl() -> Result<(), ReplError> {
    let mut tokenizer = Lexer::new();
    let mut parser = Parser::new();

    print!("> ");
    io::stdout().flush()?;

    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer)?;
    tokenizer.tokenize(&buffer)?;
    parser.parse(&mut tokenizer.tokens.into_iter())?;

    println!("{:#?}", parser.ast);

    Ok(())
}

fn main() {
    println!("Welcome to scheme-ish!");
    loop {
        if let Err(e) = repl() {
            println!("{}: {:?}", e, e);
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

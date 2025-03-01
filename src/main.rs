
#![allow(clippy::needless_return)]

use std::error::Error;

mod common;
mod lex;

use lex::lexer::Lexer;


fn main() -> Result<(), Box<dyn Error>> {

    let input = std::fs::read_to_string("samples/min.rqi")?;

    let lexemes = Lexer::new().tokenize(input)?;

    for lxm in lexemes {
        println!("{lxm:?}");
    }

    return Ok(());
}

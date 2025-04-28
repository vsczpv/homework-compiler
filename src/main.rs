#![allow(clippy::needless_return)]

use std::error::Error;

mod common;
mod lex;
mod syn;

use lex::lexer::Lexer;
use syn::syntax::SyntaxParser;

fn main() -> Result<(), Box<dyn Error>> {
    let input = std::fs::read_to_string("samples/min.rqi")?;

    let lexemes = Lexer::new()
        .tokenize(input)?
        .into_iter()
        .filter(|v| !v.get_token_ref().is_comment())
        .collect();

    let _syn = SyntaxParser::new(lexemes).parse()?;

    return Ok(());
}

#![allow(clippy::needless_return)]

use std::error::Error;

mod common;
mod lex;
mod sem;
mod syn;

use lex::lexer::Lexer;
use sem::irgen::{IrGen, SymbolDefinedState, SymbolTable};
use syn::syntax::SyntaxParser;

fn main() -> Result<(), Box<dyn Error>> {
    let input = std::fs::read_to_string("samples/other.l")?;

    let lexemes = Lexer::new()
        .tokenize(input)?
        .into_iter()
        .filter(|v| !v.get_token_ref().is_comment())
        .collect();

    let syn = SyntaxParser::new(lexemes)
        .parse()?
        .try_apply_many(&syn::preprocess::PREPROCESSES)?
        .make_root();

    let mut symbols = SymbolTable::new();

    IrGen::new(&mut symbols).generate(&syn)?;
    symbols.print();

    for s in symbols.all_syms() {
        if matches!(s.defined, SymbolDefinedState::Undefined) {
            eprintln!("warning: symbol '{}' never defined.", s.ident);
        }

        if s.used == false {
            eprintln!("warning: symbol '{}' never used.", s.ident);
        }
    }

    return Ok(());
}

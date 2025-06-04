#![allow(clippy::needless_return)]

use std::error::Error;

mod common;
mod lex;
mod sem;
mod syn;

use lex::lexer::Lexer;
use sem::{
    asmspit::AssemblySpitter,
    symtab::{SemanticError, SymbolTable, SymtabGenerator},
    typechk::TypeChecker,
};
use syn::syntax::SyntaxParser;

fn main() -> Result<(), Box<dyn Error>> {
    let input = std::fs::read_to_string("samples/target.l")?;

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
    let syn = SymtabGenerator::new(&mut symbols).generate(syn)?;

    let typetree = {
        let mut tyck = TypeChecker::new(&symbols);
        let res = tyck.typecheck(syn)?;
        let res = tyck.bindcheck(res)?;
        let res = tyck.assure_ints(res)?;
        res
    };

    /* NOTE: Currently, having anything anywhere other than the globalscope is undefined behaviour. */
    let global_only = symbols
        .get_all_syms()
        .iter()
        .fold(true, |left, right| left && right.scope == 0);

    if global_only == false {
        Err(SemanticError(format!(
            "error: compiler only support globals."
        )))?;
    }

    let program = AssemblySpitter::new(&symbols, &typetree)
        .spit_defines()
        .spit_globals()
        .spit_global_routine()
        .get_program();

    println!("{program}");
    typetree.print_tree(1);

    return Ok(());
}

#![allow(clippy::needless_return)]

use clap::Parser;
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

#[derive(clap::ValueEnum, Clone, Debug)]
enum EmissionMode {
    Lex,
    RawSyntax,
    Syntax,
    TypedSyntax,
    Symtab,
    Asm,
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long, help = "Compilation mode.")]
    mode: EmissionMode,
    #[arg(required(true), help = "Path of systeml program to compile.")]
    file: String,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    let input = std::fs::read_to_string(args.file)?;

    let lexemes = Lexer::new()
        .tokenize(input)?
        .into_iter()
        .filter(|v| !v.get_token_ref().is_comment())
        .collect();

    if matches!(args.mode, EmissionMode::Lex) {
        for l in lexemes {
            println!("{l:?}");
        }
        return Ok(());
    }

    let syn = SyntaxParser::new(lexemes).parse()?;

    if matches!(args.mode, EmissionMode::RawSyntax) {
        syn.print_tree(0);
        return Ok(());
    }

    let syn = syn
        .try_apply_many(&syn::preprocess::PREPROCESSES)?
        .make_root();

    let mut symbols = SymbolTable::new();
    let syn = SymtabGenerator::new(&mut symbols).generate(syn)?;

    if matches!(args.mode, EmissionMode::Syntax) {
        syn.print_tree(0);
        return Ok(());
    }

    let typetree = {
        let mut tyck = TypeChecker::new(&symbols);
        let res = tyck.typecheck(syn)?;
        let res = tyck.bindcheck(res)?;
        let res = tyck.assure_ints(res)?;
        res
    };

    if matches!(args.mode, EmissionMode::TypedSyntax) {
        typetree.print_tree(0);
        return Ok(());
    }

    if matches!(args.mode, EmissionMode::Symtab) {
        symbols.print();
        return Ok(());
    }

    assert!(matches!(args.mode, EmissionMode::Asm));

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

    return Ok(());
}

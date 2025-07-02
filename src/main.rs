#![allow(clippy::needless_return)]

use clap::Parser;
use std::{error::Error, io::Read};

mod common;
mod lex;
mod sem;
mod syn;

use lex::lexer::Lexer;
use sem::{
    asmspit::{AssemblySpitter, Compiler},
    symtab::{SemanticError, Symbol, SymbolTable, SymtabGenerator},
    typechk::{BuiltinTypes, SymbolMajorType, TypeChecker, ValueKind},
};
use syn::{
    syntax::SyntaxParser,
    tree::{AstNode, Virtual},
};

#[derive(clap::ValueEnum, Clone, Debug)]
enum EmissionMode {
    Lex,
    RawSyntax,
    Syntax,
    TypedSyntax,
    Symtab,
    ScopeTree,
    Asm,
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long, help = "Compilation mode.")]
    emit: EmissionMode,
    #[arg(required(true), help = "Path of systeml program to compile.")]
    file: String,
}

fn update_lambda_argsym(syms: &mut SymbolTable, tree: &Box<AstNode>) {
    if let Some(Virtual::LetBinding {
        ident: _,
        defined: _,
        typed: _,
        generation,
    }) = tree.get_kind().to_owned().some_virt()
    {
        if syms
            .get_by_gen(generation.unwrap())
            .unwrap()
            .stype
            .is_lambda()
        {
            let ValueKind::Rvalue(SymbolMajorType::Lambda { args, ret, argsym }) = tree
                .follow_line(3)
                .get_kind()
                .to_owned()
                .some_typed()
                .unwrap()
                .1
            else {
                panic!();
            };
            let old = (*syms.get_by_gen(generation.unwrap()).unwrap()).clone();
            let new = Symbol {
                stype: SymbolMajorType::Lambda { args, ret, argsym },
                ..old
            };
            syms.update_by_gen(generation.unwrap(), new);
        }
    } else {
        for k in tree.get_children() {
            update_lambda_argsym(syms, k);
        }
    };
}

fn find_lambda_scopes<'a>(
    tree: &'a Box<AstNode>,
    vec: &mut Vec<&'a Box<AstNode>>,
    arg: &mut Vec<&'a Box<AstNode>>,
) {
    if tree
        .get_kind()
        .to_owned()
        .some_typed()
        .is_some_and(|v| matches!(v.0, Virtual::LambdaRoot))
    {
        let scope = tree.follow_line2(1, 2);
        let args = tree.follow_line2(1, 0);
        vec.push(scope);
        arg.push(args);
        return;
    } else {
        for k in tree.get_children() {
            find_lambda_scopes(k, vec, arg);
        }
    }
}

fn assure_no_application(tree: &Box<AstNode>) -> bool {
    let mut res = false;
    for k in tree.get_children() {
        if tree
            .get_kind()
            .to_owned()
            .some_typed()
            .is_some_and(|t| matches!(t.0, Virtual::Application))
        {
            return true;
        }
        res |= assure_no_application(k);
    }
    return res;
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    let input = if args.file.eq("-") {
        let mut buf = String::new();
        std::io::stdin().read_to_string(&mut buf)?;
        buf
    } else {
        std::fs::read_to_string(args.file)?
    };

    let lexemes = Lexer::new()
        .tokenize(input)?
        .into_iter()
        .filter(|v| !v.get_token_ref().is_comment())
        .collect();

    if matches!(args.emit, EmissionMode::Lex) {
        for l in lexemes {
            println!("{l:?}");
        }
        return Ok(());
    }

    let syn = SyntaxParser::new(lexemes).parse()?;

    if matches!(args.emit, EmissionMode::RawSyntax) {
        syn.print_tree(0);
        return Ok(());
    }

    let syn = syn
        .try_apply_many(&syn::preprocess::PREPROCESSES)?
        .make_root();

    let mut symbols = SymbolTable::new();
    let syn = SymtabGenerator::new(&mut symbols, 0).generate(syn)?;

    if matches!(args.emit, EmissionMode::Syntax) {
        syn.print_tree(0);
        return Ok(());
    }

    update_lambda_argsym(&mut symbols, &syn);

    let typetree = {
        let mut tyck = TypeChecker::new(&symbols);
        let res = tyck.typecheck(syn)?;
        let res = tyck.bindcheck(res)?;
        let res = tyck.assure_ints(res)?;
        res
    };

    let mut function_count = 0;
    /* NOTE: Making the program flat and non-recursive deeply simplifies codegen. */
    for s in symbols.get_all_syms() {
        if s.scope == 0 {
            if s.stype.is_lambda() == false {
                Err(format!("Only functions may be declared on root-scope."))?
            }
            function_count += 1;
        } else {
            if s.stype.is_lambda() {
                Err(format!("Cannot declare nested functions."))?
            }
        }
    }

    let mut index_to_ignore = None;
    for (i, s) in symbols.get_all_syms().iter().enumerate() {
        if s.scope == 0 && s.ident.eq("main") {
            index_to_ignore = Some(i);
        }
    }

    let Some(index_to_ignore) = index_to_ignore else {
        return Err(format!("No main function found."))?;
    };

    let SymbolMajorType::Lambda {
        args: main_args,
        ret: main_ret,
        argsym: _,
    } = symbols
        .get_all_syms()
        .get(index_to_ignore)
        .unwrap()
        .stype
        .clone()
    else {
        return Err(format!("Main must be function of unit -> unit."))?;
    };

    if main_args.len() != 1
        || !matches!(main_args[0], SymbolMajorType::Builtin(BuiltinTypes::Unit))
        || !matches!(
            *main_ret,
            SymbolMajorType::Builtin(sem::typechk::BuiltinTypes::Unit)
        )
    {
        return Err(format!("Main must be function of unit -> unit."))?;
    }

    /* NOTE: This only works due to the flat structure we're using. */
    let mut function_scopes: Vec<&Box<AstNode>> = Vec::new();
    let mut function_argmts: Vec<&Box<AstNode>> = Vec::new();
    find_lambda_scopes(&typetree, &mut function_scopes, &mut function_argmts);

    if function_count != function_scopes.len() {
        return Err(format!("All functions must be defined"))?;
    }

    let mut functions: Vec<(String, &Box<AstNode>, usize)> = Vec::new();

    let mut callcheck_index = 0;
    for (i, s) in symbols.get_all_syms().iter().enumerate() {
        if s.stype.is_lambda() {
            functions.push((
                s.ident.clone(),
                function_scopes[callcheck_index],
                s.generation,
            ));
            if i != index_to_ignore {
                if assure_no_application(function_scopes[callcheck_index]) {
                    Err(format!("Only main can call functions."))?
                }
            }
            callcheck_index += 1;
        }
    }

    if matches!(args.emit, EmissionMode::TypedSyntax) {
        typetree.print_tree(0);
        return Ok(());
    }

    if matches!(args.emit, EmissionMode::Symtab) {
        symbols.print();
        return Ok(());
    }

    assert!(matches!(args.emit, EmissionMode::Asm));

    let mut compiler = Compiler::new(&symbols, functions);

    compiler.compile();
    println!("{}\n", compiler.get_program());

    return Ok(());
}

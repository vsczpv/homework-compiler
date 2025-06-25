#![allow(clippy::needless_return)]

use std::error::Error;
use std::env;

mod common;
mod lex;
mod sem;
mod syn;

mod ide_module;


use lex::lexer::Lexer;
use sem::{
    asmspit::AssemblySpitter,
    symtab::{SemanticError, SymbolTable, SymtabGenerator},
    typechk::TypeChecker,
};
use syn::syntax::SyntaxParser;

use ide_module::output_writer::Writer;
fn main() -> Result<(), Box<dyn Error>> {

     /* Coleta código passado como parâmetro */
    let args: Vec<String> = env::args().collect();
    let mut code_input = String::new();
    if !args.get(1).is_none() { 
        for arg in args.iter().skip(1) {
            code_input.push_str(arg);
        }
     } else { 
        code_input = std::fs::read_to_string("samples/min.l")?
    };
    //println!("Input: {} \n ", code_input);


    /* Análise Léxica 
    
    Cria lexer
    Transforma o código fonte em um vetor de lexemas
    Transforma vetor em iterador
    remove comentários
    Transforma iterador em vetor

    resultado: lexemes = vetor de lexemas
    */
    let lexemes = Lexer::new()
        .tokenize(code_input)?
        .into_iter()
        .filter(|v| !v.get_token_ref().is_comment())
        .collect();


    /* Análise Sintática
    Cria parser para o vetor de lexemas
    Gera árvore AST de acordo com lexemas
    Organiza a árvore
    Adiciona um nó raíz
    
    */
    let syn = SyntaxParser::new(lexemes)
        .parse()?
        .try_apply_many(&syn::preprocess::PREPROCESSES)?
        .make_root();
    
        println!("\n\nsyn--------------------------------------------------\n\n");
        syn.print_tree(0); // printa a arvore no terminal
    
    
    /* Análise Semântica
    Syntab:
    Cria tabela de símbolos vadia
    Cria gerador de tabela de símbolos (controlador)
    Recria a AST, passando a analize semântica (.generate())

    typechk
    Cria 

    */
    let mut symbols = SymbolTable::new();
    let syn = SymtabGenerator::new(&mut symbols).generate(syn)?;

    println!("\n\nSymtabGenerator--------------------------------------------------\n\n");
    syn.print_tree(0); // printa a arvore no terminal

    let typetree = {
        let mut tyck = TypeChecker::new(&symbols);
        let res = tyck.typecheck(syn)?;
        let res = tyck.bindcheck(res)?;
        let res = tyck.assure_ints(res)?;
        res
    };
    
    println!("\n\ntypetree--------------------------------------------------\n\n");
    typetree.print_tree(0); // printa a arvore no terminal
    let writer = Writer::new();
    writer.write("typed_tree.txt", &typetree.tree_as_string())?;




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

    println!("\n\nsymble table--------------------------------------------------\n\n");
    symbols.print();


    /* Geração de código
    
    
    */

    let program = AssemblySpitter::new(&symbols, &typetree)
        .spit_defines()
        .spit_globals()
        .spit_global_routine()
        .get_program();

    println!("\n\nassembly program--------------------------------------------------\n\n");
    println!("{program}");

    return Ok(());
}

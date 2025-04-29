#![allow(clippy::needless_return)]

use std::error::Error;

mod common;
mod lex;
mod syn;

use lex::{lexer::Lexer, tokens::Token};
use syn::{
    syntax::SyntaxParser,
    tree::{AstNode, NodeKind, Virtual},
};

fn genericize(mut node: Box<AstNode>) -> Box<AstNode> {
    if !node.is_nonterminal_expression() && node.get_children().len() == 0 {
        return node;
    }

    if node.is_nonterminal_expression() {
        node.morph(NodeKind::Virt(Virtual::GenericExpression));
    }

    let mut newnode = AstNode::new(node.get_kind().clone());
    let children = node.unpeel_children();

    for c in children {
        newnode.add_child(genericize(c));
    }

    return newnode;
}

fn unparenthisize(mut node: Box<AstNode>) -> Box<AstNode> {
    if node.is_expression() && node.get_children().len() == 3 {
        #[rustfmt::skip]
        let cond: bool =
            match node.get_children()[0].get_kind() {
                NodeKind::Lex(lex) => lex.get_token() == Token::OpenPar,
                _ => false,
            } &
            node.get_children()[1].is_expression() &
            match node.get_children()[2].get_kind() {
                NodeKind::Lex(lex) => lex.get_token() == Token::ClosePar,
                _ => false,
            };

        if cond {
            let child = node.unpeel_children().into_iter().nth(1).unwrap();
            node = AstNode::new(NodeKind::Virt(Virtual::GenericExpression));
            node.add_child(child);
        }
    }

    let mut newnode = AstNode::new(node.get_kind().clone());
    let children = node.unpeel_children();

    for c in children {
        newnode.add_child(unparenthisize(c));
    }

    return newnode;
}

fn flatten_expressions(mut node: Box<AstNode>) -> Box<AstNode> {
    if !node.is_expression() && node.get_children().len() == 0 {
        return node;
    }

    if node.is_expression()
        && node.get_children().len() == 1
        && node.get_children()[0].is_expression()
    {
        let child = node.unpeel_children().into_iter().nth(0).unwrap();
        node = flatten_expressions(child);
    }

    let mut newnode = AstNode::new(node.get_kind().clone());
    let children = node.unpeel_children();

    for c in children {
        newnode.add_child(flatten_expressions(c));
    }

    return newnode;
}

fn main() -> Result<(), Box<dyn Error>> {
    let input = std::fs::read_to_string("samples/min.rqi")?;

    let lexemes = Lexer::new()
        .tokenize(input)?
        .into_iter()
        .filter(|v| !v.get_token_ref().is_comment())
        .collect();

    let syn = SyntaxParser::new(lexemes).parse()?;

    let syn = genericize(syn);
    let syn = unparenthisize(syn);
    let syn = flatten_expressions(syn);
    syn.print_tree(0);

    return Ok(());
}

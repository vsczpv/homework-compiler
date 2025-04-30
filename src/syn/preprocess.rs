use std::{error::Error, fmt::Display};

use crate::lex::{lexer::Lexeme, tokens::Token};

use super::tree::{AstNode, NodeKind, Virtual};

#[derive(Debug, Clone)]
pub struct AstPreprocessingError(String, Lexeme);

impl Display for AstPreprocessingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "syntax error on {:?}: {} ", self.1.get_token(), self.0)
    }
}

impl Error for AstPreprocessingError {}

pub type PreprocessClosure = fn(Box<AstNode>) -> Box<AstNode>;
pub type TryPreprocessClosure = fn(Box<AstNode>) -> Result<Box<AstNode>, AstPreprocessingError>;

pub enum PreprocessKind {
    Infallible(PreprocessClosure),
    Fallible(TryPreprocessClosure),
}

use PreprocessKind::*;

const GENERICIZE_EXPRESSIONS: PreprocessKind = Infallible(|mut node| {
    if node.is_nonterminal_expression() {
        node.morph(NodeKind::Virt(Virtual::GenericExpression));
    }

    return node;
});

const UNPARENTHETIZE: PreprocessKind = Infallible(|mut node| {
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
    };
    return node;
});

const REDUCE_EXPRESSIONS: PreprocessKind = Infallible(|node| {
    if node.is_expression()
        && node.get_children().len() == 1
        && node.get_children()[0].is_expression()
    {
        return node.unpeel_children().into_iter().nth(0).unwrap();
    }
    return node;
});

const DECLARATION_CHECK: PreprocessKind = Fallible(|node| Ok(node));

pub const PREPROCESSES: [PreprocessKind; 4] = [
    GENERICIZE_EXPRESSIONS,
    UNPARENTHETIZE,
    REDUCE_EXPRESSIONS,
    DECLARATION_CHECK,
];

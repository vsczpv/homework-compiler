use crate::lex::tokens::Token;

use super::tree::{AstNode, NodeKind, Virtual};

pub type PreprocessClosure = fn(Box<AstNode>) -> Box<AstNode>;

pub enum PreprocessKind {
    Prefix(PreprocessClosure),
    Postfix(PreprocessClosure),
}

use PreprocessKind::*;

const GENERICIZE_EXPRESSIONS: PreprocessKind = Prefix(|mut node| {
    if node.is_nonterminal_expression() {
        node.morph(NodeKind::Virt(Virtual::GenericExpression));
    }

    return node;
});

const UNPARENTHETIZE: PreprocessKind = Prefix(|mut node| {
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

const REDUCE_EXPRESSIONS: PreprocessKind = Postfix(|node| {
    if node.is_expression()
        && node.get_children().len() == 1
        && node.get_children()[0].is_expression()
    {
        return node.unpeel_children().into_iter().nth(0).unwrap();
    }
    return node;
});

pub const PREPROCESSES: [PreprocessKind; 3] =
    [GENERICIZE_EXPRESSIONS, UNPARENTHETIZE, REDUCE_EXPRESSIONS];

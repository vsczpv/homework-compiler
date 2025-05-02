use std::{error::Error, fmt::Display};

use crate::{
    lex::{lexer::Lexeme, tokens::Token},
    syn::tree::NonTerminal,
};

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
        if matches!(node.get_kind(), NodeKind::Non(NonTerminal::ExprL))
            && node.get_children().len() != 1
        {
            node.morph(NodeKind::Virt(Virtual::GenericExpressionList));
        } else {
            node.morph(NodeKind::Virt(Virtual::GenericExpression));
        }
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

const ELEMINATE_COMMAS_IN_EXPRL: PreprocessKind = Infallible(|mut node| {
    if matches!(
        node.get_kind(),
        NodeKind::Virt(Virtual::GenericExpressionList)
    ) {
        let children: Vec<Box<AstNode>> = node
            .unpeel_children()
            .into_iter()
            .filter(|n| {
                n.get_kind()
                    .to_owned()
                    .some_lex()
                    .map_or(true, |l| !matches!(l.get_token(), Token::Colon))
            })
            .collect();
        node = AstNode::new(NodeKind::Virt(Virtual::GenericExpressionList));
        for c in children {
            node.add_child(c);
        }
    }
    return node;
});

const FLATTEN_EXPRL: PreprocessKind = Infallible(|mut node| {
    if matches!(
        node.get_kind(),
        NodeKind::Virt(Virtual::GenericExpressionList)
    ) {
        if matches!(
            node.get_children()[1].get_kind(),
            NodeKind::Virt(Virtual::GenericExpressionList)
        ) {
            let mut newnode = AstNode::new(node.get_kind().to_owned());
            let mut children = node.unpeel_children().into_iter();

            let head = children.next().unwrap();
            let tail = children.next().unwrap();

            newnode.add_child(head);
            for c in tail.unpeel_children() {
                newnode.add_child(c);
            }

            node = newnode;
        }
    }

    return node;
});

macro_rules! declarm {
    ($node:expr, $b:ident, $bg:ident) => {{
        let children = $node.unpeel_children();

        let NodeKind::Lex(errlex) = children.iter().nth(0).unwrap().get_kind().to_owned() else {
            panic!("Invalid preprocessing state.");
        };

        let bindings = match children.iter().nth(1).unwrap().get_kind() {
            NodeKind::Virt(Virtual::GenericExpressionList) => {
                children.into_iter().nth(1).unwrap().unpeel_children()
            }
            NodeKind::Virt(Virtual::GenericExpression) => {
                Vec::from([children.into_iter().nth(1).unwrap()])
            }
            _ => panic!("Invalid preprocessing state."),
        };

        let mut bindlist: Vec<Box<AstNode>> = Vec::new();
        for bind in bindings {
            let defined = bind.get_children().len() != 1;

            let ident = bind
                .follow_line(if defined { 5 } else { 4 })
                .get_kind()
                .to_owned()
                .some_lex()
                .and_then(|lex| {
                    if let Token::Identifier(ident) = lex.get_token() {
                        Some(ident)
                    } else {
                        None
                    }
                })
                .ok_or(AstPreprocessingError(
                    "First element of a binding must be a non-namespaced identifier.".into(),
                    errlex.clone(),
                ))?;

            let mut letbinding = AstNode::new(NodeKind::Virt(Virtual::$b { ident, defined }));

            if defined {
                /* TODO: Use actual application check */
                if bind.follow_line(2).get_children().len() != 1 {
                    return Err(AstPreprocessingError(
                        "Function application disallowed on left-hand side of binding.".into(),
                        errlex.clone(),
                    ));
                }
                let valid = bind.get_children()[1]
                    .get_kind()
                    .to_owned()
                    .some_non()
                    .map_or(false, |nt| matches!(nt, NonTerminal::OptrA));

                if !valid {
                    return Err(AstPreprocessingError(
                        "Missing assignment on binding.".into(),
                        errlex.clone(),
                    ));
                }

                let expr = bind.unpeel_children().into_iter().nth(2).unwrap();
                letbinding.add_child(expr);
            }

            bindlist.push(letbinding);
        }

        let mut res = AstNode::new(NodeKind::Virt(Virtual::$bg));

        for b in bindlist {
            res.add_child(b);
        }

        return Ok(res);
    }};
}

const DECLARATIONS: PreprocessKind = Fallible(|node| match node.get_kind() {
    NodeKind::Non(NonTerminal::LetStmt) => {
        declarm!(node, LetBinding, LetBindingGroup)
    }
    NodeKind::Non(NonTerminal::ConstStmt) => {
        declarm!(node, ConstBinding, ConstBindingGroup)
    }
    _ => Ok(node),
});

pub const PREPROCESSES: [PreprocessKind; 6] = [
    GENERICIZE_EXPRESSIONS,
    UNPARENTHETIZE,
    REDUCE_EXPRESSIONS,
    ELEMINATE_COMMAS_IN_EXPRL,
    FLATTEN_EXPRL,
    DECLARATIONS,
];

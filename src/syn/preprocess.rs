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

const UNPARENTHETIZE_VALUE: PreprocessKind = Infallible(|mut node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Value)) && node.get_children().len() > 2
    {
        let mut newnode = AstNode::new(node.get_kind().to_owned());
        let mut children = node.unpeel_children().into_iter();

        /* <term> */
        newnode.add_child(children.next().unwrap());

        /* OPEN_PAR */
        children.next();

        /* <expr_l> */
        newnode.add_child(children.next().unwrap());

        /* CLOSE_PAR */
        children.next();

        /* <value> */
        if let Some(ast) = children.next() {
            newnode.add_child(ast);
        }

        node = newnode;
    }

    return node;
});

const PROXY_TERM_REDUCTION: PreprocessKind = Infallible(|mut node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Value))
        && node.get_children().len() == 1
    {
        let mut newnode = AstNode::new(NodeKind::Virt(Virtual::WrappedTerm));
        let child = node
            .unpeel_children()
            .into_iter()
            .nth(0)
            .unwrap()
            .unpeel_children()
            .into_iter()
            .nth(0)
            .unwrap();

        newnode.add_child(child);
        node = newnode;
    } else if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Term)) {
        let mut newnode = AstNode::new(NodeKind::Virt(Virtual::WrappedTerm));
        #[rustfmt::skip]
        let child = node
            .unpeel_children()
            .into_iter()
            .nth(0)
            .unwrap();
        newnode.add_child(child);
        node = newnode;
    }

    return node;
});

const FLATTEN_VALUE: PreprocessKind = Infallible(|mut node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Value)) {
        let mut newnode = AstNode::new(node.get_kind().to_owned());
        let len = node.get_children().len();
        let mut children = node.unpeel_children().into_iter();
        match len {
            2 => {
                newnode.add_child(children.next().unwrap());
                let tail = children.next().unwrap();
                if matches!(tail.get_kind(), NodeKind::Non(NonTerminal::Value)) {
                    let morekids = tail.unpeel_children();
                    for k in morekids {
                        newnode.add_child(k);
                    }
                } else {
                    newnode.add_child(tail);
                }
                node = newnode;
            }
            3 => {
                newnode.add_child(children.next().unwrap());
                newnode.add_child(children.next().unwrap());
                let tail = children.next().unwrap();
                if matches!(tail.get_kind(), NodeKind::Non(NonTerminal::Value)) {
                    let morekids = tail.unpeel_children();
                    for k in morekids {
                        newnode.add_child(k);
                    }
                } else {
                    newnode.add_child(tail);
                }
                node = newnode;
            }
            _ => panic!("Invalid preprocessing state."),
        };
    }
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

const VALUE_INTO_APPLICATION: PreprocessKind = Infallible(|mut node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Value)) {
        let mut newnode = AstNode::new(NodeKind::Virt(Virtual::Application));
        for c in node.unpeel_children() {
            newnode.add_child(c);
        }
        node = newnode;
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
                .follow_line(if defined { 4 } else { 3 })
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

            /* Very hacky test for namespaced idents. */
            if bind
                .follow_line(if defined { 3 } else { 2 })
                .get_children()
                .len()
                != 1
            {
                bind.follow_line(if defined { 3 } else { 2 }).print_tree(1);
                return Err(AstPreprocessingError(
                    "First element of a binding must be a non-namespaced identifier.".into(),
                    errlex.clone(),
                ));
            };
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

const FLATTEN_SCOPES: PreprocessKind = Infallible(|mut node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Scope)) {
        let mut newnode = AstNode::new(NodeKind::Virt(Virtual::Scope));
        let mut children = node
            .unpeel_children()
            .into_iter()
            .filter(|v| !matches!(v.get_kind(), NodeKind::Lex(_)));

        newnode.add_child(children.next().unwrap());

        if let Some(v) = children.next() {
            if matches!(v.get_kind(), NodeKind::Virt(Virtual::Scope)) {
                let kids = v.unpeel_children();
                for k in kids {
                    newnode.add_child(k);
                }
            }
        }

        node = newnode;
    }
    return node;
});

const RETURN_AND_YIELD: PreprocessKind = Infallible(|mut node| {
    match node.get_kind() {
        NodeKind::Non(NonTerminal::ReturnStmt) => {
            node.morph(NodeKind::Virt(Virtual::Return));
            node.get_children_mut().remove(0);
        }
        NodeKind::Non(NonTerminal::YieldStmt) => {
            node.morph(NodeKind::Virt(Virtual::Yield));
            node.get_children_mut().remove(0);
        }
        _ => {}
    };
    return node;
});

const NAMESPACES: PreprocessKind = Infallible(|mut node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::NamespaceStmt)) {
        let mut children = node.unpeel_children().into_iter().skip(1);

        let mut newnode = AstNode::new(NodeKind::Virt(Virtual::Namespace {
            ident: children
                .next()
                .unwrap()
                .get_kind()
                .to_owned()
                .some_lex()
                .unwrap()
                .get_token()
                .some_identifier()
                .unwrap()
                .to_owned(),
        }));

        newnode.add_child(children.next().unwrap());
        node = newnode;
    }
    return node;
});

const REDUCE_BLOCKS: PreprocessKind = Infallible(|node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Block)) {
        if matches!(node.get_children()[1].get_kind(), NodeKind::Lex(_)) {
            return AstNode::new(NodeKind::Virt(Virtual::Scope));
        } else {
            return node.unpeel_children().into_iter().skip(1).next().unwrap();
        }
    }
    return node;
});

fn parse_lambda_recurse(
    root: Box<AstNode>,
    sigident: &mut Vec<String>,
    sigtype: &mut Vec<Box<AstNode>>,
) {
    let mut children = root.unpeel_children().into_iter();

    let this_ident = children.next().unwrap();
    let more = children.next().unwrap();
    let this_type = children.next().unwrap();

    sigident.push(
        this_ident
            .get_kind()
            .to_owned()
            .some_lex()
            .unwrap()
            .get_token()
            .some_identifier()
            .cloned()
            .unwrap(),
    );

    if matches!(more.get_kind(), NodeKind::Non(NonTerminal::LVarNTypes)) {
        parse_lambda_recurse(more, sigident, sigtype);
    }

    sigtype.push(this_type);
}

const PARSE_LAMBDA: PreprocessKind = Infallible(|mut node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Lambda)) {
        let mut newnode = AstNode::new(NodeKind::Virt(Virtual::LambdaRoot));
        let mut children = node.unpeel_children().into_iter().skip(1);

        let typesig = children.next().unwrap();
        let scope = children.next().unwrap();

        let mut sigkids = typesig.unpeel_children().into_iter();
        let varntypes = sigkids.next().unwrap();

        sigkids.next();

        let returntype = sigkids.next().unwrap();

        let mut signatures_ident: Vec<String> = Vec::new();
        let mut signatures_type: Vec<Box<AstNode>> = Vec::new();

        parse_lambda_recurse(varntypes, &mut signatures_ident, &mut signatures_type);

        assert_eq!(signatures_ident.len(), signatures_type.len());

        let mut sigsnode = AstNode::new(NodeKind::Virt(Virtual::LambdaSignature));

        for (ident, typ) in signatures_ident
            .into_iter()
            .zip(signatures_type.into_iter())
        {
            let mut kid = AstNode::new(NodeKind::Virt(Virtual::LambdaTypeVarPair { ident }));
            kid.add_child(typ);
            sigsnode.add_child(kid);
        }

        newnode.add_child(sigsnode);
        newnode.add_child(returntype);
        newnode.add_child(scope);

        node = newnode;
    }
    return node;
});

const REDUCE_IDENTS: PreprocessKind = Infallible(|mut node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Ident)) {
        if node.get_children().len() != 1 {
            let mut newnode = AstNode::new(NodeKind::Virt(Virtual::Ident));
            let mut children = node.unpeel_children().into_iter();

            newnode.add_child(children.next().unwrap());

            children.next();

            let kids = children.next().unwrap().unpeel_children();

            for k in kids {
                newnode.add_child(k);
            }

            node = newnode;
        } else {
            node.morph(NodeKind::Virt(Virtual::Ident));
        }
    }
    return node;
});

const REDUCE_SCOPEITEM: PreprocessKind = Infallible(|node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::ScopeItem)) {
        if matches!(
            node.follow_line(1).get_kind(),
            NodeKind::Virt(Virtual::GenericExpression)
        ) || matches!(
            node.follow_line(1).get_kind(),
            NodeKind::Virt(Virtual::GenericExpressionList)
        ) {
            return node.move_follow_line(1);
        } else {
            return node.move_follow_line(2);
        }
    }
    return node;
});

const ACCEPT_IFEXPR: PreprocessKind = Infallible(|mut node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::IfExpr))
        || matches!(node.get_kind(), NodeKind::Non(NonTerminal::IfExprMore))
    {
        node.get_children_mut()
            .retain(|n| !matches!(n.get_kind(), NodeKind::Lex(_)) && n.get_children().len() != 0);

        node.morph(match node.get_kind() {
            NodeKind::Non(NonTerminal::IfExpr) => NodeKind::Virt(Virtual::IfExpr),
            NodeKind::Non(NonTerminal::IfExprMore) => NodeKind::Virt(Virtual::IfExprMore),
            _ => panic!("Invalid preprocessing state."),
        });
    }

    return node;
});

pub const PREPROCESSES: [PreprocessKind; 18] = [
    GENERICIZE_EXPRESSIONS,
    UNPARENTHETIZE,
    UNPARENTHETIZE_VALUE,
    PROXY_TERM_REDUCTION,
    REDUCE_EXPRESSIONS,
    ELEMINATE_COMMAS_IN_EXPRL,
    FLATTEN_VALUE,
    FLATTEN_EXPRL,
    VALUE_INTO_APPLICATION,
    REDUCE_IDENTS,
    DECLARATIONS,
    FLATTEN_SCOPES,
    RETURN_AND_YIELD,
    NAMESPACES,
    REDUCE_BLOCKS,
    PARSE_LAMBDA,
    REDUCE_SCOPEITEM,
    ACCEPT_IFEXPR,
];

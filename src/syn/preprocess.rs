use std::{convert::Infallible, error::Error, fmt::Display, thread::Scope};

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

pub type PreprocessClosure = fn(&mut Box<AstNode>);
pub type TryPreprocessClosure = fn(&mut Box<AstNode>) -> Result<(), AstPreprocessingError>;

pub enum PreprocessKind {
    Infallible(PreprocessClosure),
    Fallible(TryPreprocessClosure),
}

use PreprocessKind::*;

/*
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
*/

const GENERICIZE_EXPRESSIONS: PreprocessKind = Infallible(|node| {
    if node.is_nonterminal_expression() {
        if matches!(node.get_kind(), NodeKind::Non(NonTerminal::ExprL)) && node.ccount() != 1 {
            node.morph(NodeKind::Virt(Virtual::GenericExpressionList));
        } else {
            node.morph(NodeKind::Virt(Virtual::GenericExpression));
        }
    }
});

/*
const UNPARENTHETIZE: PreprocessKind = Infallible(|mut node| {
    if node.is_expression() && node.get_children().len() == 3 {
        #[rustfmt::skip]
        let cond =
            node.get_children()[0]
                .get_kind()
                .to_owned()
                .some_lex()
                .is_some_and(|l| l.get_token() == Token::OpenPar) &

            node.get_children()[1]
                .is_expression() &

            node.get_children()[2]
                .get_kind()
                .to_owned()
                .some_lex()
                .is_some_and(|l| l.get_token() == Token::ClosePar);

        if cond {
            let child = node.unpeel_children().into_iter().nth(1).unwrap();
            node = AstNode::new(NodeKind::Virt(Virtual::GenericExpression));
            node.cpush(child);
        }
    }

    return node;
});
*/

const UNPARENTHETIZE: PreprocessKind = Infallible(|node| {
    if node.is_expression() && node.ccount() == 3 {
        #[rustfmt::skip]
        let cond =
            node.get(0)
                .unwrap()
                .get_kind()
                .to_owned()
                .some_lex()
                .is_some_and(|l| l.get_token() == Token::OpenPar) &

            node.get(1)
                .unwrap()
                .is_expression() &

            node.get(2)
                .unwrap()
                .get_kind()
                .to_owned()
                .some_lex()
                .is_some_and(|l| l.get_token() == Token::ClosePar);

        if cond {
            node.retain(|n| n.get_kind().to_owned().some_lex().is_none());
        }
    }
});

/*
const UNPARENTHETIZE_VALUE: PreprocessKind = Infallible(|mut node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Value)) && node.get_children().len() > 2
    {
        let mut newnode = AstNode::new(node.get_kind().to_owned());
        let mut children = node.unpeel_children().into_iter();

        /* <term> */
        newnode.cpush(children.next().unwrap());

        /* OPEN_PAR */
        children.next();

        /* <expr_l> */
        newnode.cpush(children.next().unwrap());

        /* CLOSE_PAR */
        children.next();

        /* <value> */
        if let Some(ast) = children.next() {
            newnode.cpush(ast);
        }

        node = newnode;
    }

    return node;
});
*/

const UNPARENTHETIZE_VALUE: PreprocessKind = Infallible(|node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Value)) && node.ccount() > 2 {
        node.retain(|n| n.get_kind().to_owned().some_lex().is_none());
    }
});

/*
const PROXY_TERM_REDUCTION: PreprocessKind = Infallible(|mut node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Value))
        && node.get_children().len() == 1
    {
        let mut newnode = AstNode::new(NodeKind::Virt(Virtual::WrappedTerm));

        let child = node.move_follow_line(2);

        newnode.cpush(child);
        node = newnode;
    } else if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Term)) {
        let mut newnode = AstNode::new(NodeKind::Virt(Virtual::WrappedTerm));

        let child = node.move_follow_line(1);

        newnode.cpush(child);
        node = newnode;
    }

    return node;
});
*/

const PROXY_TERM_REDUCTION: PreprocessKind = Infallible(|node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Value)) && node.ccount() == 1 {
        node.morph(NodeKind::Virt(Virtual::WrappedTerm));
        let child = node.raise(0).raise(0);
        node.cpush(child);
    } else if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Term)) {
        node.morph(NodeKind::Virt(Virtual::WrappedTerm));
    }
});

/*
const FLATTEN_VALUE: PreprocessKind = Infallible(|mut node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Value)) {
        let mut newnode = AstNode::new(node.get_kind().to_owned());
        let len = node.get_children().len();
        let mut children = node.unpeel_children().into_iter();
        match len {
            2 => {
                newnode.cpush(children.next().unwrap());
                let tail = children.next().unwrap();
                if matches!(tail.get_kind(), NodeKind::Non(NonTerminal::Value)) {
                    let morekids = tail.unpeel_children();
                    for k in morekids {
                        newnode.cpush(k);
                    }
                } else {
                    newnode.cpush(tail);
                }
                node = newnode;
            }
            3 => {
                newnode.cpush(children.next().unwrap());
                newnode.cpush(children.next().unwrap());
                let tail = children.next().unwrap();
                if matches!(tail.get_kind(), NodeKind::Non(NonTerminal::Value)) {
                    let morekids = tail.unpeel_children();
                    for k in morekids {
                        newnode.cpush(k);
                    }
                } else {
                    newnode.cpush(tail);
                }
                node = newnode;
            }
            _ => panic!("Invalid preprocessing state."),
        };
    }
    return node;
});
*/

const FLATTEN_VALUE: PreprocessKind = Infallible(|node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Value)) {
        let len = node.ccount();
        match len {
            2 | 3 => {
                if matches!(
                    node.get(len - 1).unwrap().get_kind(),
                    NodeKind::Non(NonTerminal::Value)
                ) {
                    let morekids = node.raise(len - 1).unpeel_children();
                    for k in morekids {
                        node.cpush(k);
                    }
                }
            }
            _ => panic!("Invalid preprocessing state."),
        }
    }
});

/*
const REDUCE_EXPRESSIONS: PreprocessKind = Infallible(|node| {
    if node.is_expression()
        && node.get_children().len() == 1
        && node.get_children()[0].is_expression()
    {
        return node.unpeel_children().into_iter().nth(0).unwrap();
    }
    return node;
});
*/

const REDUCE_EXPRESSIONS: PreprocessKind = Infallible(|node| {
    if node.is_expression() && node.ccount() == 1 && node.get(0).unwrap().is_expression() {
        *node = node.raise(0);
    }
});

/*
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
            node.cpush(c);
        }
    }
    return node;
});
*/

const ELEMINATE_COMMAS_IN_EXPRL: PreprocessKind = Infallible(|node| {
    if matches!(node.get_kind(), NodeKind::Virt(Virtual::GenericExpression)) {
        node.retain(|n| n.get_kind().to_owned().some_lex()
        .map_or(true, |l| !matches!(l.get_token(), Token::Colon)))
        todo!()
    }
});

/*
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

            newnode.cpush(head);
            for c in tail.unpeel_children() {
                newnode.cpush(c);
            }

            node = newnode;
        }
    }

    return node;
});
*/

const FLATTEN_EXPRL: PreprocessKind = Infallible(|node| {
    if matches!(node.get_kind(), NodeKind::Virt(Virtual::GenericExpressionList)) {
        if matches!(node.get(1).unwrap().get_kind(), NodeKind::Virt(Virtual::GenericExpressionList)) {
            let kids = node.raise(1).unpeel_children();
            for k in kids {
                node.cpush(k);
            }
        }
    }
});

/*
const VALUE_INTO_APPLICATION: PreprocessKind = Infallible(|mut node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Value)) {
        let mut newnode = AstNode::new(NodeKind::Virt(Virtual::Application));
        for c in node.unpeel_children() {
            newnode.cpush(c);
        }
        node = newnode;
    }

    return node;
});
*/

const VALUE_INTO_APPLICATION: PreprocessKind = Infallible(|node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Value)) {
       node.morph(NodeKind::Virt(Virtual::Application));
    }
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
            if bind.get_children().len() == 1 {
                return Err(AstPreprocessingError(
                    "Bare let statements are not allowed.".into(),
                    errlex.clone(),
                ));
            }

            let (defined, typed) = match bind.get_children()[1].get_kind() {
                NodeKind::Non(NonTerminal::OptrA) => (
                    true,
                    bind.get_children()[0].get_children().len() == 3
                        && bind.get_children()[0].get_children()[1]
                            .get_kind()
                            .to_owned()
                            .some_lex()
                            .is_some(),
                ),
                NodeKind::Lex(_) => (false, true),
                _ => panic!("Invalid preprocessing state."),
            };

            let idepth = match (defined, typed) {
                /*
                    expr
                        expr
                            expr
                                wrapped
                                    ident
                                        Ident
                            spec
                            type
                        optra
                        expr
                */
                (true, true) => 5,
                /*
                    expr
                        expr
                            wrapped
                                ident
                                    ident
                        optra
                        expr
                */
                (true, false) => 4,
                /*
                    expr
                        expr
                            wrapped
                                ident
                                    ident
                        spec
                        type
                */
                (false, true) => 4,
                (false, false) => {
                    return Err(AstPreprocessingError(
                        "Declaration with no type and no value are forbidden.".into(),
                        errlex.to_owned(),
                    ))
                }
            };

            if bind.follow_line2(idepth - 1, 0).get_children().len() != 1 {
                return Err(AstPreprocessingError(
                    "First element of a binding must be a non-namespaced identifier.".into(),
                    errlex.to_owned(),
                ));
            }

            let ident = bind
                .follow_line2(idepth, 0)
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

            let mut letbinding = AstNode::new(NodeKind::Virt(Virtual::$b {
                ident,
                defined,
                typed,
                generation: None,
            }));

            if defined {
                if bind.follow_line(2).is_application() {
                    return Err(AstPreprocessingError(
                        "Function application disallowed on left-hand side of binding.".into(),
                        errlex.clone(),
                    ));
                }
            }

            let mut kids = bind.unpeel_children().into_iter();

            let tpe = match (defined, typed) {
                (true, true) => Some(kids.next().unwrap().move_follow_line2(1, 2)),
                (true, false) => {
                    kids.next();
                    None
                }
                (false, true) => Some(kids.nth(2).unwrap()),
                (false, false) => panic!("Invalid preprocessor state."),
            };

            kids.next();

            if defined {
                let expr = kids.next().unwrap();
                letbinding.add_child(expr);
            }

            if let Some(t) = tpe {
                letbinding.add_child(t);
            };

            bindlist.push(letbinding);
        }

        let mut res = AstNode::new(NodeKind::Virt(Virtual::$bg));

        for b in bindlist {
            res.add_child(b);
        }

        return Ok(res);
    }};
}

/*
const DECLARATIONS: PreprocessKind = Fallible(|node| match node.get_kind() {
    NodeKind::Non(NonTerminal::LetStmt) => {
        declarm!(node, LetBinding, LetBindingGroup)
    }
    NodeKind::Non(NonTerminal::ConstStmt) => {
        declarm!(node, ConstBinding, ConstBindingGroup)
    }
    _ => Ok(node),
});
*/

const DECLARATIONS: PreprocessKind = Fallible(|node| match node.get_kind() {
    NodeKind::Non(NonTerminal::LetStmt) => {
        todo!()
    }
    NodeKind::Non(NonTerminal::ConstStmt) => {
        todo!()
    }
    _ => {
        Ok(())
    }
});

/*
const FLATTEN_SCOPES: PreprocessKind = Infallible(|mut node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Scope)) {
        let mut newnode = AstNode::new(NodeKind::Virt(Virtual::Scope));
        let mut children = node
            .unpeel_children()
            .into_iter()
            .filter(|v| !matches!(v.get_kind(), NodeKind::Lex(_)));

        newnode.cpush(children.next().unwrap());

        if let Some(v) = children.next() {
            if matches!(v.get_kind(), NodeKind::Virt(Virtual::Scope)) {
                let kids = v.unpeel_children();
                for k in kids {
                    newnode.cpush(k);
                }
            }
        }

        node = newnode;
    }
    return node;
});
*/

const FLATTEN_SCOPES: PreprocessKind = Infallible(|node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Scope)) {
        node.morph(NodeKind::Virt(Virtual::Scope));
        node.retain(|n| n.get_kind().to_owned().some_lex().is_none());
        if node.ccount() == 2 {
            let scnd = node.raise(1);
            match scnd.get_kind() {
                NodeKind::Virt(Virtual::Scope) => {
                    for k in scnd.unpeel_children() {
                        node.cpush(k);
                    }
                },
                _ => { eprintln!("warning: non-vscope continuation.")}
            }
        }
        todo!()
    }
});

/*
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
*/

const RETURN_AND_YIELD: PreprocessKind = Infallible(|node| {
   match node.get_kind() {
       NodeKind::Non(NonTerminal::ReturnStmt) => {
           node.morph(NodeKind::Virt(Virtual::Return));
           node.raise(0);
       }
       NodeKind::Non(NonTerminal::YieldStmt) => {
           node.morph(NodeKind::Virt(Virtual::Yield));
           node.raise(0);
       }
       _ => {}
   } 
});

/*
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

        newnode.cpush(children.next().unwrap());
        node = newnode;
    }
    return node;
});
*/

const NAMESPACES: PreprocessKind = Infallible(|node| {
   if matches!(node.get_kind(), NodeKind::Non(NonTerminal::NamespaceStmt))  {
       node.morph(NodeKind::Virt(Virtual::Namespace {
           ident: node
               .get(1)
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
       node.retain(|n| matches!(n.get_kind(), NodeKind::Virt(Virtual::Scope)));
   }
});

/*
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
*/

const REDUCE_BLOCKS: PreprocessKind = Infallible(|node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Block)) {
        if matches!(node.get(1).unwrap().get_kind(), NodeKind::Lex(_)) {
            *node = AstNode::new(NodeKind::Virt(Virtual::Scope));
        } else {
            *node = node.raise(1);
        }
    }
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
            kid.cpush(typ);
            sigsnode.cpush(kid);
        }

        newnode.cpush(sigsnode);
        newnode.cpush(returntype);
        newnode.cpush(scope);

        node = newnode;
    }
    return node;
});

const REDUCE_IDENTS: PreprocessKind = Infallible(|mut node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Ident)) {
        if node.get_children().len() != 1 {
            let mut newnode = AstNode::new(NodeKind::Virt(Virtual::Ident));
            let mut children = node.unpeel_children().into_iter();

            newnode.cpush(children.next().unwrap());

            children.next();

            let kids = children.next().unwrap().unpeel_children();

            for k in kids {
                newnode.cpush(k);
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

const ACCEPT_WHILEEXPR: PreprocessKind = Infallible(|mut node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::WhileExpr)) {
        node.get_children_mut()
            .retain(|n| !matches!(n.get_kind(), NodeKind::Lex(_)));

        node.morph(NodeKind::Virt(Virtual::WhileExpr));
        if node.get_children().len() == 3 {
            assert!(matches!(
                node.get_children()[2].get_kind(),
                NodeKind::Non(NonTerminal::MaybeYield)
            ));
            let myield = &mut node.get_children_mut()[2];
            if myield.get_children().len() == 0 {
                node.get_children_mut().remove(2);
            } else {
                myield
                    .get_children_mut()
                    .retain(|n| n.get_kind().to_owned().some_lex().is_none());
                node.move_shuffle_n_transform(&[
                    (0, &|n| n),
                    (1, &|n| n),
                    (2, &|n| n.move_follow_line(1)),
                ]);
            }
        }
    }
    return node;
});

const ACCEPT_FOREXPR: PreprocessKind = Fallible(|mut node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::ForExpr)) {
        node.get_children_mut()
            .retain(|n| n.get_kind().to_owned().some_lex().is_none());

        let idn = &node.get_children()[0];

        assert!(matches!(idn.get_kind(), NodeKind::Virt(Virtual::Ident)));
        if idn.get_children().len() != 1 {
            return Err(AstPreprocessingError(
                "Identifier in for expression must be non-namespaced.".into(),
                Lexeme::default(),
            ));
        }

        let ident = idn
            .follow_line(1)
            .get_kind()
            .to_owned()
            .some_lex()
            .unwrap()
            .get_token()
            .some_identifier()
            .unwrap()
            .clone();

        node.morph(NodeKind::Virt(Virtual::ForExpr { ident: ident }));
        node.get_children_mut().remove(0);

        let myield = &mut node.get_children_mut()[3];

        if myield.get_children().len() == 0 {
            node.get_children_mut().remove(3);
        } else {
            myield
                .get_children_mut()
                .retain(|n| n.get_kind().to_owned().some_lex().is_none());
            node.move_shuffle_n_transform(&[
                (0, &|n| n),
                (1, &|n| n),
                (2, &|n| n),
                (3, &|n| n.move_follow_line(1)),
            ]);
        }
    }
    return Ok(node);
});

const ACCEPT_TYPES: PreprocessKind = Infallible(|mut node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Type)) {
        if matches!(
            node.follow_line(1).get_kind(),
            NodeKind::Non(NonTerminal::BuiltinTypes)
        ) {
            node = node.move_follow_line(1);
            node.morph(NodeKind::Virt(Virtual::Type));
        } else if matches!(
            node.follow_line(1).get_kind(),
            NodeKind::Non(NonTerminal::ArrayType)
        ) {
            node = node.move_follow_line(1);
            node.morph(NodeKind::Virt(Virtual::TypeArray));
            node.get_children_mut()
                .retain(|n| n.get_kind().to_owned().some_lex().is_none());
        }
    }
    return node;
});

const ACCEPT_ARRAY_LIT: PreprocessKind = Infallible(|mut node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Array)) {
        node.get_children_mut()
            .retain(|n| n.get_kind().to_owned().some_lex().is_none());
        node.move_shuffle_n_transform(&[
            (0, &|mut n| {
                n.morph(NodeKind::Virt(Virtual::TypeArray));
                n.get_children_mut()
                    .retain(|n| n.get_kind().to_owned().some_lex().is_none());
                n
            }),
            (1, &|mut n| {
                if n.get_children().len() != 0 {
                    n.get_children_mut()
                        .retain(|m| m.get_kind().to_owned().some_lex().is_none());
                    n = n.move_follow_line(2);
                }
                n
            }),
        ]);
        if node.follow_line2(1, 1).get_children().len() == 0 {
            node.get_children_mut().remove(1);
            node.morph(NodeKind::Virt(Virtual::ArrayLit { filled: false }))
        } else {
            node.morph(NodeKind::Virt(Virtual::ArrayLit { filled: true }))
        }
    }
    return node;
});

const FLATTEN_LTYPES: PreprocessKind = Infallible(|mut node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::LTypes)) {
        if node.get_children().len() != 1
            && (matches!(
                node.follow_line2(1, 1).get_kind(),
                NodeKind::Non(NonTerminal::LTypes)
            ) || matches!(
                node.follow_line2(1, 1).get_kind(),
                NodeKind::Virt(Virtual::TypeList)
            ))
        {
            let mut newnode = AstNode::new(NodeKind::Virt(Virtual::TypeList));
            let mut children = node.unpeel_children().into_iter();

            let head = children.next().unwrap();
            let tail = children.next().unwrap();

            newnode.cpush(head);

            for c in tail.unpeel_children() {
                newnode.cpush(c);
            }

            node = newnode;
        }
    }
    return node;
});

const ACCEPT_LTYPE: PreprocessKind = Infallible(|mut node| {
    if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Type)) {
        if node.get_children().len() == 3
            && node
                .follow_line2(1, 0)
                .get_kind()
                .to_owned()
                .some_lex()
                .is_some()
            && node
                .follow_line2(1, 1)
                .get_kind()
                .to_owned()
                .some_non()
                .is_some()
            && node
                .follow_line2(1, 2)
                .get_kind()
                .to_owned()
                .some_lex()
                .is_some()
        {
            node = node.move_follow_line2(1, 1);
            node.morph(NodeKind::Virt(Virtual::TypeLambda));
            node.get_children_mut()
                .retain(|n| n.get_kind().to_owned().some_lex().is_none());
        }
    }
    return node;
});

const ACCEPT_OPTR: PreprocessKind = Infallible(|mut node| {
    if let NodeKind::Lex(l) = node.get_kind().to_owned() {
        if l.get_token().is_logicoptr() || l.get_token().is_bitwiseoptr() {
            let mut newnode = AstNode::new(NodeKind::Virt(Virtual::Optr));
            newnode.cpush(node);
            return newnode;
        }
    } else {
        if node.is_operator() {
            node.morph(NodeKind::Virt(Virtual::Optr));
        } else if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Prefix)) {
            node.morph(NodeKind::Virt(Virtual::OptrPrefix));
        } else if matches!(node.get_kind(), NodeKind::Non(NonTerminal::Postfix)) {
            node.morph(NodeKind::Virt(Virtual::OptrPostfix));
        }
    }
    return node;
});

const UNPARENTHETIZE_EXPRL: PreprocessKind = Infallible(|mut node| {
    if let NodeKind::Virt(Virtual::GenericExpression) = node.get_kind().to_owned() {
        if node.get_children().len() == 3 {
            let kids = node.get_children();
            if kids[0]
                .get_kind()
                .to_owned()
                .some_lex()
                .is_some_and(|l| matches!(l.get_token(), Token::OpenPar))
                && kids[1]
                    .get_kind()
                    .to_owned()
                    .some_virt()
                    .is_some_and(|v| matches!(v, Virtual::GenericExpressionList))
                && kids[2]
                    .get_kind()
                    .to_owned()
                    .some_lex()
                    .is_some_and(|l| matches!(l.get_token(), Token::ClosePar))
            {
                node.get_children_mut()
                    .retain(|n| n.get_kind().to_owned().some_virt().is_some());
            }
        }
    };
    return node;
});

const ACCEPT_BRACK: PreprocessKind = Infallible(|mut node| {
    if let NodeKind::Non(NonTerminal::Brack) = node.get_kind().to_owned() {
        node.get_children_mut()
            .retain(|n| n.get_kind().to_owned().some_lex().is_none());
        node.morph(NodeKind::Virt(Virtual::Brack));
    }
    return node;
});

const WRAP_BRACK_SOURCE: PreprocessKind = Infallible(|mut node| {
    if matches!(node.get_kind(), NodeKind::Virt(Virtual::GenericExpression)) {
        if node.get_children().len() == 2
            && matches!(
                node.follow_line2(1, 0).get_kind(),
                NodeKind::Virt(Virtual::WrappedTerm)
            )
            && matches!(
                node.follow_line2(1, 1).get_kind(),
                NodeKind::Virt(Virtual::Brack)
            )
        {
            let mut newnode = AstNode::new(node.get_kind().to_owned());
            let mut kids = node.unpeel_children().into_iter();

            let mut wrapper = AstNode::new(NodeKind::Virt(Virtual::GenericExpression));
            wrapper.cpush(kids.next().unwrap());

            newnode.cpush(wrapper);
            newnode.cpush(kids.next().unwrap());
            node = newnode;
        }
    }
    return node;
});

pub const PREPROCESSES: [PreprocessKind; 28] = [
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
    FLATTEN_SCOPES,
    RETURN_AND_YIELD,
    NAMESPACES,
    REDUCE_BLOCKS,
    PARSE_LAMBDA,
    REDUCE_SCOPEITEM,
    ACCEPT_IFEXPR,
    ACCEPT_WHILEEXPR,
    ACCEPT_FOREXPR,
    DECLARATIONS,
    ACCEPT_TYPES,
    ACCEPT_ARRAY_LIT,
    FLATTEN_LTYPES,
    ACCEPT_LTYPE,
    ACCEPT_OPTR,
    UNPARENTHETIZE_EXPRL,
    ACCEPT_BRACK,
    WRAP_BRACK_SOURCE,
];

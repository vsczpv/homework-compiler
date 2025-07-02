use std::thread::Scope;

use crate::syn::tree::{AstNode, NodeKind, Virtual};

use super::symtab::Symbol;

#[allow(dead_code)]
pub struct LinearSection {
    assembly: String,
    push_result: bool,
}

#[allow(dead_code)]
pub enum STNodeElemKind<'a> {
    SubNode(Box<STNode<'a>>),
    LS(LinearSection),
    Branch(Box<STNode<'a>>, Box<STNode<'a>>, Box<STNode<'a>>),
    WhileLoop(Box<STNode<'a>>, Box<STNode<'a>>),
    ReturnStmt(Box<STNode<'a>>),
}

#[allow(dead_code)]
pub struct STNode<'a> {
    vars: Vec<&'a Symbol>,
    elems: Vec<STNodeElemKind<'a>>,
}

#[allow(dead_code)]
impl<'a> STNode<'a> {
    fn new() -> Box<Self> {
        Box::new(STNode {
            vars: Vec::new(),
            elems: Vec::new(),
        })
    }
    fn search_for_scope(self: &mut Box<Self>, tree: &Box<AstNode>) {
        if matches!(tree.get_kind().to_owned(), NodeKind::Virt(Virtual::Scope)) {
            self.process_scope(tree);
        } else {
            tree.get_children()
                .iter()
                .for_each(|n| self.search_for_scope(n));
        };
    }
    fn process_scope(self: &mut Box<Self>, tree: &Box<AstNode>) {
        /*
         * Virt(LetBindingGroup)
         * TypedVirt(GenericExpression)
         * Virt(ReturnStmt)
         *
         * INTO
         *
         * Vars
         * LinearSection    from expr
         * Branch and While from expr
         * ReturnStmt
         *
         */

        todo!()
    }
}

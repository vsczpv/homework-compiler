#![allow(unused)]

use std::{collections::VecDeque, error::Error, fmt::Display};

use crate::lex::lexer::Lexeme;
use crate::lex::tokens::Token;
use crate::sem::types::*;
use crate::syn::tree::*;

#[derive(Debug)]
pub enum SymbolDefinedState {
    Defined,
    Undefined,
    Transient,
}

impl From<bool> for SymbolDefinedState {
    fn from(value: bool) -> Self {
        match value {
            true => Self::Defined,
            false => Self::Undefined,
        }
    }
}

#[derive(Debug)]
pub struct Symbol {
    pub ident: String,
    pub stype: SymbolMajorType,
    pub defined: SymbolDefinedState,
    pub scope: usize,
    pub used: bool,
}

pub struct SymbolTable {
    syms: Vec<Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable { syms: Vec::new() }
    }
    fn add(&mut self, sym: Symbol) {
        if sym.ident != "_" {
            self.syms.push(sym);
        }
    }
    fn is_ident_in_scope(&self, ident: &String, scope: usize) -> bool {
        for s in self.syms.iter().filter(|e| e.scope == scope) {
            if s.ident.eq(ident) {
                return true;
            }
        }
        return false;
    }
    pub fn print(&self) {
        for s in &self.syms {
            println!("{s:?}");
        }
    }
    pub fn get(&self, ident: String) -> Option<&Symbol> {
        for s in &self.syms {
            if s.ident == ident {
                return Some(s);
            }
        }
        return None;
    }
    pub fn get_mut(&mut self, ident: String) -> Option<&mut Symbol> {
        for s in &mut self.syms {
            if s.ident == ident {
                return Some(s);
            }
        }
        return None;
    }
    pub fn all_syms(&self) -> &Vec<Symbol> {
        &self.syms
    }
}

#[derive(Debug)]
pub struct SemanticError(String);

impl Error for SemanticError {}
impl Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

type SemanticResult = Result<(), SemanticError>;

pub struct IrGen<'a> {
    scope_stack: VecDeque<usize>,
    scope_counter: usize,
    syms: &'a mut SymbolTable,
    last_seen_type: SymbolMajorType,
}

impl<'a> IrGen<'a> {
    pub fn new(syms: &'a mut SymbolTable) -> Self {
        IrGen {
            scope_stack: VecDeque::new(),
            scope_counter: 0,
            syms,
            last_seen_type: SymbolMajorType::Unknown,
        }
    }
    fn in_scope(&self, id: usize) -> bool {
        self.scope_stack.binary_search(&id).is_ok()
    }
    fn search_all_scopes(&self, ident: &String) -> Option<usize> {
        for scopeid in self.scope_stack.iter().rev() {
            if self.syms.is_ident_in_scope(ident, *scopeid) {
                return Some(*scopeid);
            }
        }
        return None;
    }
    fn act_on(&mut self, node: &Box<AstNode>) -> SemanticResult {
        match node.get_kind() {
            NodeKind::Virt(Virtual::Scope) => self.handle_scope(node),
            NodeKind::Virt(Virtual::LetBindingGroup)
            | NodeKind::Virt(Virtual::ConstBindingGroup) => self.handle_bind(node),
            NodeKind::Virt(Virtual::ForExpr { ident }) => self.handle_forexpr(node, ident),
            NodeKind::Virt(Virtual::LambdaRoot) => self.handle_lambda(node),
            NodeKind::Virt(Virtual::Ident) => self.handle_ident(node),
            NodeKind::Virt(Virtual::GenericExpression) => self.handle_expr(node),
            _ => self.generate(node),
        }
    }
    pub fn generate(&mut self, tree: &Box<AstNode>) -> SemanticResult {
        for c in tree.get_children() {
            self.act_on(c)?;
        }
        Ok(())
    }
    pub fn act_on_rev(&mut self, node: &Box<AstNode>) -> SemanticResult {
        match node.get_kind() {
            NodeKind::Virt(Virtual::GenericExpression) => self.handle_expr(node),
            _ => panic!("Internal compiler error: rev on non-rev."),
        }
    }
    pub fn generate_rev(&mut self, tree: &Box<AstNode>) -> SemanticResult {
        for c in tree.get_children() {
            self.generate_rev(tree)?;
        }
        self.act_on_rev(tree)
    }
    fn handle_expr(&mut self, node: &Box<AstNode>) -> SemanticResult {
        let kids = node.get_children();
        if kids.len() == 3 {
            self.act_on(&kids[0]);
            let ltype = self.last_seen_type.clone();

            if kids[1]
                .get_kind()
                .to_owned()
                .some_lex()
                .map(|lex| lex.get_token().is_typeexpect())
                .is_some_and(|b| b)
            {
                /* TODO */
                self.last_seen_type = SymbolMajorType::parse_type(&kids[2], &self.syms);
                eprintln!(
                    "NOTE: Force cast from {ltype:?} to {:?}",
                    self.last_seen_type
                );
                return Ok(());
            }

            self.act_on(&kids[2]);
            let rtype = self.last_seen_type.clone();

            if ltype != rtype {
                return Err(SemanticError(format!(
                    "Type mismatch on '{ltype:?}' v. '{rtype:?}'."
                )));
            }

            match &kids[1].get_kind() {
                _ => self.last_seen_type = ltype,
            };

            Ok(())
        } else if kids.len() == 2 {
            let right_hand = if kids[0].is_unary_optr() {
                self.generate(&kids[1]);
                false
            } else {
                self.generate(&kids[0]);
                true
            };

            let rtype = self.last_seen_type.clone();

            if right_hand {
                if matches!(kids[1].get_kind(), NodeKind::Virt(Virtual::Brack)) {
                    match rtype {
                        SymbolMajorType::Array { elem, quant } => {
                            self.last_seen_type = *elem.clone()
                        }
                        _ => {
                            return Err(SemanticError(format!(
                            "Bracket operator can only be used on arrays, got '{rtype:?}' instead."
                        )))
                        }
                    }
                }
            }

            Ok(())
        } else {
            match kids[0].get_kind() {
                NodeKind::Virt(Virtual::Application) => {
                    self.generate(node);

                    let mut tots = kids[0].get_children().iter();
                    let callee = tots.next().unwrap();

                    let idenct = callee
                        .follow_line2(1, 0)
                        .get_children()
                        .len()
                        .checked_sub(1)
                        .ok_or(SemanticError(format!(
                            "Identifier expected on application."
                        )))?;

                    let ident = callee
                        .follow_line2(1, 0)
                        .follow_line2(1, idenct)
                        .get_kind()
                        .to_owned()
                        .some_lex()
                        .unwrap()
                        .get_token()
                        .some_identifier()
                        .unwrap()
                        .clone();

                    let stype = self.syms.get(ident).unwrap().stype.clone();

                    self.last_seen_type = stype;

                    /* TODO: Check application */
                    /*
                    for args in tots {
                        todo!()
                    }
                    */
                }
                NodeKind::Virt(Virtual::WrappedTerm) => {
                    let kd = kids[0].follow_line2(1, 0);
                    match kd.get_kind() {
                        NodeKind::Lex(lex) => {
                            self.last_seen_type = SymbolMajorType::from_lex(lex, &self.syms);
                        }
                        NodeKind::Virt(Virtual::Ident) => {
                            self.handle_ident(kd)?;
                        }
                        NodeKind::Virt(Virtual::LambdaRoot) => {
                            let mut args: Vec<SymbolMajorType> = Vec::new();

                            for t in kd.follow_line2(1, 0).get_children() {
                                let t =
                                    SymbolMajorType::parse_type(t.follow_line2(1, 0), &self.syms);
                                args.push(t);
                            }

                            let rtype =
                                SymbolMajorType::parse_type(&kd.get_children()[1], &self.syms);

                            let stype = SymbolMajorType::Lambda {
                                args,
                                ret: Box::new(rtype),
                            };
                            /* TODO: Check lambda lit */
                            self.generate(node);
                            self.last_seen_type = stype;
                        }
                        NodeKind::Virt(Virtual::ArrayLit { filled }) => {
                            let tpe = kd.follow_line2(1, 0);
                            /* TODO: Check array lit */
                            self.generate(node);
                            self.last_seen_type = SymbolMajorType::parse_type(tpe, &self.syms);
                        }
                        NodeKind::Virt(Virtual::IfExpr) => {
                            self.generate(node);
                            /* TODO: Handle ifexpr */
                            self.last_seen_type = SymbolMajorType::Builtin(BuiltinTypes::Unit);
                        }
                        NodeKind::Virt(Virtual::ForExpr { ident }) => {
                            self.generate(node);
                            /* TODO: Handle ifexpr */
                            self.last_seen_type = SymbolMajorType::Builtin(BuiltinTypes::Unit);
                        }
                        NodeKind::Virt(Virtual::WhileExpr) => {
                            self.generate(node);
                            /* TODO: Handle ifexpr */
                            self.last_seen_type = SymbolMajorType::Builtin(BuiltinTypes::Unit);
                        }
                        _ => {
                            self.generate(node);
                            self.last_seen_type = SymbolMajorType::Unknown
                        }
                    };
                }
                _ => {
                    self.generate(node);
                    self.last_seen_type = SymbolMajorType::Unknown
                }
            };
            Ok(())
        }
    }
    fn handle_ident(&mut self, node: &Box<AstNode>) -> SemanticResult {
        let last = node.get_children().len() - 1;

        let ident = node.follow_line2(1, last);

        let ident = ident
            .get_kind()
            .to_owned()
            .some_lex()
            .unwrap()
            .get_token()
            .some_identifier()
            .unwrap()
            .clone();

        if last != 0 {
            eprintln!("NOTE: Namespacing not implemented, treating as {ident}.");
        }

        if self.search_all_scopes(&ident).is_some() {
            let sm = self.syms.get_mut(ident.clone()).unwrap();

            if matches!(sm.defined, SymbolDefinedState::Undefined) {
                eprintln!("warning: use of uninitialized symbol {ident}.");
            }

            self.last_seen_type = sm.stype.clone();

            sm.used = true;

            Ok(())
        } else {
            Err(SemanticError(format!("Undeclared symbol '{ident}'.")))
        }
    }

    fn handle_scope(&mut self, node: &Box<AstNode>) -> SemanticResult {
        self.scope_stack.push_back(self.scope_counter);
        self.scope_counter += 1;

        self.generate(node)?;

        self.scope_stack.pop_back();

        Ok(())
    }

    fn handle_lambda(&mut self, node: &Box<AstNode>) -> SemanticResult {
        self.scope_stack.push_back(self.scope_counter);
        self.scope_counter += 1;

        let atscope = *self.scope_stack.back().unwrap();

        let sigs = node.follow_line2(1, 0);

        for c in sigs.get_children() {
            let newsym = Symbol {
                stype: SymbolMajorType::parse_type(c.follow_line2(1, 0), &self.syms),
                defined: SymbolDefinedState::Transient,
                scope: atscope,
                ident: c
                    .get_kind()
                    .to_owned()
                    .some_virt()
                    .and_then(|n| match n {
                        Virtual::LambdaTypeVarPair { ident } => Some(ident),
                        _ => None,
                    })
                    .unwrap(),
                used: false,
            };

            if self.syms.is_ident_in_scope(&newsym.ident, atscope) {
                return Err(SemanticError(format!(
                    "Non unique identifier on lambda definition."
                )));
            }

            self.syms.add(newsym);
        }

        /* TODO */
        self.act_on(node.follow_line2(1, 1))?;
        self.act_on(node.follow_line2(1, 2))?;

        self.scope_stack.pop_back();
        Ok(())
    }

    fn handle_forexpr(&mut self, node: &Box<AstNode>, ident: &String) -> SemanticResult {
        self.scope_stack.push_back(self.scope_counter);
        self.scope_counter += 1;

        let atscope = *self.scope_stack.back().unwrap();

        let newsym = Symbol {
            stype: SymbolMajorType::Builtin(BuiltinTypes::Int),
            defined: SymbolDefinedState::Transient,
            scope: atscope,
            ident: ident.to_owned(),
            used: false,
        };

        self.syms.add(newsym);

        let mut kidsref = node.get_children().iter();
        let range_a = kidsref.next().unwrap();
        let range_b = kidsref.next().unwrap();
        let body = kidsref.next().unwrap();
        let myield = kidsref.next();

        /* TODO */
        self.act_on(range_a)?;
        self.act_on(range_b)?;
        self.act_on(body)?;

        if let Some(n) = myield {
            self.generate(n)?
        };

        self.scope_stack.pop_back();
        Ok(())
    }

    fn handle_bind(&mut self, node: &Box<AstNode>) -> SemanticResult {
        for c in node.get_children() {
            macro_rules! arm {
                ($which:ident) => {
                    if let Virtual::$which {
                        ident,
                        defined,
                        typed,
                    } = c.get_kind().to_owned().some_virt().unwrap()
                    {
                        let atscope = *self.scope_stack.back().unwrap();

                        if self.syms.is_ident_in_scope(&ident, atscope) {
                            return Err(SemanticError(format!(
                                "Redefinition of symbol '{ident}'."
                            )));
                        }

                        let mut kidsref = c.get_children().iter();

                        let expr = if defined {
                            Some(kidsref.next().unwrap())
                        } else {
                            None
                        };

                        let btype = if typed {
                            Some(kidsref.next().unwrap())
                        } else {
                            None
                        };

                        let mut stype = SymbolMajorType::Unknown;

                        if let Some(n) = expr {
                            self.act_on(n)?;
                            stype = self.last_seen_type.clone();
                        }

                        let estype = if let Some(n) = btype {
                            SymbolMajorType::parse_type(n, &self.syms)
                        } else {
                            SymbolMajorType::Unknown
                        };

                        if matches!(stype, SymbolMajorType::Unknown)
                            && matches!(estype, SymbolMajorType::Unknown)
                        {
                            eprintln!("NOTE: Symbol with no type.");
                        }

                        if matches!(stype, SymbolMajorType::Unknown) {
                            stype = estype;
                        } else {
                            if !matches!(estype, SymbolMajorType::Unknown) {
                                if stype != estype {
                                    return Err(SemanticError(format!(
                                        "Conflicting type for bind '{ident}',  '{stype:?}' v. '{estype:?}'"
                                    )));
                                }
                            }
                        };

                        let newsym = Symbol {
                            stype,
                            defined: SymbolDefinedState::from(defined),
                            scope: atscope,
                            ident,
                            used: false,
                        };

                        self.syms.add(newsym);
                    } else {
                        if !c.is_bind() {
                            panic!("Internal compiler error: expected bind.");
                        }
                    };
                };
            }
            arm!(LetBinding);
            arm!(ConstBinding);
        }
        Ok(())
    }
}

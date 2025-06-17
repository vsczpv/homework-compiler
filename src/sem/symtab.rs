#![allow(unused)]

use std::collections::LinkedList;
use std::{collections::VecDeque, error::Error, fmt::Display};

use crate::sem::typechk::*;
use crate::syn::tree::*;

#[derive(Debug)]
pub enum SymbolDefinedState {
    Defined,
    Undefined,
    Transient,
}

impl SymbolDefinedState {
    fn from_bool(b: bool) -> Self {
        match b {
            true => Self::Defined,
            false => Self::Undefined,
            _ => panic!(),
        }
    }
}

#[derive(Debug)]
pub struct Symbol {
    pub stype: SymbolMajorType,
    pub defined: SymbolDefinedState,
    pub scope: usize,
    pub ident: String,
    pub used: bool,
    pub generation: usize,
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
    fn get(&self, ident: &String, scope: usize) -> Option<&Symbol> {
        for s in self.syms.iter().rev() {
            if s.ident.eq(ident) && s.scope == scope {
                return Some(s);
            }
        }
        return None;
    }
    pub fn get_by_gen(&self, gen: usize) -> Option<&Symbol> {
        for s in &self.syms {
            if s.generation == gen {
                return Some(s);
            }
        }
        return None;
    }
    pub fn print(&self) {
        for s in &self.syms {
            println!("{s:?}");
        }
    }
    pub fn get_all_syms(&self) -> &Vec<Symbol> {
        return &self.syms;
    }
}

#[derive(Debug)]
pub struct SemanticError(pub String);

impl Error for SemanticError {}
impl Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub type SemanticResult = Result<Box<AstNode>, SemanticError>;

pub struct SymtabGenerator<'a> {
    scope_stack: VecDeque<usize>,
    scope_counter: usize,
    syms: &'a mut SymbolTable,
    generation: usize,
}

impl<'a> SymtabGenerator<'a> {
    pub fn new(syms: &'a mut SymbolTable) -> Self {
        SymtabGenerator {
            scope_stack: VecDeque::new(),
            scope_counter: 0,
            syms,
            generation: 0,
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
    fn act_on(&mut self, mut node: Box<AstNode>) -> SemanticResult {
        match node.get_kind().to_owned() {
            NodeKind::Virt(Virtual::Scope) => self.handle_scope(node),
            NodeKind::Virt(Virtual::LetBindingGroup)
            | NodeKind::Virt(Virtual::ConstBindingGroup) => self.handle_bind(node),
            NodeKind::Virt(Virtual::ForExpr { ident }) => self.handle_forexpr(node, &ident),
            NodeKind::Virt(Virtual::LambdaRoot) => self.handle_lambda(node),
            NodeKind::Virt(Virtual::Ident) => self.handle_ident(node),
            _ => self.generate(node),
        }
        //        self.typecheck(res)
    }
    pub fn generate(&mut self, mut tree: Box<AstNode>) -> SemanticResult {
        let mut newnode = AstNode::new(tree.get_kind().to_owned());
        for c in tree.unpeel_children() {
            newnode.cpush(self.act_on(c)?);
        }
        Ok(newnode)
        /*
        for c in tree.get_children() {
            self.act_on(c)?;
        }
        Ok(())
        */
    }

    fn handle_ident(&mut self, mut node: Box<AstNode>) -> SemanticResult {
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

        if let Some(scopeid) = self.search_all_scopes(&ident) {
            let sym = self.syms.get(&ident, scopeid).unwrap();
            let vk = ValueKind::Lvalue(sym.stype.clone(), sym.generation);
            node.morph(node.get_kind().to_owned().entype(vk));
            Ok(node)
        } else {
            self.syms.print();
            Err(SemanticError(format!("Undeclared symbol '{ident}'.")))
        }
    }

    fn handle_scope(&mut self, mut node: Box<AstNode>) -> SemanticResult {
        self.scope_stack.push_back(self.scope_counter);
        self.scope_counter += 1;

        let node = self.generate(node)?;

        self.scope_stack.pop_back();

        Ok(node)
    }

    fn handle_lambda(&mut self, mut node: Box<AstNode>) -> SemanticResult {
        self.scope_stack.push_back(self.scope_counter);
        self.scope_counter += 1;

        let atscope = *self.scope_stack.back().unwrap();

        let sigs = node.follow_line2(1, 0);

        for c in sigs.get_children() {
            let newsym = Symbol {
                stype: SymbolMajorType::Unknown,
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
                generation: self.generation,
            };

            self.generation += 1;

            if self.syms.is_ident_in_scope(&newsym.ident, atscope) {
                return Err(SemanticError(format!(
                    "Non unique identifier on lambda definition."
                )));
            }

            self.syms.add(newsym);
        }

        /* TODO */

        let mut newnode = AstNode::new(node.get_kind().to_owned());

        //        assert_eq!(node.get_children().len(), 2);

        for c in node.unpeel_children() {
            newnode.cpush(self.act_on(c)?);
        }

        self.scope_stack.pop_back();
        Ok(newnode)

        /*
        self.act_on(node.follow_line2(1, 1))?;
        self.act_on(node.follow_line2(1, 2))?;

        self.scope_stack.pop_back();
        Ok(())
        */
    }

    fn handle_forexpr(&mut self, mut node: Box<AstNode>, ident: &String) -> SemanticResult {
        self.scope_stack.push_back(self.scope_counter);
        self.scope_counter += 1;

        let atscope = *self.scope_stack.back().unwrap();

        let newsym = Symbol {
            stype: SymbolMajorType::Builtin(BuiltinTypes::Int),
            defined: SymbolDefinedState::Transient,
            scope: atscope,
            ident: ident.to_owned(),
            used: false,
            generation: self.generation,
        };

        self.generation += 1;

        self.syms.add(newsym);

        let mut newnode = AstNode::new(node.get_kind().to_owned());
        let mut kids = node.unpeel_children().into_iter();

        let range_a = kids.next().unwrap();
        let range_b = kids.next().unwrap();
        let body = kids.next().unwrap();
        let myield = kids.next();

        newnode.cpush(self.act_on(range_a)?);
        newnode.cpush(self.act_on(range_b)?);
        newnode.cpush(self.act_on(body)?);

        if let Some(n) = myield {
            newnode.cpush(self.act_on(n)?);
        }

        self.scope_stack.pop_back();

        Ok(newnode)

        /*

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
            self.act_on(n)?
        };

        self.scope_stack.pop_back();
        Ok(())
        */
    }

    fn handle_bind(&mut self, mut node: Box<AstNode>) -> SemanticResult {
        let mut newestnode = AstNode::new(node.get_kind().to_owned());
        for c in node.unpeel_children() {
            //        for c in node.get_children_mut() {
            macro_rules! arm {
                ($which:ident) => {
                    if let Virtual::$which {
                        ident,
                        defined,
                        typed,
                        generation,
                    } = c.get_kind().clone().some_virt().unwrap()
                    {
                        let cc = c.clone();

                        let atscope = *self.scope_stack.back().unwrap();

                        if self.syms.is_ident_in_scope(&ident, atscope) {
                            return Err(SemanticError(format!(
                                "Redefinition of symbol '{ident}'."
                            )));
                        }

                        //                        let mut kidsref = c.get_children().iter();
//                        let mut newnode = AstNode::new(cc.get_kind().to_owned());

                        let mut newnode = AstNode::new(cc.get_kind().to_owned());

                        let mut kids = cc.unpeel_children().into_iter();

                        let expr = if defined {
                            Some(kids.next().unwrap())
                        } else {
                            None
                        };

                        let btype = if typed {
                            Some(kids.next().unwrap())
                        } else {
                            None
                        };

                        /* TODO */
                        if let Some(n) = expr {
                            newnode.add_child(self.act_on(n)?);
                        }

                        /* TODO */
                        /*
                        let finaltype = if let Some(n) = btype {
                            let ft = SymbolMajorType::parse_type(&n);
                            newnode.add_child(self.act_on(n)?);
                            ft
                        } else {
                            return Err(SemanticError(format!("type error: type required.")));
                        };
                        */

                        let finaltype = if let Some(n) = btype {
                            let tp = self.act_on(n)?;
                            let ft = SymbolMajorType::parse_type(&tp);
                            newnode.add_child(tp);
                            ft
                        } else {
                            return Err(SemanticError(format!("type error: type required.")));
                        };

                        for rk in kids {
                            newnode.add_child(rk);
                        }

                        let mut newnode_kind = NodeKind::Virt(Virtual::$which {
                            ident: ident.clone(),
                            defined,
                            typed,
                            generation: Some(self.generation)
                        });

                        let newsym = Symbol {
                            stype: finaltype,
                            defined: SymbolDefinedState::from_bool(defined),
                            scope: atscope,
                            ident,
                            used: false,
                            generation: self.generation,
                        };

                        newnode.morph(newnode_kind);

                        self.generation += 1;

                        self.syms.add(newsym);

                        newestnode.add_child(newnode);
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
        Ok(newestnode)
    }
}

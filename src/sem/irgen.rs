#![allow(unused)]

use std::{collections::VecDeque, error::Error, fmt::Display};

use crate::syn::tree::*;

#[derive(Debug)]
pub enum BuiltinTypes {
    Int,
    Float,
    Char,
    Unit,
}

#[derive(Debug)]
pub enum SymbolMajorType {
    Builtin(BuiltinTypes),
    Lambda {
        args: Vec<SymbolMajorType>,
        ret: Box<SymbolMajorType>,
    },
    Array {
        elem: Box<SymbolMajorType>,
        quant: usize,
    },
    Undefined,
}

#[derive(Debug)]
pub struct Symbol {
    stype: SymbolMajorType,
    defined: bool,
    scope: usize,
    ident: String,
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
}

impl<'a> IrGen<'a> {
    pub fn new(syms: &'a mut SymbolTable) -> Self {
        IrGen {
            scope_stack: VecDeque::new(),
            scope_counter: 0,
            syms,
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
            _ => self.generate(node),
        }
    }
    pub fn generate(&mut self, tree: &Box<AstNode>) -> SemanticResult {
        for c in tree.get_children() {
            self.act_on(c)?;
        }
        Ok(())
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

        //        if self
        //            .syms
        //            .is_ident_in_scope(&ident, *self.scope_stack.back().unwrap())
        if self.search_all_scopes(&ident).is_some() {
            Ok(())
        } else {
            self.syms.print();
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
                stype: SymbolMajorType::Undefined,
                defined: false, // TODO,
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
            defined: false, // TODO
            scope: atscope,
            ident: ident.to_owned(),
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
            self.act_on(n)?
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

                        /* TODO */
                        if let Some(n) = expr {
                            self.act_on(n)?;
                        }

                        /* TODO */
                        if let Some(n) = btype {
                            self.act_on(n)?;
                        }

                        let newsym = Symbol {
                            stype: SymbolMajorType::Undefined,
                            defined,
                            scope: atscope,
                            ident,
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

#![allow(unused)]

use std::thread::Scope;

use crate::{
    lex::{lexer::Lexeme, tokens::Token},
    syn::tree::{AstNode, NodeKind, Virtual},
};

use super::symtab::{SemanticError, SemanticResult, SymbolTable, SymtabGenerator};
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BuiltinTypes {
    Int,
    Float,
    Char,
    Unit,
}

#[derive(Debug, Clone)]
/// Pode ser
/// - Valor (Builtin(int, float, unit, char))
/// - Lambda
/// - Array
/// - Unknown
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
    Unknown,
}

impl PartialEq for SymbolMajorType {
    fn eq(&self, other: &Self) -> bool {
        if std::mem::discriminant(self) != std::mem::discriminant(other) {
            return false;
        }
        match self {
            Self::Builtin(lhs) => {
                let Self::Builtin(rhs) = other else {
                    panic!();
                };
                return lhs == rhs;
            }
            Self::Lambda { args, ret } => {
                let largs = args;
                let lret = ret;
                let Self::Lambda { args, ret } = other else {
                    panic!();
                };
                let rargs = args;
                let rret = ret;

                if *lret != *rret {
                    return false;
                }

                if largs.len() != rargs.len() {
                    return false;
                }

                for (l, r) in largs.iter().zip(rargs.iter()) {
                    if *l != *r {
                        return false;
                    }
                }

                return true;
            }
            Self::Array { elem, quant } => {
                let lelem = elem;
                let lquant = quant;
                let Self::Array { elem, quant } = other else {
                    panic!();
                };
                let relem = elem;
                let rquant = quant;

                if *lquant != *rquant {
                    return false;
                }

                if *lelem != *relem {
                    return false;
                }

                return true;
            }
            Self::Unknown => panic!("Internal compiler error: expected type."),
        }
    }
}

impl SymbolMajorType {
    pub fn is_array(&self) -> bool {
        match self {
            Self::Array { elem, quant } => true,
            _ => false,
        }
    }
    pub fn get_array_elem(&self) -> Option<Box<SymbolMajorType>> {
        match self {
            Self::Array { elem, quant } => Some(elem.clone()),
            _ => None,
        }
    }
    pub fn get_array_quant(&self) -> Option<usize> {
        match self {
            Self::Array { elem, quant } => Some(*quant),
            _ => None,
        }
    }
    pub fn from_lex(lex: &Lexeme) -> Self {
        match lex.get_token() {
            Token::TypeInt => Self::Builtin(BuiltinTypes::Int),
            Token::TypeFloat => Self::Builtin(BuiltinTypes::Float),
            Token::TypeChar => Self::Builtin(BuiltinTypes::Char),
            Token::TypeUnit => Self::Builtin(BuiltinTypes::Unit),
            Token::Number(_) => Self::Builtin(BuiltinTypes::Int),
            Token::Float(_) => Self::Builtin(BuiltinTypes::Float),
            Token::CharLiter(_) => Self::Builtin(BuiltinTypes::Char),
            Token::StringLiter(str) => Self::Array {
                elem: Box::new(SymbolMajorType::Builtin(BuiltinTypes::Char)),
                quant: str.len(),
            },
            Token::Unit => Self::Builtin(BuiltinTypes::Unit),
            _ => panic!("Internal compiler error: expected typable."),
        }
    }
    pub fn parse_type(node: &Box<AstNode>) -> Self {
        match node.get_kind().to_owned() {
            NodeKind::Lex(_) | NodeKind::Non(_) => {
                panic!("Internal compiler error: expected virt or typed virt.")
            }
            NodeKind::TypedVirt(_, tp) => {
                if let ValueKind::Rvalue(tp) = tp {
                    return tp;
                } else {
                    panic!("internal compiler error: expected rvalue");
                }
            }
            NodeKind::Virt(v) => match v {
                Virtual::Type => {
                    let lex = node
                        .follow_line2(1, 0)
                        .get_kind()
                        .to_owned()
                        .some_lex()
                        .expect("Internal compiler error: expected lex");
                    Self::from_lex(&lex)
                }
                Virtual::TypeArray => {
                    let kids = node.get_children();

                    let at = Self::parse_type(&kids[0]);

                    let len = *(&kids[1])
                        .follow_line2(2, 0)
                        .get_kind()
                        .to_owned()
                        .some_lex()
                        .unwrap()
                        .get_token()
                        .some_number()
                        .expect("Compiler does not support constant analysis.")
                        as usize;

                    Self::Array {
                        elem: Box::new(at),
                        quant: len,
                    }
                }
                Virtual::TypeLambda => {
                    let kids = node.get_children();
                    let ret = Self::parse_type(&kids[1]);

                    let args: Vec<SymbolMajorType> = kids[0]
                        .get_children()
                        .to_owned()
                        .iter()
                        .map(|n| Self::parse_type(n))
                        .collect();
                    Self::Lambda {
                        args,
                        ret: Box::new(ret),
                    }
                }
                _ => panic!(
                    "Internal compiler error: expected typable, got {:?}.",
                    node.get_kind()
                ),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub enum ValueKind {
    LvalueRef(SymbolMajorType),
    Lvalue(SymbolMajorType, usize),
    Rvalue(SymbolMajorType),
}

impl ValueKind {
    fn inner_type(&self) -> &SymbolMajorType {
        match self {
            Self::LvalueRef(res) => res,
            Self::Lvalue(res, _) => res,
            Self::Rvalue(res) => res,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    AddOptr,
    SubOptr,
    BitAndOptr,
    BitOrOptr,
    BitXorOptr,
    AssignOptr,
    Brack,
}

impl From<Token> for Operator {
    fn from(value: Token) -> Self {
        match value {
            Token::PlusSign => Self::AddOptr,
            Token::MinusSign => Self::SubOptr,
            Token::BitAndOptr => Self::BitAndOptr,
            Token::BitOrOptr => Self::BitOrOptr,
            Token::BitXorEqualsOptr => Self::BitXorOptr,
            Token::AssignOptr => Self::AssignOptr,
            _ => panic!(
                "internal compiler error: unimplementer operator {:?}",
                value
            ),
        }
    }
}

impl Operator {
    fn must_be_same(&self) -> bool {
        match self {
            Self::Brack => false,
            _ => true,
        }
    }
    fn integer_capable(&self) -> bool {
        match self {
            Operator::AddOptr => true,
            Operator::SubOptr => true,
            Operator::BitAndOptr => true,
            Operator::BitOrOptr => true,
            Operator::BitXorOptr => true,
            Operator::AssignOptr => true,
            Operator::Brack => true,
        }
    }
    fn float_capable(&self) -> bool {
        match self {
            Operator::AddOptr => true,
            Operator::SubOptr => true,
            Operator::AssignOptr => true,
            Operator::BitAndOptr => false,
            Operator::BitOrOptr => false,
            Operator::BitXorOptr => false,
            Operator::Brack => false,
        }
    }
}

pub fn operator_check(lhs: &ValueKind, rhs: &ValueKind, op: &Operator) -> Option<ValueKind> {
    let SymbolMajorType::Builtin(lhst) = lhs.inner_type() else {
        if *op == Operator::Brack {
            if !lhs.inner_type().is_array() {
                return None;
            }
            let SymbolMajorType::Builtin(rhst) = rhs.inner_type() else {
                return None;
            };
            if matches!(lhs, ValueKind::Lvalue(_, _)) || matches!(lhs, ValueKind::LvalueRef(_)) {
                if !rhst.eq(&BuiltinTypes::Int) {
                    return None;
                }
                let elemtp = *lhs.inner_type().get_array_elem().unwrap();

                let SymbolMajorType::Builtin(elemtp) = elemtp else {
                    return None;
                };

                return Some(ValueKind::LvalueRef(SymbolMajorType::Builtin(elemtp)));
            } else {
                return None;
            }
        } else {
            return None;
        }
    };

    let SymbolMajorType::Builtin(rhst) = rhs.inner_type() else {
        return None;
    };

    if op.eq(&Operator::Brack) {
        return None;
    }

    if op.must_be_same() {
        if !rhst.eq(lhst) {
            return None;
        }
    }
    match op {
        Operator::AssignOptr => {
            if matches!(lhs, ValueKind::Lvalue(_, _)) || matches!(lhs, ValueKind::LvalueRef(_)) {
                return Some(ValueKind::Rvalue(SymbolMajorType::Builtin(lhst.clone())));
            } else {
                return None;
            }
        }
        _ => {
            match lhst {
                BuiltinTypes::Int => {
                    if !op.integer_capable() {
                        return None;
                    }
                }
                BuiltinTypes::Float => {
                    if !op.float_capable() {
                        return None;
                    }
                }
                _ => {
                    todo!("{lhs:?}")
                }
            };

            return Some(ValueKind::Rvalue(SymbolMajorType::Builtin(lhst.clone())));
        }
    }
}

/// Contém apenas:
/// - tabela de símbolos (referência para)
/// - métodos 
pub struct TypeChecker<'a> {
    syms: &'a SymbolTable,
}

impl<'a> TypeChecker<'a> {
    pub fn new(syms: &'a SymbolTable) -> Self {
        TypeChecker { syms }
    }
    pub fn assure_ints(&mut self, tree: Box<AstNode>) -> SemanticResult {
        match tree.get_kind() {
            NodeKind::TypedVirt(_, tp) => {
                if *tp.inner_type() != SymbolMajorType::Builtin(BuiltinTypes::Int) {
                    if !(tp.inner_type().is_array()
                        && (*(tp.inner_type().get_array_elem().unwrap()))
                            .eq(&SymbolMajorType::Builtin(BuiltinTypes::Int)))
                    {
                        return Err(SemanticError(format!(
                            "type error: only ints and int arrays are currently supported. (found {tp:?})"
                        )));
                    }
                }
            }
            _ => {}
        }

        let mut newnode = AstNode::new(tree.get_kind().clone());
        for c in tree.unpeel_children() {
            newnode.add_child(self.assure_ints(c)?);
        }
        return Ok(newnode);
    }
    pub fn bindcheck(&mut self, tree: Box<AstNode>) -> SemanticResult {
        if matches!(
            tree.get_kind(),
            NodeKind::Virt(Virtual::ConstBinding {
                ident,
                defined,
                typed,
                generation
            })
        ) {
            todo!("const binding not yet implemented");
        }
        if let Some(Virtual::LetBinding {
            ident,
            defined,
            typed,
            generation,
        }) = tree.get_kind().to_owned().some_virt()
        {
            if defined {
                self.syms.print();
                let sym = self
                    .syms
                    .get_by_gen(generation.unwrap())
                    .expect("internal compiler error: expected symbol at bind.");
                let expr = tree.follow_line2(1, 0);
                if let NodeKind::TypedVirt(_, tp) = expr.get_kind() {
                    if !tp.inner_type().eq(&sym.stype) {
                        return Err(SemanticError(format!(
                            "type error: attempt to assign {tp:?} to {sym:?}"
                        )));
                    }
                } else {
                    panic!("internal compiler error: expected typable no bindcheck.");
                }
            }
        }

        let mut newnode = AstNode::new(tree.get_kind().clone());
        for c in tree.unpeel_children() {
            newnode.add_child(self.bindcheck(c)?);
        }
        return Ok(newnode);
    }

    /// Primeiro método do TypeChecker chamado pelo main.
    /// 
    /// **Retorna** a descendência do nó, contendo a tipagem de cada expressão
    /// 
    /// 
    /// # Funcionamento
    /// 
    /// Função recursiva que se aplica a cada nó da AST (por *deph first*)
    /// 
    /// 
    pub fn typecheck(&mut self, tree: Box<AstNode>) -> SemanticResult {
        let mut newnode = AstNode::new(tree.get_kind().clone());
        for c in tree.unpeel_children() {
            newnode.add_child(self.typecheck(c)?);
        }

        match newnode.get_kind() {
            NodeKind::Virt(Virtual::Type)
            | NodeKind::Virt(Virtual::TypeArray)
            | NodeKind::Virt(Virtual::TypeLambda) => {
                let tp = SymbolMajorType::parse_type(&newnode);
                newnode.morph(newnode.get_kind().to_owned().entype(ValueKind::Rvalue(tp)));
            }
            NodeKind::Virt(Virtual::Ident) => {
                todo!("ident")
            }
            NodeKind::Virt(Virtual::WrappedTerm) => {
                let kid = newnode.follow_line2(1, 0);

                match kid.get_kind().to_owned() {
                    NodeKind::Lex(l) => {
                        let tp = SymbolMajorType::from_lex(&l);
                        newnode.morph(newnode.get_kind().to_owned().entype(ValueKind::Rvalue(tp)));
                    }
                    NodeKind::TypedVirt(_, tp) => {
                        newnode.morph(newnode.get_kind().to_owned().entype(tp));
                    }
                    NodeKind::Virt(Virtual::Ident) => {
                        todo!("virtident")
                    }
                    _ => {
                        newnode.morph(newnode.get_kind().to_owned().entype(ValueKind::Rvalue(
                            SymbolMajorType::Builtin(BuiltinTypes::Unit),
                        )));
                        eprintln!("warning: Ignoring type of WrappedTerm");
                    }
                }
            }
            NodeKind::Virt(Virtual::GenericExpressionList) => {
                let mut nnnode = AstNode::new(newnode.get_kind().to_owned());
                for c in newnode.unpeel_children() {
                    nnnode.add_child(self.typecheck(c)?);
                }
                newnode = nnnode;

                let NodeKind::TypedVirt(_, last) =
                    newnode.get_children().last().unwrap().get_kind().clone()
                else {
                    panic!();
                };

                newnode.morph(newnode.get_kind().to_owned().entype(last));
            }
            NodeKind::Virt(Virtual::GenericExpression) => match newnode.get_children().len() {
                1 => match newnode.follow_line(1).get_kind() {
                    NodeKind::TypedVirt(_, tp) => {
                        newnode.morph(newnode.get_kind().to_owned().entype(tp.clone()));
                    }
                    _ => todo!(),
                },
                2 => {
                    if matches!(
                        newnode.follow_line2(1, 1).get_kind(),
                        NodeKind::Virt(Virtual::Brack)
                    ) {
                        let optr = Operator::Brack;

                        let NodeKind::TypedVirt(_, source) =
                            newnode.follow_line2(1, 0).get_kind().to_owned()
                        else {
                            panic!("internal compiler error: expected type of source.");
                        };

                        let NodeKind::TypedVirt(_, index) = newnode
                            .follow_line2(1, 1)
                            .follow_line2(1, 0)
                            .get_kind()
                            .to_owned()
                        else {
                            panic!("internal compiler error: expected type of index.");
                        };

                        if let Some(tyck) = operator_check(&source, &index, &optr) {
                            newnode.morph(newnode.get_kind().to_owned().entype(tyck));
                        } else {
                            return Err(SemanticError(format!(
                                "type error: attempt to index {:?} with {:?}",
                                source, index
                            )));
                        }
                    } else {
                        todo!("internal compiler error: unary operations not yet implemented, except brack")
                    }
                }
                3 => {
                    if newnode
                        .follow_line2(1, 1)
                        .get_kind()
                        .to_owned()
                        .some_lex()
                        .is_some()
                    {
                        todo!("internal compiler error: casting not yet implemented")
                    }

                    let optr = Operator::from(
                        newnode
                            .follow_line2(1, 1)
                            .follow_line(1)
                            .get_kind()
                            .to_owned()
                            .some_lex()
                            .unwrap()
                            .get_token(),
                    );

                    let NodeKind::TypedVirt(_, operand_lhs) =
                        newnode.follow_line2(1, 0).get_kind().to_owned()
                    else {
                        panic!("internal compiler error: expected type of lhs")
                    };

                    let NodeKind::TypedVirt(_, operand_rhs) =
                        newnode.follow_line2(1, 2).get_kind().to_owned()
                    else {
                        panic!("internal compiler error: expected type or rhs")
                    };

                    if let Some(tyck) = operator_check(&operand_lhs, &operand_rhs, &optr) {
                        newnode.morph(newnode.get_kind().to_owned().entype(tyck));
                    } else {
                        return Err(SemanticError(format!(
                            "type error: attempt to operate {:?} on {:?} with {:?}",
                            operand_lhs, operand_rhs, optr
                        )));
                    }
                }
                _ => panic!("internal compiler error: GenericExpression of invalid size"),
            },
            _ => {}
        };

        return Ok(newnode);
    }
}

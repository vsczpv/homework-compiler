#![allow(unused)]

use std::process::CommandArgs;

use crate::lex::lexer::Lexeme;
use crate::lex::tokens::Token;
use crate::syn::tree::{AstNode, NodeKind, Virtual};

use super::irgen::SymbolTable;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BuiltinTypes {
    Int,
    Float,
    Char,
    Unit,
}

#[derive(Debug, Clone)]
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
                    panic!()
                };
                let rargs = args;
                let rret = ret;

                if *lret != *rret {
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

impl BuiltinTypes {}

/*
<value> ::= <term> <value> | <term> OPENPAR <expr_l> CLOSEPAR <value> | <term> OPENPAR <expr_l> CLOSEPAR | <term>;
<term>  ::= NUMBER | FLOAT | <ident> | CHARLITER | STRINGLITER | UNIT | <lambda> | <array> | <ifexpr> | <whileexpr> | <forexpr> ;
*/
impl SymbolMajorType {
    pub fn from_lex(lex: &Lexeme, syms: &SymbolTable) -> Self {
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
    pub fn parse_literal(node: &Box<AstNode>) -> Self {
        todo!()
    }
    pub fn get_expr_type(node: &Box<AstNode>) -> Self {
        todo!()
    }
    pub fn parse_operation(node: &Box<AstNode>) -> Self {
        let lhs = &node.get_children()[0];
        let opr = &node.get_children()[1];
        let rha = &node.get_children()[2];

        if opr.get_kind().to_owned().some_lex().is_some() {
            todo!()
        } else {
            todo!()
        }
    }
    pub fn parse_type(node: &Box<AstNode>, syms: &SymbolTable) -> Self {
        match node
            .get_kind()
            .to_owned()
            .some_virt()
            .expect("Internal compiler error: expected virt.")
        {
            Virtual::Type => {
                let lex = node
                    .follow_line2(1, 0)
                    .get_kind()
                    .to_owned()
                    .some_lex()
                    .expect("Internal compiler error: expected lex");
                Self::from_lex(&lex, syms)
            }
            Virtual::TypeArray => {
                let kids = node.get_children();

                let at = Self::parse_type(&kids[0], syms);

                let len = *(&kids[1])
                    .follow_line2(2, 0)
                    .get_kind()
                    .to_owned()
                    .some_lex()
                    .unwrap()
                    .get_token()
                    .some_number()
                    .expect("Compiler does not support constant analisys.")
                    as usize;

                Self::Array {
                    elem: Box::new(at),
                    quant: len,
                }
            }
            Virtual::TypeLambda => {
                let kids = node.get_children();

                let ret = Self::parse_type(&kids[1], syms);

                let args: Vec<SymbolMajorType> = kids[0]
                    .get_children()
                    .to_owned()
                    .into_iter()
                    .map(|n| Self::parse_type(n, syms))
                    .collect();

                Self::Lambda {
                    args,
                    ret: Box::new(ret),
                }
            }
            _ => panic!("Internal compiler error: expected typable."),
        }
    }
}

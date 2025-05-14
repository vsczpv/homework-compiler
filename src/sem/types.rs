#![allow(unused)]

use std::process::CommandArgs;

use crate::lex::lexer::Lexeme;
use crate::lex::tokens::Token;
use crate::syn::tree::{AstNode, NodeKind, Virtual};

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
    Unknown,
}

impl BuiltinTypes {
    pub fn from_lex(lex: &Lexeme) -> Self {
        match lex.get_token() {
            Token::TypeInt => Self::Int,
            Token::TypeFloat => Self::Float,
            Token::TypeChar => Self::Char,
            Token::TypeUnit => Self::Unit,
            Token::Number(_) => Self::Int,
            Token::Float(_) => Self::Float,
            Token::CharLiter(_) => Self::Char,
            Token::StringLiter(_) => todo!("arrays arent working yet"),
            _ => panic!("Internal compiler error: expected typable."),
        }
    }
}

impl SymbolMajorType {
    pub fn parse_type(node: &Box<AstNode>) -> Self {
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
                Self::Builtin(BuiltinTypes::from_lex(&lex))
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
                    .expect("Compiler does not support constant analisys.")
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
                    .into_iter()
                    .map(|n| Self::parse_type(n))
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

use crate::lex::lexer::Lexeme;
use crate::syn::preprocess::*;
use std::ops::Range;

#[derive(Debug, Copy, Clone)]
#[repr(u16)]
pub enum NonTerminal {
    Scope,
    Block,
    ScopeItem,
    Statement,
    LetStmt,
    ConstStmt,
    ReturnStmt,
    YieldStmt,
    NamespaceStmt,
    ExprL,
    ExprLast,
    ExprA,
    Expr9,
    Expr8,
    Expr7,
    Expr6,
    Expr5,
    Expr4,
    Expr3,
    Expr2,
    Expr1,
    Expr0,
    ExprZ,
    ExprP,
    OptrA,
    Optr4,
    Optr3,
    Optr2,
    Optr1,
    Optr0,
    Unary0,
    Unary1,
    Prefix,
    Postfix,
    Brack,
    Value,
    Term,
    Ident,
    Type,
    BuiltinTypes,
    ArrayType,
    Lambda,
    LTypeWithVar,
    LVarNTypes,
    LType,
    LTypes,
    Array,
    ArrayDef,
    IfExpr,
    IfExprMore,
    WhileExpr,
    ForExpr,
    MaybeYield,
    MaybeExprL,
    #[allow(non_camel_case_types)]
    ETA_PRODUCTION,
}

impl From<u16> for NonTerminal {
    fn from(value: u16) -> Self {
        use NonTerminal::*;
        match value {
            0 => Scope,
            1 => Block,
            2 => ScopeItem,
            3 => Statement,
            4 => LetStmt,
            5 => ConstStmt,
            6 => ReturnStmt,
            7 => YieldStmt,
            8 => NamespaceStmt,
            9 => ExprL,
            10 => ExprLast,
            11 => ExprA,
            12 => Expr9,
            13 => Expr8,
            14 => Expr7,
            15 => Expr6,
            16 => Expr5,
            17 => Expr4,
            18 => Expr3,
            19 => Expr2,
            20 => Expr1,
            21 => Expr0,
            22 => ExprZ,
            23 => ExprP,
            24 => OptrA,
            25 => Optr4,
            26 => Optr3,
            27 => Optr2,
            28 => Optr1,
            29 => Optr0,
            30 => Unary0,
            31 => Unary1,
            32 => Prefix,
            33 => Postfix,
            34 => Brack,
            35 => Value,
            36 => Term,
            37 => Ident,
            38 => Type,
            39 => BuiltinTypes,
            40 => ArrayType,
            41 => Lambda,
            42 => LTypeWithVar,
            43 => LVarNTypes,
            44 => LType,
            45 => LTypes,
            46 => Array,
            47 => ArrayDef,
            48 => IfExpr,
            49 => IfExprMore,
            50 => WhileExpr,
            51 => ForExpr,
            52 => MaybeYield,
            53 => MaybeExprL,
            _ => panic!("wrong nt id"),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Virtual {
    GenericExpression,
}

#[derive(Debug, Clone)]
pub enum NodeKind {
    Lex(Lexeme),
    Non(NonTerminal),
    Virt(Virtual),
}

impl Default for NodeKind {
    fn default() -> Self {
        Self::Lex(Lexeme::new(
            crate::lex::tokens::Token::DOLLAR,
            Range {
                start: usize::MAX,
                end: usize::MAX,
            },
            usize::MAX,
            usize::MAX,
        ))
    }
}

#[derive(Debug, Default)]
pub struct AstNode {
    kind: NodeKind,
    children: Vec<Box<AstNode>>,
}

impl AstNode {
    pub fn new(kind: NodeKind) -> Box<Self> {
        Box::new(AstNode {
            kind,
            children: Vec::default(),
        })
    }
    pub fn apply_many(mut self: Box<Self>, processes: &[PreprocessClosure]) -> Box<Self> {
        for p in processes {
            self = self.apply(p);
        }
        return self;
    }
    pub fn apply<F>(self: Box<Self>, transform: F) -> Box<Self>
    where
        F: Fn(Box<Self>) -> Box<Self> + Copy + Clone,
    {
        let mut newnode = AstNode::new(self.get_kind().clone());
        let children = self.unpeel_children();

        for c in children {
            newnode.add_child(c.apply(transform));
        }

        return transform(newnode);
    }
    pub fn add_child(&mut self, child: Box<AstNode>) {
        self.children.push(child);
    }
    pub fn morph(&mut self, kind: NodeKind) {
        self.kind = kind;
    }
    pub fn get_kind(&self) -> &NodeKind {
        &self.kind
    }
    pub fn print_tree(&self, depth: u32) {
        println!("{}{:?}", "    ".repeat(depth as usize), self.kind);
        for c in &self.children {
            c.print_tree(depth + 1);
        }
    }
    pub fn invert_children(&mut self) {
        self.children.reverse();
    }
    pub fn get_children(&self) -> &Vec<Box<AstNode>> {
        &self.children
    }
    pub fn unpeel_children(self) -> Vec<Box<AstNode>> {
        self.children
    }
    pub fn is_nonterminal_expression(&self) -> bool {
        if let NodeKind::Non(kd) = self.get_kind() {
            match kd {
                NonTerminal::ExprL
                | NonTerminal::ExprLast
                | NonTerminal::ExprA
                | NonTerminal::Expr9
                | NonTerminal::Expr8
                | NonTerminal::Expr7
                | NonTerminal::Expr6
                | NonTerminal::Expr5
                | NonTerminal::Expr4
                | NonTerminal::Expr3
                | NonTerminal::Expr2
                | NonTerminal::Expr1
                | NonTerminal::Expr0
                | NonTerminal::ExprZ
                | NonTerminal::ExprP
                | NonTerminal::Unary0
                | NonTerminal::Unary1 => true,
                _ => false,
            }
        } else {
            false
        }
    }
    pub fn is_expression(&self) -> bool {
        if let NodeKind::Virt(kd) = self.get_kind() {
            match kd {
                Virtual::GenericExpression => true,
                #[allow(unreachable_patterns)]
                _ => false,
            }
        } else {
            false
        }
    }
}

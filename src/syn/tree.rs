use crate::lex::lexer::Lexeme;
use crate::sem::typechk::*;
use crate::syn::preprocess::*;
use std::ops::Range;

use std::io::{self, Write};
use std::fs::OpenOptions;

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

#[allow(unused)]
#[derive(Debug, Clone)]
pub enum Virtual {
    AstRoot,
    GenericExpression,
    GenericExpressionList,
    LetBinding {
        ident: String,
        defined: bool,
        typed: bool,
        generation: Option<usize>,
    },
    ConstBinding {
        ident: String,
        defined: bool,
        typed: bool,
        generation: Option<usize>,
    },
    LetBindingGroup,
    ConstBindingGroup,
    WrappedTerm,
    Application,
    Scope,
    Return,
    Yield,
    Namespace {
        ident: String,
    },
    LambdaRoot,
    LambdaSignature,
    LambdaTypeVarPair {
        ident: String,
    },
    Ident,
    IfExpr,
    IfExprMore,
    WhileExpr,
    ForExpr {
        ident: String,
    },
    Type,
    TypeArray,
    ArrayLit {
        filled: bool,
    },
    TypeList,
    TypeLambda,
    Optr,
    OptrPrefix,
    OptrPostfix,
    Brack,
}

#[derive(Debug, Clone)]
pub enum NodeKind {
    Lex(Lexeme),
    Non(NonTerminal),
    Virt(Virtual),
    TypedVirt(Virtual, ValueKind),
}

impl NodeKind {

    /// Transfirna o nó em TypedVirt, que contém sua expressão virtual e se é Lvalue, Rvalue ou RvalueRef
    pub fn entype(self, tp: ValueKind) -> Self {
        match self {
            Self::Virt(v) => Self::TypedVirt(v, tp),
            _ => panic!(),
        }
    }
    pub fn some_lex(self) -> Option<Lexeme> {
        match self {
            NodeKind::Lex(lexeme) => Some(lexeme),
            _ => None,
        }
    }
    pub fn some_non(self) -> Option<NonTerminal> {
        match self {
            NodeKind::Non(nt) => Some(nt),
            _ => None,
        }
    }
    pub fn some_virt(self) -> Option<Virtual> {
        match self {
            NodeKind::Virt(virt) => Some(virt),
            _ => None,
        }
    }
}

impl Default for NodeKind {


    /// Retorna um nó padrão da AST:
    /// 
    /// tipo: Lexeme
    /// 
    /// token: DOLLAR
    /// 
    /// range: (usize::MAX, usize::MAX)
    /// 
    /// line: usize::MAX
    /// 
    /// column: usize::MAX
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

impl Clone for Box<AstNode> {
    fn clone(&self) -> Self {
        let mut newnode = AstNode::new(self.get_kind().to_owned());
        for c in self.get_children().iter() {
            newnode.add_child(c.clone());
        }
        newnode
    }
}

impl AstNode {
    pub fn new(kind: NodeKind) -> Box<Self> {
        Box::new(AstNode {
            kind,
            children: Vec::default(),
        })
    }

    
    /// 
    /// Recebe um nó da AST (self) e um array de funções de preprocessamento
    /// 
    /// Para cada preprocessamento do array, aplica ele na árvore iniciada pelo nó
    /// 
    /// 
    /// 
    pub fn try_apply_many(
        mut self: Box<Self>,
        processes: &[PreprocessKind],
    ) -> Result<Box<Self>, AstPreprocessingError> {
        for p in processes {
            match p {
                PreprocessKind::Infallible(proc) => {
                    self = self.try_apply(|node| Ok(proc(node))).unwrap()
                }
                PreprocessKind::Fallible(proc) => self = self.try_apply(proc)?,
            }
        }
        return Ok(self);
    }


    /// # Finalidade
    /// 
    /// Chamada pela [AstNode::try_apply_many]
    ///
    /// **recebe** um só da AST (self) e uma função conforme especificações no `where`
    ///
    /// **retorna** um Result com nó transformado ou um erro sintático
    /// 
    /// 
    /// > OBS.: o nó é consumido na função
    ///
    /// Recusrivamente, aplica uma transformação em cada nó da árvore
    /// 
    /// 
    /// # Funcionamento
    /// 
    /// Cria uma cópia do nó original
    /// 
    /// Coleta os filhos do nó original
    /// 
    /// Faz uma recursão (`try_apply()`) para cada filho, em *Deph First*
    /// 
    /// Interrompe em qualquer recursão com erro (usando `?`)
    /// 
    /// Retorna, a cada recursão, o nó/sub-árvore transformada
    pub fn try_apply<F>(self: Box<Self>, transform: F) -> Result<Box<Self>, AstPreprocessingError>
    where // Essa cláusula where está defininfo o que é o tipo `F`. Isso fica mais legível do que se colocasse dentro do `()`
        F: Fn(Box<Self>) -> Result<Box<Self>, AstPreprocessingError> + Copy + Clone,
    {
        let mut newnode = AstNode::new(self.get_kind().clone());
        let children = self.unpeel_children();

        for c in children {
            newnode.add_child(c.try_apply(transform)?);
        }

        return transform(newnode);
    }


    /// Adiciona o nó ao vector de filhos
    pub fn add_child(&mut self, child: Box<AstNode>) {
        self.children.push(child);
    }

    /// muda o tipo ([NodeKind]) do nó
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

    
    /// Escreve a árvore em um arquivo localizado em `IDE/outputs/`
    pub fn doc_tree(&self, name: &str) -> io::Result<()> {
        let mut doc_path = String::from("IDE/outputs/");
        doc_path.push_str(name);

        let mut file = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(doc_path)?;
        
        file.write_all(b"")?;
        self.doc_tree_recursive(&mut file, 0)?;
        Ok(())
    }
    pub fn doc_tree_recursive(&self, file: &mut std::fs::File, depth: u32) -> io::Result<()> {
        
        writeln!(file, "{}{:?}", "    ".repeat(depth as usize), self.kind)?;
        for c in &self.children {
            c.doc_tree_recursive(file, depth + 1)?;
        }

        Ok(())
    }


    /// Retorna árvore como String
    pub fn tree_as_string(&self) -> String {

        let mut text = String::from("");
        let ptr = &mut text;
        self.tree_as_string_recursive(ptr, 0);
        return text;
    }
    pub fn tree_as_string_recursive(&self, text: &mut String, depth: u32) -> () {

        text.push_str(format!("{}{:?}\n", "    ".repeat(depth as usize), self.kind).as_str());
        for c in &self.children {
            c.tree_as_string_recursive(text, depth + 1);
        }
    }
    pub fn invert_children(&mut self) {
        
        self.children.reverse();
    }
    pub fn get_children(&self) -> &Vec<Box<AstNode>> {
        &self.children
    }
    pub fn get_children_mut(&mut self) -> &mut Vec<Box<AstNode>> {
        &mut self.children
    }


    /// **Resultado**: o directionésimo filho do nó, indo sempre pela esquerda
    /// 
    /// Vai coletando o filho 0 de cada nó até chegar à profundidade solicitada
    pub fn follow_line<'a>(self: &'a Box<Self>, depth: usize) -> &'a Box<Self> {
        match depth {
            0 => self,
            _ => self.children[0].follow_line(depth - 1),
        }
    }
    pub fn move_follow_line(self: Box<Self>, depth: usize) -> Box<Self> {
        match depth {
            0 => self,
            _ => self
                .unpeel_children()
                .into_iter()
                .next()
                .unwrap()
                .move_follow_line(depth - 1),
        }
    }


    /// **Resultado**: o directionésimo filho do nó, seguindo a direlão especificada
    /// 
    /// Vai coletando o finho \[direction] de cada nó, a até chegar ao desejado
    pub fn follow_line2(self: &Box<Self>, depth: usize, direction: usize) -> &Box<Self> {
        match depth {
            0 => self,
            _ => self.children[direction].follow_line2(depth - 1, direction),
        }
    }
    pub fn move_follow_line2(self: Box<Self>, depth: usize, direction: usize) -> Box<Self> {
        match depth {
            0 => self,
            _ => self
                .unpeel_children()
                .into_iter()
                .nth(direction)
                .unwrap()
                .move_follow_line2(depth - 1, direction),
        }
    }

    /// Retorna os filhos do nó
    pub fn unpeel_children(self) -> Vec<Box<AstNode>> {
        self.children
    }
    pub fn is_operator(&self) -> bool {
        if let NodeKind::Non(kd) = self.get_kind() {
            match kd {
                NonTerminal::OptrA
                | NonTerminal::Optr4
                | NonTerminal::Optr3
                | NonTerminal::Optr2
                | NonTerminal::Optr1
                | NonTerminal::Optr0 => true,
                _ => false,
            }
        } else {
            false
        }
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
    pub fn is_application(&self) -> bool {
        matches!(self.get_kind(), NodeKind::Virt(Virtual::Application))
    }
    #[allow(unused)]
    pub fn is_bind(&self) -> bool {
        match self.get_kind().to_owned().some_virt() {
            Some(Virtual::LetBinding {
                ident,
                defined,
                typed,
                generation,
            }) => true,
            Some(Virtual::ConstBinding {
                ident,
                defined,
                typed,
                generation,
            }) => true,
            _ => false,
        }
    }
    pub fn move_shuffle_n_transform(
        &mut self,
        operations: &[(usize, &dyn Fn(Box<AstNode>) -> Box<AstNode>)],
    ) -> () {
        let mut new_vec = Vec::new();
        for (which, oper) in operations {
            let kid = std::mem::replace(
                &mut self.children[*which],
                AstNode::new(NodeKind::default()),
            );
            new_vec.push(oper(kid));
        }
        self.children = new_vec;
    }
    pub fn make_root(self: Box<Self>) -> Box<Self> {
        let mut newroot = AstNode::new(NodeKind::Virt(Virtual::AstRoot));
        newroot.add_child(self);
        return newroot;
    }
}

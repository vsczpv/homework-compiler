pub type ParserState = u16;
pub type ProductionId = u16;
pub type JumpId = u16;

#[derive(Clone, Copy, Debug)]
pub enum ParserStateAction {
    Accept,
    Shift(ParserState),
    Reduce(ProductionId),
    Error,
}

pub struct ProductionAction {
    pub jumpid: JumpId,
    pub ct_to_pop: u32,
}

use super::constants_autogen::*;
use super::tree::{AstNode, NodeKind, NonTerminal};
use crate::lex::lexer::Lexeme;
use std::collections::VecDeque;
use std::error::Error;
use std::fmt::Display;
use ParserStateAction::*;

#[derive(Clone, Debug)]
pub struct SyntaxError(ParserState, Lexeme);

impl Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "error at state {} at {:?}", self.0, self.1.get_token())
    }
}

impl Error for SyntaxError {}

pub struct SyntaxParser {
    stack: Vec<ParserState>,
    curr_lx: Option<Lexeme>,
    prev_lx: Option<Lexeme>,
    lexemes: VecDeque<Lexeme>,
    tree: Vec<Box<AstNode>>,
}

enum SyntaxParsingState {
    Continue,
    Accept,
    Reject(SyntaxError),
}

impl SyntaxParser {
    pub fn new(lexemes: Vec<Lexeme>) -> Self {
        SyntaxParser {
            stack: Vec::default(),
            curr_lx: None,
            prev_lx: None,
            lexemes: lexemes.into(),
            tree: Vec::default(),
        }
    }

    /// **retorna** - Árvose AST completa, de acordo com seu fluxo de tokens
    ///
    /// > OBS.: **O parser é deletado ao usar este método.**
    ///
    /// # Funcionamento
    /// 
    /// É uma máquina de estados 
    /// 
    /// A cada passo ([SyntaxParser::step]), verifica se:
    /// 
    /// 1. finalizou - retorna a árvore
    /// 
    /// 2. erro de sintaxe - retorna erro
    /// 
    /// 3. continua - executa o passo seguinte
    /// 
    /// As operações na arvore são feitas diretametne pela [SyntaxParser::step]
    pub fn parse(mut self) -> Result<Box<AstNode>, SyntaxError> {
        /* Select initial state */
        self.stack.push(0);
        self.curr_lx = self.lexemes.pop_front();
        loop {
            match self.step() {
                SyntaxParsingState::Accept => {
                    let mut res: Box<AstNode> = Box::<AstNode>::default();
                    std::mem::swap(&mut self.tree[0], &mut res);
                    return Ok(res);
                }
                SyntaxParsingState::Reject(err) => return Err(err),
                SyntaxParsingState::Continue => {}
            }
        }
    }

    /// verifica o que a tabela de análise sintática diz para fazer no estado atual com o token atual
    /// 
    ///  **output** - Retorna o estado da máquina após o passo
    ///
    ///
    /// Já executa a redução, empilhamento e transições
    /// 
    fn step(&mut self) -> SyntaxParsingState {
        let tokenid = self.curr_lx.as_ref().unwrap().get_token().as_index();

        let state = self.stack.last().cloned().expect("syntax state");
        let cmd = PARSER_TABLE[state as usize][tokenid as usize];

        return match cmd {
            Accept => {
                assert_eq!(self.tree.len(), 1);
                return SyntaxParsingState::Accept;
            }
            Shift(state) => {
                self.stack.push(state);

                self.tree
                    .push(AstNode::new(NodeKind::Lex(self.curr_lx.clone().unwrap())));

                self.prev_lx = self
                    .curr_lx
                    .replace(self.lexemes.pop_front().expect("lexemes empty"));

                return SyntaxParsingState::Continue;
            }
            Reduce(production) => {
                let prod = &PRODUCTIONS[production as usize];

                let mut node = AstNode::new(NodeKind::Non(NonTerminal::ETA_PRODUCTION));

                for _ in 0..prod.ct_to_pop {
                    node.add_child(self.tree.pop().unwrap());
                    self.stack.pop();
                }

                node.invert_children();

                let oldstate = self.stack.last().cloned().expect("stack empty");

                let jump =
                    GOTO_TABLE[oldstate as usize][prod.jumpid as usize].expect("syntax error!");

                self.stack.push(jump);

                node.morph(NodeKind::Non(NonTerminal::from(prod.jumpid)));

                self.tree.push(node);

                return SyntaxParsingState::Continue;
            }
            Error => SyntaxParsingState::Reject(SyntaxError(state, self.curr_lx.clone().unwrap())),
        };
    }
}

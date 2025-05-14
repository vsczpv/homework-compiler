

use crate::lex::tokens;
use crate::lex::tokens::Token;

enum SemanticAction {
    openScope,
    closeScope,
    finishStatement,
    returnStatement,
    yieldStatement,

    declareVariable,       // let ## x = y (se o segundo lado for valor normal)
    declareLambda,         // let ## x = lambda (se o segundo lado for uma lambda)
    declareArray,          // let ## x = array (se o segundo lado for um array)
    declareConstantValue,  // const ## x = y (se o segundo lado for valor normal)
    declareConstantLambda, // const ## x = lambda (se o segundo lado for uma lambda)
    declareConstantArray,  // const ## x = array (se o segundo lado for um array)
    
    declareNamespace, // namespace ## x {}
    separateWithComma, // x ##, y

    assign, // x # = y
    logicExpression,
    bitwiseExpression,
    shift,
    sum_sub,
    mult_div,
    cast,
    
    openParenthesis,
    closeParenthesis,

}

struct Semantic {
    semanticTable : SemanticTable
}

impl Semantic {
    fn error(message : &str) {
        
    }
    // Funções complementares ===========---------------===================--------------------======================-------------------------==========================--------------------------=========================
    



    // Ações Semânticas ===========---------------===================--------------------======================-------------------------==========================--------------------------=========================
    fn openScope(&mut self) {
        self.semanticTable.scopeStack.push(Scope::new());
    }
    fn closeScope(&mut self) {
        self.semanticTable.scopeStack.pop();
    }

    /*fn declaracao(&mut self, tokens: Vec<(&Token, &Lexeme)>) {
        if tokens.len() < 3 { error("Declaração de variável inválida") };
        if tokens[0].get_token() != Token::LetBinding { error("Declaração não iniciada com 'let'") };
        if tokens[1].get_token() != Token::Identifier { error("Tentativa de declarar em algo inválido") };
        if tokens[2].get_token() != Token::AssignOptr { error("Declaração que não é uma atribuição") };
        match tokens[3].get_token() {
            Token::Identifier => self.variable_declaration(tokens),
            Token::Lambda     => self.lambda_declaration(tokens),
            Token::Array      => self.array_declaration(tokens),
            _                 => error("Declaração sem valor")
        }
    }*/


    // Identifica Ação Semântica ===========---------------===================--------------------======================-------------------------==========================--------------------------=========================
    fn identificar_acao_semantica(&mut self, node: &AstNode) {
        let kind = node.get_kind();
        match kind {
            NodeKind::Lex(lexeme) => {
                match lexeme.get_token() {

                }

            }
            NodeKind::NonTerminal(nt) => {
                match nt {
                }
            }
            NodeKind::Virtual(v) => {
                match v {
                    
                }
            }
            _ => {}
        }

        for child in node.get_children() {
            self.identificar_acao_semantica(child);
        }


    }
}
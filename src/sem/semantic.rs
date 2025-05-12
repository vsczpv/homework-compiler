

use crate::lex::tokens;
use crate::lex::tokens::Token;


struct Semantic {
    semanticTable : SemanticTable,
    scopeStack    : Vec<i32>,
    scopeCount    : i32
}

impl Semantic {
    fn error(message : &str) {
        
    }
    // Funções complementares ===========---------------===================--------------------======================-------------------------==========================--------------------------=========================
    fn is_in_scope(&self, variable: &SemanticSymbol) -> bool { // Verifica se a variável está no escopo
        for scope in scopeStack[..] {
            if variable.scope == scope { return true }
        }
    }
    fn is_in_scope(&self, id : &str) -> bool { // Verifica se a variável está no escopo
        let symbol = self.semanticTable.getSymbol(id);
        if symbol.is_none() { return false }
        is_in_scope(symbol.unwrap());
    }


    fn value_is_valid(value: &SemanticSymbol) {
        return is_in_scope(&value) && value.declarada() && value.inicializada();
    }


    // Ações Semânticas ===========---------------===================--------------------======================-------------------------==========================--------------------------=========================
    fn openScope(&mut self) {
        self.scopeCount += 1;
        self.scopeStack.push(self.scopeCount);
    }
    fn closeScope(&mut self) {
        self.scopeStack.pop();
    }

    fn variable_declaration(&mut self, tokens: Vec<(&Token, &Lexeme)>) {

    }
    fn lambda_declaration(&mut self, tokens: Vec<(&Token, &Lexeme)>) {

    }
    fn array_declaration(&mut self, tokens: Vec<(&Token, &Lexeme)>) {

    }
    fn declaracao(&mut self, tokens: Vec<(&Token, &Lexeme)>) {
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
    }


    // Identifica Ação Semântica ===========---------------===================--------------------======================-------------------------==========================--------------------------=========================
    fn identificar_acao_semantica(&mut self, tokens: Vec<(&Token, &Lexeme)>) {
    }
}
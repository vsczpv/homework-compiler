
use std::collections::HashMap;


enum Tipo {
    bool,
    char,
    float,
    int,
    unit,
}

struct SemanticSymbol {
    tipo             : Tipo,
    declarada        : bool,
    inicializada     : bool,
    escopo           : i32,
    parametro        : bool,
    posicaoParametro : i32,
    vetor            : bool,
    referencia       : bool,
    lambda           : bool,
}



impl SemanticSymbol {
    fn new(
        p_tipo : Tipo,
        p_declarada : bool,
        p_inicializada : bool,
        p_escopo : int,
        p_parametro : bool,
        p_posicaoParametro : i32,
        p_vetor : bool,
        p_referencia : bool,
        p_lambda : bool
    ) -> SemanticSymbol {
        SemanticSymbol {
            tipe: p_tipo,
            declarada: p_declarada,
            inicializada: p_inicializada, 
            escopo: p_escopo, 
            parametro: p_parametro, 
            posicaoParametro: p_posicaoParametro, 
            vetor: p_vetor, 
            referencia: p_referencia, 
            lambda: p_lambda
        }
        
    }
}

struct SemanticTable {
    symbols : HashMap<String, SemanticSymbol>,
    contId  : i32,
}
impl SemanticTable {
    fn new() -> SemanticTable{
        SemanticTable {symbols: HashMap<String, SemanticSymbol>::new(), contId: 0}
    }

    fn add_symbol(&mut self, tipo: Tipo, declarada: bool, inicializada: bool, escopo: i32, parametro: bool, posicaoParametro: i32, vetor: bool, referencia: bool, lambda: bool) {
        let symbol = SemanticSymbol::new(tipo, declarada, inicializada, escopo, parametro, posicaoParametro, vetor, referencia, lambda);
        self.symbols.insert(id.to_string(), symbol);
    }

    fn get_symbol(&self, id: &str) -> Option<&SemanticSymbol> {
        self.symbols.get(id)
    }

}

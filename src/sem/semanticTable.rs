
use std::collections::HashMap;


enum Tipo {
    bool,
    char,
    float,
    number,
    string,
    unit,
}

struct SemanticTable {
    scopeStack : Vec<Scope>
}
struct Scope {
    symbles: HashMap<String, SemanticSymble> // O identifier fica como chave da map
}
struct SemanticSymbol {
  kind: SymbolKind,          // var, lambda, array, const
}

enum SymbolKind {
    normal_value { // para variáveis, return_type de lambdas, return_type de arrays
        return_type: Tipo,
    },

    Lambda {
        return_type: Box<SymbolKind>, // Tipo de retorno da lambda (pode ser uma lambda)
        param_types: Vec<SymbolKind>,
    },

    Array {
        return_type: Box<SymbolKind>, // Tipo dos elementos do array
        size: i32,
    },
    //Constant,
}



impl SemanticSymbol {
    fn new(
        kind: SymbolKind,
    ) -> SemanticSymbol {
        SemanticSymbol {
            kind: kind,
        }
        
    }
}

impl SemanticTable {
    fn new() -> SemanticTable{
        SemanticTable {symbols: HashMap<String, SemanticSymbol>::new(), contId: 0}
    }

    fn add_variable(&mut self, p_id: &str, p_type: Tipo) {
        let symbol = SemanticSymbol::new(SymbolKind::normal_value {
            return_type: p_type,
        });
        self.symbols.insert(id.to_string(), symbol);
    }

    fn get_symbol(&self, id: &str) -> Option<&SemanticSymbol> {
        for scope in self.scopeStack.iter().rev() {
            if let Some(symbol) = scope.symbols.get(id) {
                return Some(symbol);
            }
        }
        return None;
    }




    fn is_in_scope(&self, id: &str) -> bool {
        for scope in self.scopeStack.iter().rev() {
            if scope.symbols.contains_key(id) {
                return true;
            }
        }
        return false
    }

}

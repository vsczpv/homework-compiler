use crate::common::*;

pub const TOKEN_AMNT: usize = 72;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    DOLLAR,
    /* Comments */
    MultilineComment, /* /\/\*([^\*]|\*+[^\*\/])*\*+\// */
    Comment,          /* /\/\/.+\n/ */

    /* Statements & Keywords  */
    Namespace,    /* namespace */
    LetBinding,   /* let       */
    ConstBinding, /* const   */
    Lambda,       /* lambda    */
    IfStmt,       /* if        */
    ElseStmt,     /* else      */
    WhileLoop,    /* while     */
    ForLoop,      /* for       */
    Yielding,     /* Yielding  */
    Yield,        /* yield     */
    Return,       /* return    */
    Array,        /* array     */

    TypeChar,  /* char */
    TypeFloat, /* float */
    TypeInt,   /* int */
    TypeUnit,  /* unit */

    FlIn,     /* in        */
    LReturns, /* -> */

    /* Meta & Symbols */
    OpenPar,     /* (                      */
    ClosePar,    /* )                      */
    OpenBlock,   /* {                      */
    CloseBlock,  /* }                      */
    OpenBrack,   /* [                      */
    CloseBrack,  /* ]                      */
    DoubleColon, /* ::                     */
    TypeExpect,  /* :                      */
    SemiColon,   /* ;                      */
    Colon,       /* ,                      */
    Range,       /* to                     */

    /* Operators & Signs */
    PlusPlusOptr,     /* ++ */
    MinusMinusOptr,   /* -- */
    PlusEqualsOptr,   /* += */
    MinusEqualsOptr,  /* -= */
    MultEqualsOptr,   /* *= */
    DivEqualsOptr,    /* /= */
    RShiftEqualsOptr, /* >>= */
    LShiftEqualsOptr, /* <<= */
    BitAndEqualsOptr, /* &=  */
    BitXorEqualsOptr, /* ^= */
    BitOrEqualsOptr,  /* |=  */

    PlusSign,   /* +  */
    MinusSign,  /* -  */
    MultOptr,   /* *  */
    DivOptr,    /* /  */
    BitAndOptr, /* &  */
    BitOrOptr,  /* | */
    BitNotOptr, /* ~ */
    BitXorOptr, /* ^ */
    RemOptr,    /* % */
    RShiftOptr, /* >> */
    LShiftOptr, /* << */
    GteOptr,    /* >= */
    LteOptr,    /* <= */
    GtOptr,     /* >  */
    LtOptr,     /* <  */
    EqualsOptr, /* == */
    NeqOptr,    /* != */
    NotOptr,    /* not */
    AndOptr,    /* and */
    OrOptr,     /* or  */
    AssignOptr, /* =  */

    Unit, /* !                      */

    Identifier(String), /* /[a-zA-Z_][a-zA-Z_0-9]+/ */
    Number(ArchInt),    /* /\d+/                  */
    Float(ArchFloat),   /* /\d+\.?\d*|\.\d+/ */

    CharLiter(ArchCharLiter),     /* /\'[^\']\'|\'\\[^\']\'/ */
    StringLiter(ArchStringLiter), /* /\"\"|\"(\\\"|[^\"])+|"/  */

    Whitespace, /* \s* */
    Newline,
    EverythingElse,
}

impl Token {
    pub fn is_typeexpect(&self) -> bool {
        if let Token::TypeExpect = self.clone() {
            return true;
        } else {
            return false;
        }
    }
    pub fn is_identifier(&self) -> bool {
        if let Token::Identifier(_) = self.clone() {
            return true;
        } else {
            return false;
        }
    }
    pub fn some_identifier(&self) -> Option<&String> {
        match self {
            Token::Identifier(s) => Some(s),
            _ => None,
        }
    }
    pub fn is_number(&self) -> bool {
        if let Token::Number(_) = self.clone() {
            return true;
        } else {
            return false;
        }
    }
    pub fn is_float(&self) -> bool {
        if let Token::Float(_) = self.clone() {
            return true;
        } else {
            return false;
        }
    }
    pub fn is_charliter(&self) -> bool {
        if let Token::CharLiter(_) = self.clone() {
            return true;
        } else {
            return false;
        }
    }
    pub fn is_stringliter(&self) -> bool {
        if let Token::StringLiter(_) = self.clone() {
            return true;
        } else {
            return false;
        }
    }
    pub fn is_whitespace(&self) -> bool {
        if Token::Whitespace == *self {
            return true;
        } else {
            return false;
        }
    }
    pub fn is_newline(&self) -> bool {
        if Token::Newline == *self {
            return true;
        } else {
            return false;
        }
    }
    pub fn is_comment(&self) -> bool {
        if Token::MultilineComment == *self {
            return true;
        } else if Token::Comment == *self {
            return true;
        } else {
            return false;
        }
    }
    pub fn is_unknown(&self) -> bool {
        if Token::EverythingElse == *self {
            return true;
        } else {
            return false;
        }
    }
    pub fn as_index(&self) -> u32 {
        match *self {
            Token::DOLLAR => 0,
            Token::MultilineComment => 1,
            Token::Comment => 2,
            Token::Namespace => 3,
            Token::LetBinding => 4,
            Token::ConstBinding => 5,
            Token::Lambda => 6,
            Token::IfStmt => 7,
            Token::ElseStmt => 8,
            Token::WhileLoop => 9,
            Token::ForLoop => 10,
            Token::Yielding => 11,
            Token::Yield => 12,
            Token::Return => 13,
            Token::Array => 14,
            Token::TypeChar => 15,
            Token::TypeFloat => 16,
            Token::TypeInt => 17,
            Token::TypeUnit => 18,
            Token::FlIn => 19,
            Token::LReturns => 20,
            Token::OpenPar => 21,
            Token::ClosePar => 22,
            Token::OpenBlock => 23,
            Token::CloseBlock => 24,
            Token::OpenBrack => 25,
            Token::CloseBrack => 26,
            Token::DoubleColon => 27,
            Token::TypeExpect => 28,
            Token::SemiColon => 29,
            Token::Colon => 30,
            Token::Range => 31,
            Token::PlusPlusOptr => 32,
            Token::MinusMinusOptr => 33,
            Token::PlusEqualsOptr => 34,
            Token::MinusEqualsOptr => 35,
            Token::MultEqualsOptr => 36,
            Token::DivEqualsOptr => 37,
            Token::RShiftEqualsOptr => 38,
            Token::LShiftEqualsOptr => 39,
            Token::BitAndEqualsOptr => 40,
            Token::BitXorEqualsOptr => 41,
            Token::BitOrEqualsOptr => 42,
            Token::PlusSign => 43,
            Token::MinusSign => 44,
            Token::MultOptr => 45,
            Token::DivOptr => 46,
            Token::BitAndOptr => 47,
            Token::BitOrOptr => 48,
            Token::BitNotOptr => 49,
            Token::BitXorOptr => 50,
            Token::RemOptr => 51,
            Token::RShiftOptr => 52,
            Token::LShiftOptr => 53,
            Token::GteOptr => 54,
            Token::LteOptr => 55,
            Token::GtOptr => 56,
            Token::LtOptr => 57,
            Token::EqualsOptr => 58,
            Token::NeqOptr => 59,
            Token::NotOptr => 60,
            Token::AndOptr => 61,
            Token::OrOptr => 62,
            Token::AssignOptr => 63,
            Token::Unit => 64,
            Token::Identifier(_) => 65,
            Token::Number(_) => 66,
            Token::Float(_) => 67,
            Token::CharLiter(_) => 68,
            Token::StringLiter(_) => 69,
            Token::Whitespace => 70,
            Token::Newline => 71,
            Token::EverythingElse => 72,
        }
    }
}

macro_rules! rule {
    ($enum:ident, $id:expr, $match:expr) => {
        (
            Token::$enum,
            $id,
            concat!(concat!(concat!(concat!(r"(?P<", $id), ">"), $match), ")|"),
        ) // r"(?P<" stringify!($id) ">" stringify!($match) ")|")
    };
}

pub fn get_token_id_from_rules<'a>(cap: &'a regex::Captures) -> Option<(usize, regex::Match<'a>)> {
    for (i, tk) in TOKEN_RULES.iter().enumerate() {
        if let Some(m) = cap.name(tk.1) {
            return Some((i, m));
        }
    }
    println!("what");
    return None;
}

pub fn rules_as_single_string() -> String {
    let mut res = String::new();
    for (_, _, rule) in TOKEN_RULES {
        res.push_str(rule);
    }
    return res;
}

/*
 * NOTE: Order matters here.
 *       You must put greedier matches at the end (eg. Token::Identifier and Token::Number go last).
 */
#[rustfmt::skip]
pub const TOKEN_RULES: [(Token, &str, &str); TOKEN_AMNT] = [
    rule!(MultilineComment, "multilinecomment", r"\/\*([^\*]|\*+[^\*\/])*\*+\/"),
    rule!(Comment, "comment", r"\/\/.+\n"),
    rule!(Namespace, "namespace", r"namespace"),
    rule!(LetBinding, "letbinding", r"let"),
    rule!(ConstBinding, "constbinding", r"const"),
    rule!(Lambda, "lambda", r"lambda"),
    rule!(IfStmt, "ifstmt", r"if"),
    rule!(ElseStmt, "elsestmt", r"else"),
    rule!(WhileLoop, "whileloop", r"while"),
    rule!(ForLoop, "forloop", r"for"),
    rule!(Yielding, "yielding", r"yielding"),
    rule!(Yield, "yield", r"yield"),
    rule!(Return, "return", r"return"),
    rule!(Array, "array", r"array"),
    rule!(TypeChar, "typechar", r"char"),
    rule!(TypeFloat, "typefloat", r"float"),
    rule!(TypeInt, "typeint", r"int"),
    rule!(TypeUnit, "typeunit", r"unit"),
    rule!(FlIn, "flin", r"in"),
    rule!(LReturns, "lreturns", r"\->"),
    rule!(OpenPar, "openpar", r"\("),
    rule!(ClosePar, "closepar", r"\)"),
    rule!(OpenBlock, "openblock", r"\{"),
    rule!(CloseBlock, "closeblock", r"\}"),
    rule!(OpenBrack, "openbrack", r"\["),
    rule!(CloseBrack, "closebrack", r"\]"),
    rule!(DoubleColon, "doublecolon", r"\:\:"),
    rule!(TypeExpect, "blockexpect", r"\:"),
    rule!(SemiColon, "semicolon", r"\;"),
    rule!(Colon, "colon", r"\,"),
    rule!(Range, "range", r"to"),
    rule!(PlusPlusOptr, "plusplusoptr", r"\+\+"),
    rule!(MinusMinusOptr, "minusminusoptr", r"\-\-"),
    rule!(PlusEqualsOptr, "plusequalsoptr", r"\+="),
    rule!(MinusEqualsOptr, "minusequalsoptr", r"\-="),
    rule!(MultEqualsOptr, "multequalsoptr", r"\*="),
    rule!(DivEqualsOptr, "divequalsoptr", r"\/="),
    rule!(RShiftEqualsOptr, "rshiftequalsoptr", r">>="),
    rule!(LShiftEqualsOptr, "lshiftequalsoptr", r"<<="),
    rule!(BitAndEqualsOptr, "bitandequalsoptr", r"&="),
    rule!(BitXorEqualsOptr, "bitxorequalsoptr", r"\^="),
    rule!(BitOrEqualsOptr,  "bitorequalsoptr",  r"\|="),
    rule!(PlusSign, "plussing", r"\+"),
    rule!(MinusSign, "minussing", r"\-"),
    rule!(MultOptr, "multoptr", r"\*"),
    rule!(DivOptr, "divoptr", r"\/"),
    rule!(BitAndOptr, "bitandoptr", r"&"),
    rule!(BitOrOptr, "bitoroptr", r"\|"),
    rule!(BitNotOptr, "bitnotoptr", r"~"),
    rule!(BitXorOptr, "bitxoroptr", r"\^"),
    rule!(RemOptr, "remoptr", "%"),
    rule!(RShiftOptr, "rshiftoptr", ">>"),
    rule!(LShiftOptr, "lshiftoptr", "<<"),
    rule!(GteOptr, "gteoptr", r">="),
    rule!(LteOptr, "lteoptr", r"<="),
    rule!(GtOptr, "gtoptr", r">"),
    rule!(LtOptr, "ltoptr", r"<"),
    rule!(EqualsOptr, "equalsoptr", r"=="),
    rule!(NeqOptr, "neqoptr", r"!="),
    rule!(NotOptr, "notoptr", r"not"),
    rule!(AndOptr, "andoptr", r"and"),
    rule!(OrOptr, "oroptr", r"or"),
    rule!(AssignOptr, "assignoptr", r"="),
    rule!(Unit, "unit", r"!"),
    (
        Token::Identifier(String::new()),
        "identifier",
        r"(?P<identifier>[a-zA-Z_][a-zA-Z0-9_]*)|",
    ),
    (Token::Number(0),       "number",    r"(?P<number>\d+)|"),
    (Token::Float(0f64),     "float",     r"(?P<float>\d+\.?\d*|\.\d+)|"),
    (Token::CharLiter('\0'), "charliter", r"(?P<charliter>\'[^\']\'|\'\\[^\']\')|"),
    (
        Token::StringLiter(String::new()),
        "stringliter",
        "(?P<stringliter> \"\"|\"(\\\"|[^\"]+)+\" )|"
    ),
    rule!(Newline, "newline", r"\n"),
    rule!(Whitespace, "whitespace", r"\s"),
    (
        Token::EverythingElse,
        "everythingelse",
        r"(?P<everythingelse>.)|",
    ),
];

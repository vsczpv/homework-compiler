
use crate::common::ArchInt;

pub const TOKEN_AMNT: usize = 44;

#[derive(Debug, PartialEq)]
#[derive(Clone)]
pub enum Token {

	/* Statements & Keywords  */
	Namespace,   /* namespace */
	LetBinding,  /* let       */
	VarBinding,  /* var       */
	Lambda,      /* lambda    */
	IfStmt,      /* if        */
	ElseStmt,    /* else      */
	WhileLoop,   /* while     */
	Yielding,    /* Yielding  */
	Yield,       /* yield     */
	Return,      /* return    */

	/* Meta & Symbols */
	Identifier(String), /* /[a-zA-Z_][a-zA-Z_0-9]+/ */
	Number    (ArchInt), /* /\d+/                  */
	OpenPar,             /* (                      */
	ClosePar,            /* )                      */
	OpenBlock,           /* {                      */
	CloseBlock,          /* }                      */
	BlockExpect,         /* :                      */
	SemiColon,           /* ;                      */
	DoubleColon,         /* ::                     */
	Unit,                /* !                      */
	Colon,               /* ,                      */

	/* Operators & Signs */
	AssignOptr,      /* =  */
	PlusPlusOptr,    /* ++ */
	MinusMinusOptr,  /* -- */
	PlusEqualsOptr,  /* += */
	MinusEqualsOptr, /* -= */
	MultEqualsOptr,  /* *= */
	DivEqualsOptr,   /* /= */
	PlusSign,   /* +  */
	MinusSign,  /* -  */
	MultOptr,   /* *  */
	DivOptr,    /* /  */
	GteOptr,    /* >= */
	LteOptr,    /* <= */
	GtOptr,     /* >  */
	LtOptr,     /* <  */
	EqualsOptr, /* == */
	NeqOptr,    /* != */
	NotOptr,    /* not */
	AndOptr,    /* and */
	OrOptr,     /* or  */
	Whitespace, /* \s* */

	Newline,
	EverythingElse,
}

impl Token {
	pub fn is_identifier(&self) -> bool {
		if let Token::Identifier(_) = self.clone() {
			return true;
		} else {
			return false;
		}
	}
	pub fn is_number(&self) -> bool {
		if let Token::Number(_) = self.clone() {
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
	pub fn is_unknown(&self) -> bool {
		if Token::EverythingElse == *self {
			return true;
		} else {
			return false;
		}
	}
}

macro_rules! rule {
	($enum:ident, $id:expr, $match:expr) => {
		(Token::$enum, $id, concat!(concat!(concat!(concat!(r"(?P<", $id), ">"), $match), ")|"))  // r"(?P<" stringify!($id) ">" stringify!($match) ")|")
	}
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
pub const TOKEN_RULES: [(Token, &str, &str); TOKEN_AMNT] = [

	rule!(Namespace,  "namespace",    r"namespace"),
	rule!(LetBinding, "letbinding",   r"let"),
	rule!(VarBinding, "varbinding",   r"var"),
	rule!(Lambda,     "lambda",       r"lambda"),
	rule!(IfStmt,     "ifstmt",       r"if"),
	rule!(ElseStmt,   "elsestmt",     r"else"),
	rule!(WhileLoop,  "whileloop",    r"while"),
	rule!(Yielding,   "yielding",     r"yielding"),
	rule!(Yield,      "yield",        r"yield"),
	rule!(Return,     "return",       r"return"),

	rule!(OpenPar,     "openpar",     r"\("),
	rule!(ClosePar,    "closepar",    r"\)"),
	rule!(OpenBlock,   "openblock",   r"\{"),
	rule!(CloseBlock,  "closeblock",  r"\}"),
	rule!(DoubleColon, "doublecolon", r"\:\:"),
	rule!(BlockExpect, "blockexpect", r"\:"),
	rule!(SemiColon,   "semicolon",   r"\;"),
	rule!(Colon,       "colon",       r"\,"),

	rule!(PlusPlusOptr,   "plusplusoptr",   r"\+\+"),
	rule!(MinusMinusOptr, "minusminusoptr", r"\-\-"),

	rule!(PlusEqualsOptr,  "plusequalsoptr",  r"\+="),
	rule!(MinusEqualsOptr, "minusequalsoptr", r"\-="),
	rule!(MultEqualsOptr,  "multequalsoptr",  r"\*="),
	rule!(DivEqualsOptr,   "divequalsoptr",   r"\/="),

	rule!(PlusSign,   "plussing",     r"\+"),
	rule!(MinusSign,  "minussing",    r"\-"),
	rule!(MultOptr,   "multoptr",     r"\*"),
	rule!(DivOptr,    "divoptr",      r"\/"),
	rule!(GteOptr,    "gteoptr",      r">="),
	rule!(LteOptr,    "lteoptr",      r"<="),
	rule!(GtOptr,     "gtoptr",       r">"),
	rule!(LtOptr,     "ltoptr",       r"<"),
	rule!(EqualsOptr, "equalsoptr",   r"=="),
	rule!(NeqOptr,    "neqoptr",      r"!="),
	rule!(NotOptr,    "notoptr",      r"not"),
	rule!(AndOptr,    "andoptr",      r"and"),
	rule!(OrOptr,     "oroptr",       r"or"),
	rule!(AssignOptr, "assignoptr",   r"="),

	rule!(Unit,        "unit",        r"!"),

	(Token::Identifier(String::new()), "identifier", r"(?P<identifier>[a-zA-Z_][a-zA-Z0-9_]*)|"),
	(Token::Number(0),                 "number",     r"(?P<number>\d+)|"),

	rule!(Newline,    "newline",      r"\n"),
	rule!(Whitespace, "whitespace",   r"\s"),

	(Token::EverythingElse, "everythingelse", r"(?P<everythingelse>.)|"),

];


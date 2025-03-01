
use crate::lex::tokens;
use crate::lex::tokens::Token;
use regex::Regex;
use core::ops::Range;
use std::fmt;
use std::error::Error;
use crate::common::ArchInt;

#[derive(Debug, Clone)]
pub struct LexError(String);

impl fmt::Display for LexError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.0)
	}
}

impl Error for LexError {}

#[derive(Debug)]
#[allow(unused)]
pub struct Lexeme {
	token_kind: Token,
	index:      usize,
	length:     usize,
	line:       usize,
	col:        usize
}

pub struct Lexer {
	engine: Regex,
	lexemes: Vec<Lexeme>
}

impl Lexeme {
	fn new(token_kind: Token, range: Range<usize>, line: usize, col: usize) -> Self {
		Lexeme {
			token_kind,
			index:  range.start,
			length: range.end - range.start,
			line, col
		}
	}
}

impl Lexer {
	pub fn new() -> Self {
		Lexer {
			engine:  Regex::new(
				tokens::rules_as_single_string().as_str()
			).expect("Lexer's regex engine"),
			lexemes: Vec::new()
		}
	}
	pub fn tokenize(mut self, input: String) -> Result<Vec<Lexeme>, LexError> {

		let mut line = 1;
		let mut col  = 1;

		macro_rules! err {
			($msg:expr) => {
				LexError(format!($msg))
			}
		}

		for cap in self.engine.captures_iter(input.as_str()) {

			let mtch = tokens::get_token_id_from_rules(&cap);
			let mtch = mtch
				.ok_or(err!("impossible lexer state at {line}:{col}"))?;

			let token = tokens::TOKEN_RULES[mtch.0].0.clone();

			if token.is_unknown() {
				let chr = mtch.1.as_str();
				return Err(err!("unknown token '{chr}' at {line}:{col}'"));
			}

			let col_to_add = mtch.1.len();

			if token.is_newline() {
				line += 1;
				col   = 1;
				continue;
			}

			if token.is_whitespace() { col += 1; continue; }

			let token = if token.is_identifier() {
				Token::Identifier(mtch.1.as_str().into())
			} else if token.is_number() {
				Token::Number(mtch.1.as_str().parse::<ArchInt>().unwrap())
			} else {
				token
			};

			self.lexemes.push(Lexeme::new(
				token, mtch.1.range(), line, col
			));

			col += col_to_add;
		}

		Ok(self.lexemes)
	}
}

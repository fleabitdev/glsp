use smallvec::{SmallVec};
use std::cell::{Cell};
use std::convert::{TryFrom};
use std::str::{self, FromStr};
use super::collections::{Arr, DequeOps, Str, Tab};
use super::engine::{Filename, glsp, Span, SpanStorage, stock_syms::*, Sym};
use super::error::{GResult};
use super::gc::{Root};
use super::lex::{char_is_whitespace, Lexer, StrStatus, TokType};
use super::val::{Val};

/*

a streaming parser. 

when a str is passed to Parser::parse(), but it ends partway through a form, the last character
in that str must be '\n'. (this simplifies the lexer by guaranteeing that tokens aren't split 
between one str and the next.)

in practice, this means that you can either incrementally parse input from a REPL line-by-line, 
or batch-parse the entire contents of a (load) or (eval) call.

*/

#[doc(hidden)]
pub struct Parser {
	lexer: Lexer,
	stack: SmallVec<[Form; 32]>,
	prev_tok_type: TokType,
	file: Option<Filename>
}

impl Parser {
	pub fn new(file: Option<Filename>) -> Parser {
		Parser {
			lexer: Lexer::new(),
			stack: SmallVec::new(),
			prev_tok_type: TokType::Whitespace,
			file
		}
	}

	pub fn parse_all(&mut self, mut text: &str, dst: &mut Vec<Val>) -> GResult<usize> {
		let starting_len = dst.len();

		while text.len() > 0 {
			if let Some(form) = self.parse(&mut text)? {
				dst.push(form);
			}
		}

		self.ensure_finished()?;

		Ok(dst.len() - starting_len)
	}

	pub fn parse(&mut self, text: &mut &str) -> GResult<Option<Val>> {
		//we call a toplevel fn to reduce the indentation level
		parse(self, text)
	}

	pub fn ensure_finished(&self) -> GResult<()> {
		match self.lexer.str_status {
			StrStatus::OutsideStr => (),
			StrStatus::InsideStr => bail!("unterminated str"),
			StrStatus::InsideRawStr(_) => bail!("unterminated raw str"),
			StrStatus::InsideBlockComment(_) => bail!("unterminated block comment")
		}

		if let Some(&ref form) = self.stack.last() {
			match form {
				Form::Arr(..) => bail!("unterminated arr"),
				Form::Str(..) | Form::PausedStr(..) | Form::ResumedStr(..) => {
					bail!("unterminated str")
				}
				Form::RawStr(..) => bail!("unterminated raw str"),
				Form::Tab(..) => bail!("unterminated tab"),
				Form::TabClause(..) => bail!("unterminated tab clause"),
				Form::Access(..) => bail!("unterminated []"),
				Form::DiscardNext => bail!("#_ at end of input"),
				Form::Abbrv(sym) => bail!("{} abbreviation at end of input", sym)
			}
		}

		Ok(())
	}
}

//an incompletely-parsed form
enum Form {
	Arr(Root<Arr>),
	Str(Root<Str>, StrEscape),
	RawStr(Root<Str>),
	PausedStr(Root<Arr>),
	ResumedStr(Root<Arr>, Root<Str>, StrEscape),
	Tab(Root<Tab>),
	TabClause(Option<Val>, Option<Val>),
	Access(Root<Arr>),
	DiscardNext,  // #;(a-form)
	Abbrv(Sym),   // '(a-form), etc.
}

#[derive(Copy, Clone, PartialEq)]
enum StrEscape {
	NoEscape,
	NewlineEscape
}

fn parse(parser: &mut Parser, text: &mut &str) -> GResult<Option<Val>> {
	assert!(text.len() > 0, "empty string passed to parse()");

	//we create a span for this token lazily, to minimize the number of spans generated
	let span_storage: Cell<Option<Span>> = Cell::new(None);
	let line_number = parser.lexer.line_number;
	let file = parser.file;
	let span = || -> Span {
		match file {
			Some(file) => {
				if span_storage.get().is_none() {
					span_storage.set(Some(glsp::span(SpanStorage::Loaded(file, line_number))));
				}
				span_storage.get().unwrap()
			}
			None => glsp::generated_span()
		}
	};

	//each call to parse() processes one token
	let tok = match parser.lexer.lex(text) {
		Ok(tok) => tok,
		Err(err) => return Err(error_at!(span(), "lexing error").with_source(err))
	};

	//many tokens are delimited: they must be followed by ), ], }, or whitespace. this prevents a
	//number of syntax corner-cases such as (a"b""c"), (prn #t#f\c10), or \retuurn
	let delimited = match parser.prev_tok_type {
		TokType::NumOrSym | TokType::True | TokType::False | TokType::Nil | TokType::Char | 
		TokType::SpaceChar | TokType::TabChar | TokType::NewlineChar | TokType::ReturnChar |
		TokType::NulChar | TokType::AsciiChar | TokType::UnicodeChar | TokType::ArrClose |
		TokType::AccessClose | TokType::StrClose | TokType::RawStrClose => true,

		TokType::Whitespace | TokType::FormComment | TokType::ArrOpen | TokType::TabOpen |
		TokType::AccessOpen | TokType::StrOpen | TokType::StrPause | TokType::StrResume | 
		TokType::StrChars | TokType::RawStrOpen | TokType::RawStrChars | TokType::Quote | 
		TokType::Backquote | TokType::Unquote | TokType::Splay | TokType::MethName |
		TokType::Atsign => false
	};

	if delimited {
		match tok.tok_type {
			TokType::Whitespace | TokType::ArrClose | 
			TokType::AccessClose | TokType::StrResume => (),
			_ => bail_at!(span(), "the token {:?} must be followed by ), ], }}, or whitespace, \
				          rather than {:?}", parser.prev_tok_type, tok.tok_type)
		}
	}

	parser.prev_tok_type = tok.tok_type;

	//process one token. if it's an atom, such as TrueTok, take its value. if it completes a
	//partial form, such as StrCloseTok, pop that form off the stack and take its value. otherwise,
	//it must be either whitespace (which is ignored), str character data, or a new partial form 
	//to add to the stack.
	let mut parsed_val: Option<Val> = match tok.tok_type {

		//whitespace is legal everywhere, even between an abbreviation and its abbreviated form,
		//or between a form comment and the form which it's commenting out
		TokType::Whitespace => None,
		TokType::FormComment => {
			parser.stack.push(Form::DiscardNext);
			None
		}

		TokType::NumOrSym => Some(parse_num_or_sym(tok.text)?),
		TokType::True => Some(Val::Bool(true)),
		TokType::False => Some(Val::Bool(false)),
		TokType::Nil => Some(Val::Nil),

		TokType::Char => Some(Val::Char(tok.text.chars().nth(1).unwrap())),
		TokType::SpaceChar => Some(Val::Char(' ')),
		TokType::TabChar => Some(Val::Char('\t')),
		TokType::NewlineChar => Some(Val::Char('\n')),
		TokType::ReturnChar => Some(Val::Char('\r')),
		TokType::NulChar => Some(Val::Char('\0')),
		TokType::AsciiChar => Some(Val::Char(parse_ascii_char(tok.text))),
		TokType::UnicodeChar => Some(Val::Char(parse_unicode_char(tok.text, span())?)),

		TokType::ArrOpen => {
			match parser.stack.last() {
				Some(&Form::Tab(_)) => parser.stack.push(Form::TabClause(None, None)),
				_ => {
					let arr = glsp::arr();
					arr.set_span(span());
					parser.stack.push(Form::Arr(arr));
				}
			}
			None
		}
		TokType::TabOpen => {
			parser.stack.push(Form::Tab(glsp::tab()));
			None
		}
		TokType::ArrClose => {
			match parser.stack.pop() {
				Some(Form::Arr(arr)) => {
					arr.freeze();
					Some(Val::Arr(arr))
				}
				Some(Form::Tab(tab)) => {
					tab.freeze();
					Some(Val::Tab(tab))
				}
				Some(Form::TabClause(key, val)) => {
					if let (Some(key), Some(val)) = (key, val) {
						match parser.stack.last() {
							Some(Form::Tab(ref tab)) => tab.set(key, val)?,
							_ => panic!()
						}

						None
					} else {
						bail_at!(span(), "invalid clause in tab literal")
					}
				}
				Some(_) => bail_at!(span(), "unexpected ) token"),
				None => bail_at!(span(), "unexpected ) token")
			}
		}

		TokType::AccessOpen => {
			parser.stack.push(Form::Access(arr![ACCESS_SYM]));
			None
		}
		TokType::AccessClose => {
			match parser.stack.pop() {
				Some(Form::Access(access_arr)) => {
					access_arr.freeze();
					Some(Val::Arr(access_arr))
				}
				_ => bail_at!(span(), "unexpected ] token")
			}
		}

		TokType::StrOpen => {
			parser.stack.push(Form::Str(glsp::str(), StrEscape::NoEscape));
			None
		}
		TokType::StrClose => {
			match parser.stack.pop() {
				Some(Form::Str(st, _)) => {
					st.freeze();
					Some(Val::Str(st))
				}
				Some(Form::ResumedStr(arr, st, _)) => {
					if st.len() > 0 {
						st.freeze();
						arr.push(st)?;
					}

					arr.freeze();
					Some(Val::Arr(arr))
				}
				_ => panic!()
			}
		}
		TokType::StrPause => {
			match parser.stack.pop() {
				Some(Form::Str(st, _)) => {
					st.freeze();

					let arr = glsp::arr();
					arr.set_span(span());
					arr.push(TEMPLATE_STR_SYM)?;
					arr.push(st)?;
					parser.stack.push(Form::PausedStr(arr));
				}
				Some(Form::ResumedStr(arr, st, _)) => {
					st.freeze();

					arr.push(st)?;
					parser.stack.push(Form::PausedStr(arr));
				}
				_ => panic!()
			}

			None
		}
		TokType::StrResume => {
			match parser.stack.pop() {
				Some(Form::PausedStr(arr)) => {
					parser.stack.push(Form::ResumedStr(arr, glsp::str(), StrEscape::NoEscape));
					None
				}
				_ => bail_at!(span(), "unexpected }}")
			}
		}
		TokType::StrChars => {
			match parser.stack.last_mut() {
				Some(&mut Form::Str(ref st, ref mut escape)) | 
				Some(&mut Form::ResumedStr(_, ref st, ref mut escape)) => {
					parse_str_chars(st, tok.text, escape, span())?;
					None
				}
				_ => panic!()
			}
		}

		TokType::RawStrOpen => {
			parser.stack.push(Form::RawStr(glsp::str()));
			None
		}
		TokType::RawStrClose => {
			match parser.stack.pop() {
				Some(Form::RawStr(st)) => {
					st.freeze();
					Some(Val::Str(st))
				}
				_ => panic!()
			}
		}
		TokType::RawStrChars => {
			match parser.stack.last() {
				Some(&Form::RawStr(ref st)) => st.extend(tok.text.chars())?,
				_ => panic!()
			}
			None
		}

		TokType::Quote | TokType::Backquote | TokType::Unquote | 
		TokType::Splay | TokType::MethName | TokType::Atsign => {
			let sym = match tok.tok_type {
				TokType::Quote => QUOTE_SYM,
				TokType::Backquote => BACKQUOTE_SYM,
				TokType::Unquote => UNQUOTE_SYM,
				TokType::Splay => SPLAY_SYM,
				TokType::MethName => METH_NAME_SYM,
				TokType::Atsign => ATSIGN_SYM,
				_ => unreachable!()
			};

			parser.stack.push(Form::Abbrv(sym));
			None
		}
	};

	//if we haven't produced a value, we're finished. if we have, inspect the stack to decide
	//what we should do with it. if the stack is empty, we return our val to the caller as a 
	//toplevel form.
	while let Some(ref val) = parsed_val {
		let to_pop = match parser.stack.last_mut() {
			None => {
				return Ok(Some(val.clone()))
			}
			Some(&mut Form::Arr(ref arr)) | 
			Some(&mut Form::PausedStr(ref arr)) |
			Some(&mut Form::Access(ref arr))  => {
				arr.push(val)?;
				parsed_val = None;
				false
			}
			Some(&mut Form::Tab(_)) => bail_at!(span(), "invalid clause in tab literal"),
			Some(&mut Form::TabClause(ref mut key, ref mut value)) => {
				match (key.is_some(), value.is_some()) {
					(false, false) => *key = Some(val.clone()),
					(true, false) => *value = Some(val.clone()),
					(true, true) => bail_at!(span(), "invalid clause in tab literal"),
					(false, true) => panic!()
				}
				parsed_val = None;
				false
			}
			Some(&mut Form::DiscardNext) => {
				parsed_val = None;
				true
			}
			Some(&mut Form::Abbrv(abbrv_sym)) => {
				let arr = glsp::arr_with_capacity(2);
				arr.set_span(span());
				arr.push(abbrv_sym)?;
				arr.push(val)?;

				arr.freeze();
				parsed_val = Some(Val::Arr(arr));
				true
			}
			Some(&mut Form::Str(_, _)) => panic!(),
			Some(&mut Form::RawStr(_)) => panic!(),
			Some(&mut Form::ResumedStr(_, _, _)) => panic!(),
		};

		if to_pop {
			parser.stack.pop().unwrap();
		}
	}

	Ok(None)
}

fn parse_num_or_sym(text: &str) -> GResult<Val> {
	let val = if let Some(i) = parse_int(text) {
		Val::Int(i)
	} else if let Some(f) = parse_flo(text) {
		Val::Flo(f)
	} else {
		Val::Sym(glsp::sym(text)?)
	};

	Ok(val)
}

fn parse_int(mut text: &str) -> Option<i32> {
	//we closely follow rust's own grammar rules here, except for the _u32 etc. suffixes
	let sign = if text.starts_with('-') {
		text = &text[1..];
		-1
	} else {
		1
	};

	let radix: i32 = if text.starts_with("0x") {
		text = &text[2..];
		16
	} else if text.starts_with("0o") {
		text = &text[2..];
		8
	} else if text.starts_with("0b") {
		text = &text[2..];
		2
	} else if let Some(ch) = text.chars().next() {
		if ch.is_digit(10) {
			10
		} else {
			return None
		}
	} else {
		return None
	};

	let mut digit_count = 0;
	let mut val = 0i32;
	for ch in text.chars() {
		if let Some(digit) = ch.to_digit(radix as u32) {
			val = val.overflowing_mul(radix).0.overflowing_add(digit as i32).0;
			digit_count += 1;
		} else if ch == '_' {
			()
		} else {
			return None
		}
	}

	if digit_count == 0 {
		None
	} else {
		if sign == -1 {
			Some(val.overflowing_neg().0)
		} else {
			Some(val)
		}
	}
}

fn parse_flo(text: &str) -> Option<f32> {
	//f32::from_str is very close to the rust grammar for f32, except that it doesn't accept
	//an _f32 or _f64 suffix (good!), it accepts floats with no integer part (bad), it doesn't
	//accept underscores (bad), it accepts strings like 'inf' and '-NaN' (bad), and it accepts 
	//a leading '+' (bad).

	//note that the parser must call this fn for every single sym. for performance, we early-out
	//if the first char isn't '+', '-', 'n' or a decimal digit
	let first = text.chars().next().unwrap();
	if !matches!(first, '-' | '+' | 'n' | '0' ..= '9') {
		return None
	}

	match text {
		"+inf.0" => return Some(f32::INFINITY),
		"-inf.0" => return Some(f32::NEG_INFINITY),
		"nan.0" => return Some(f32::NAN),
		_ => ()
	}

	if text.contains("inf") || text.contains("NaN") || text.starts_with("-.") {
	   	return None
	}

	//strip out underscores. we iterate through bytes rather than chars, because in a utf-8
	//string, the byte b'_' can only represent the char '_'
	let mut bytes = SmallVec::<[u8; 128]>::with_capacity(text.len());
	bytes.extend(text.bytes().filter(|byte| *byte != b'_'));

	match f32::from_str(str::from_utf8(&bytes[..]).unwrap()) {
		Ok(f) => Some(f),
		Err(_) => None
	}
}

fn parse_ascii_char(text: &str) -> char {
	let mut ch = text.chars();
	let (slash, x, hi, lo) = (ch.next().unwrap(), ch.next().unwrap(), 
	                          ch.next().unwrap(), ch.next().unwrap());

	debug_assert!(slash == '\\' && x == 'x');
	let hi_u32 = hi.to_digit(8).unwrap() << 4;
	let lo_u32 = lo.to_digit(16).unwrap();

	debug_assert!(hi_u32 | lo_u32 <= 0x7f);
	char::from((hi_u32 | lo_u32) as u8)
}

fn parse_unicode_char(text: &str, span: Span) -> GResult<char> {
	debug_assert!(text.starts_with("\\u{"));

	if text.len() == 4 {
		bail_at!(span, "\\u{{}} is invalid syntax")
	}

	let mut value = 0u32;
	for (i, digit) in text[3..].chars().enumerate() {
		if digit == '}' {
			//not all 24-bit integers are valid chars
			match char::try_from(value) {
				Ok(ch) => return Ok(ch),
				Err(_) => bail_at!(span, "invalid unicode escape {}", text)
			}
		}

		if i >= 6 {
			bail_at!(span, "too many digits in {}", text)
		}

		value <<= 4;
		value |= digit.to_digit(16).unwrap();
	}

	bail_at!(span, "unterminated \\u{{ escape sequence")
}

fn parse_str_chars(
	dst: &Root<Str>, 
	mut text: &str, 
	escape: &mut StrEscape, 
	span: Span
) -> GResult<()> {

	if *escape == StrEscape::NewlineEscape {
		text = text.trim_start_matches(char_is_whitespace);
		if text.len() == 0 {
			return Ok(())
		}

		*escape = StrEscape::NoEscape;
	}

	//we iterate through the input, processing one character or escape sequence at a time
	while text.len() > 0 {
		if text == "\\" {
			bail_at!(span, "unexpected \\ character at end of str")
		}

		if text.starts_with("\\\n") || text.starts_with("\\\r\n") {
			text = (&text[1..]).trim_start_matches(char_is_whitespace);
			if text.len() == 0 {
				*escape = StrEscape::NewlineEscape;
				return Ok(())
			}

			continue
		}

		if text.starts_with('\\') {
			let mut escape_chars = text[1..].chars();
			let escaped_ch = match escape_chars.next().unwrap() {
				'\\' => '\\',
				'n' => '\n',
				'r' => '\r',
				't' => '\t',
				'0' => '\0',
				'"' => '"',
				'x' => {
					match (escape_chars.next(), escape_chars.next()) {
						(Some(hi), Some(lo)) if hi.is_digit(8) && lo.is_digit(16) => {
							let offs = 2 + hi.len_utf8() + lo.len_utf8();
							parse_ascii_char(&text[..offs])
						}
						_ => bail_at!(span, "invalid \\x escape sequence")
					}
				}
				'u' => {
					ensure_at!(span, escape_chars.next() == Some('{'), "invalid \\u escape");

					let mut offs = 3;
					loop {
						match escape_chars.next() {
							Some('}') => { 
								offs += 1; 
								break
							}
							Some(ch) if ch.is_digit(16) => offs += 1,
							_ => bail_at!(span, "invalid \\u escape")
						}
					}

					parse_unicode_char(&text[..offs], span)?
				}
				unknown_ch => bail_at!(span, "unrecognized escape character '{}'", unknown_ch)
			};

			dst.push(escaped_ch)?;
			text = escape_chars.as_str();
		} else {
			dst.push(text.chars().next().unwrap())?;

			if text.starts_with("{{") || text.starts_with("}}") {
				text = &text[2..];
			} else {
				text = &text[1..];
			}
		}
	}

	Ok(())
}

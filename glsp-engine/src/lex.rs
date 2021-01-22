use super::error::GResult;

/*

a streaming lexer. the only situation where we might receive streamed input is a repl, so whenever
the input is provided as a sequence of strs (rather than providing the entire input all at once),
the parser requires that each input str ends with a newline character, '\n'.

this is a big simplification for the lexer. it means that tokens (and str escape sequences other
than escaped newlines) can never span from one input str to the next, so we don't need a temporary
buffer for "incomplete" tokens. in fact, the lexer barely needs to maintain any state at all.

*/

pub(crate) struct Lexer {
    pub(crate) line_number: usize,
    pub(crate) str_status: StrStatus,
}

impl Lexer {
    pub(crate) fn new() -> Lexer {
        Lexer {
            line_number: 1,
            str_status: StrStatus::OutsideStr,
        }
    }

    pub(crate) fn lex<'a>(&mut self, text: &mut &'a str) -> GResult<Tok<'a>> {
        //we call a toplevel form to reduce the indentation level
        lex(self, text)
    }
}

pub(crate) struct Tok<'a> {
    pub(crate) tok_type: TokType,
    pub(crate) text: &'a str,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) enum TokType {
    Whitespace,
    NumOrSym,
    True,
    False,
    Nil,
    FormComment, // #;

    //we need to detect \space, \u{NN} and so on in the lexer, so we embed that information
    //in the token stream so that we're not repeating ourselves in the parser
    Char,
    SpaceChar,
    TabChar,
    NewlineChar,
    ReturnChar,
    NulChar,
    AsciiChar,
    UnicodeChar,

    ArrOpen,
    TabOpen,
    ArrClose,

    AccessOpen,
    AccessClose,

    StrOpen,   // "
    StrClose,  // "
    StrPause,  // {
    StrResume, // }
    StrChars,  //any characters which fall within a non-raw str, including escaped newlines

    RawStrOpen,  // r", r###", etc.
    RawStrClose, // ", "###, etc.
    RawStrChars, //raw strs cannot be paused/resumed

    Quote,
    Backquote,
    Unquote,
    Splay,
    MetName,
    Atsign,
}

#[derive(Copy, Clone, PartialEq)]
pub(crate) enum StrStatus {
    OutsideStr,
    InsideStr,
    InsideRawStr(usize),       //the number of #s which opened the raw str
    InsideBlockComment(usize), //the nesting level
}

fn lex<'a>(lexer: &mut Lexer, text: &mut &'a str) -> GResult<Tok<'a>> {
    assert!(text.len() > 0, "empty string passed to lex()");

    let mut chars = text.chars();
    let first = chars.next().unwrap();

    let (tok_type, len) = match lexer.str_status {
        StrStatus::InsideStr => {
            //we're inside a str. if we're faced with an unescaped '"', '{' or '}', process it.
            //otherwise, consume characters until we reach the end of the string, or encounter
            //one of those unescaped special characters. comments inside strs are ignored; this
            //is consistent with rust's behaviour.

            let second = chars.clone().next();
            match first {
                '"' => {
                    lexer.str_status = StrStatus::OutsideStr;
                    (TokType::StrClose, 1)
                }
                '{' if second != Some('{') => {
                    lexer.str_status = StrStatus::OutsideStr;
                    (TokType::StrPause, 1)
                }
                '}' if second != Some('}') => bail!("unexpected }} in str literal"),
                _ => {
                    let mut len = 0;
                    let mut input = &text[..];
                    while input.len() > 0 {
                        //skip any relevant escape sequences as they're encountered
                        if input.starts_with("\\\\")
                            || input.starts_with("\\\"")
                            || input.starts_with("{{")
                            || input.starts_with("}}")
                        {
                            len += 2;
                            input = &input[2..];
                        } else if input.starts_with('"')
                            || input.starts_with('{')
                            || input.starts_with('}')
                        {
                            break;
                        } else {
                            let ch = input.chars().next().unwrap();
                            let ch_len = ch.len_utf8();
                            len += ch_len;
                            input = &text[len..];
                        }
                    }
                    (TokType::StrChars, len)
                }
            }
        }

        StrStatus::InsideRawStr(num_hashes) => {
            //we're inside a raw str. if we're faced with an appropriately-sized "### delimiter,
            //process it. otherwise, consume any and all characters until we do see a delimiter

            fn starts_with_delimiter(st: &str, num_hashes: usize) -> bool {
                assert!(st.len() > 0);
                let mut chars = st.chars();
                let first = chars.next().unwrap();

                if first == '"' {
                    for _ in 0..num_hashes {
                        if chars.next() != Some('#') {
                            return false;
                        }
                    }

                    true
                } else {
                    false
                }
            }

            if starts_with_delimiter(text, num_hashes) {
                lexer.str_status = StrStatus::OutsideStr;
                (TokType::RawStrClose, num_hashes + 1)
            } else {
                let mut len = first.len_utf8();
                let mut st = chars.as_str();
                while let Some(ch) = chars.next() {
                    if starts_with_delimiter(st, num_hashes) {
                        break;
                    } else {
                        len += ch.len_utf8();
                    }

                    st = chars.as_str();
                }

                (TokType::RawStrChars, len)
            }
        }

        StrStatus::InsideBlockComment(mut nesting) => {
            //we're inside a block comment. consume any and all characters until we reach "|#" or
            //the end of the input, and return them as whitespace. keep track of any nested block
            //comments so that we don't end the comment too early.
            let mut len = 0;
            let mut input = *text;
            while input.len() > 0 {
                if input.starts_with("#|") {
                    len += 2;
                    input = &input[2..];
                    nesting += 1;
                    lexer.str_status = StrStatus::InsideBlockComment(nesting);
                } else if input.starts_with("|#") {
                    len += 2;
                    input = &input[2..];
                    if nesting == 0 {
                        lexer.str_status = StrStatus::OutsideStr;
                        break;
                    } else {
                        nesting -= 1;
                        lexer.str_status = StrStatus::InsideBlockComment(nesting);
                    }
                } else {
                    let mut chars = input.chars();
                    len += chars.next().unwrap().len_utf8();
                    input = chars.as_str();
                }
            }

            (TokType::Whitespace, len)
        }

        StrStatus::OutsideStr => {
            let second = chars.clone().next(); //peek at the iterator without advancing it

            match (first, second) {
                (ch, _) if char_is_whitespace(ch) => {
                    //the input starts with whitespace. keep going until we encounter a
                    //non-whitespace character
                    let mut len = 1;
                    while let Some(ch) = chars.next() {
                        if !char_is_whitespace(ch) {
                            break;
                        }
                        len += ch.len_utf8();
                    }

                    (TokType::Whitespace, len)
                }

                (';', _) => {
                    //we're at the start of a line comment. consume any and all characters until
                    //we encounter a newline, and report them as whitespace.
                    let mut len = 1;
                    while let Some(ch) = chars.next() {
                        len += ch.len_utf8();
                        if ch == '\n' {
                            break;
                        }
                    }

                    (TokType::Whitespace, len)
                }

                ('\\', _) => {
                    //this is the start of a character escape. look ahead to detect whether it's
                    //a unicode character, a single character, or a named character like \newline
                    let rest = chars.as_str();

                    if rest.starts_with("space") {
                        (TokType::SpaceChar, 6)
                    } else if rest.starts_with("tab") {
                        (TokType::TabChar, 4)
                    } else if rest.starts_with("newline") {
                        (TokType::NewlineChar, 8)
                    } else if rest.starts_with("return") {
                        (TokType::ReturnChar, 7)
                    } else if rest.starts_with("nul") {
                        (TokType::NulChar, 4)
                    } else if rest.starts_with("u{") {
                        chars.next().unwrap();
                        chars.next().unwrap();
                        let mut digits = 0;
                        loop {
                            match chars.next() {
                                Some('}') => break,
                                Some(ch) if ch.is_digit(16) => digits += 1,
                                _ => bail!("malformed unicode char escape"),
                            }
                        }

                        (TokType::UnicodeChar, digits + 4)
                    } else {
                        if let Some(second) = second {
                            chars.next().unwrap();
                            match (second, chars.next(), chars.next()) {
                                ('x', Some(hi), Some(lo)) if hi.is_digit(8) && lo.is_digit(16) => {
                                    (TokType::AsciiChar, 4)
                                }
                                _ => (TokType::Char, 1 + second.len_utf8()),
                            }
                        } else {
                            bail!("input ends with \\ character")
                        }
                    }
                }

                //#t, #f, #n, #_, #|
                ('#', Some('t')) => (TokType::True, 2),
                ('#', Some('f')) => (TokType::False, 2),
                ('#', Some('n')) => (TokType::Nil, 2),
                ('#', Some(';')) => (TokType::FormComment, 2),
                ('#', Some('|')) => {
                    lexer.str_status = StrStatus::InsideBlockComment(0);
                    (TokType::Whitespace, 2)
                }

                //arr/tab/access delimiters
                ('(', _) => (TokType::ArrOpen, 1),
                ('#', Some('(')) => (TokType::TabOpen, 2),
                (')', _) => (TokType::ArrClose, 1),

                ('[', _) => (TokType::AccessOpen, 1),
                (']', _) => (TokType::AccessClose, 1),

                //str delimiters
                ('"', _) => {
                    lexer.str_status = StrStatus::InsideStr;
                    (TokType::StrOpen, 1)
                }
                ('}', _) => {
                    lexer.str_status = StrStatus::InsideStr;
                    (TokType::StrResume, 1)
                }
                ('{', _) => bail!("unexpected {{"),
                ('r', Some('"')) | ('r', Some('#')) => {
                    let mut num_hashes = 0;
                    loop {
                        match chars.next() {
                            Some('"') => break,
                            Some('#') => num_hashes += 1,
                            _ => bail!("malformed raw str"),
                        }
                    }

                    lexer.str_status = StrStatus::InsideRawStr(num_hashes);
                    (TokType::RawStrOpen, 2 + num_hashes)
                }

                //abbreviations
                ('\'', _) => (TokType::Quote, 1),
                ('`', _) => (TokType::Backquote, 1),
                ('~', _) => (TokType::Unquote, 1),
                ('.', Some('.')) => (TokType::Splay, 2),
                ('.', _) => (TokType::MetName, 1),
                ('@', _) => (TokType::Atsign, 1),

                (first, _) if is_valid_sym_char(first) => {
                    //lexically speaking, all nums are also valid syms. if something doesn't parse
                    //as a num, then we automatically parse it as a sym instead. to avoid
                    //duplication of effort, we don't try to differentiate nums from syms here.
                    let mut len = first.len_utf8();
                    while let Some(ch) = chars.next() {
                        if ch == '#' {
                            len += 1;
                            break;
                        }

                        if !is_valid_sym_char(ch) {
                            break;
                        }

                        len += ch.len_utf8();
                    }

                    (TokType::NumOrSym, len)
                }

                _ => bail!("unexpected character '{}'", first),
            }
        }
    };

    //update the Lexer. we advance the line_number by scanning the emitted lexeme for '\n' bytes.
    //in utf-8, byte values below 0x80 always represent an entire char.
    let (tok_text, rest) = text.split_at(len);
    lexer.line_number += tok_text.bytes().filter(|byte| *byte == b'\n').count();

    //update the input str
    *text = rest;

    //return
    Ok(Tok {
        tok_type,
        text: tok_text,
    })
}

pub(crate) fn is_valid_sym_char(ch: char) -> bool {
    match ch {
        'A'..='Z'
        | 'a'..='z'
        | '0'..='9'
        | '!'
        | '$'
        | '%'
        | '&'
        | '*'
        | '+'
        | '-'
        | '.'
        | '/'
        | ':'
        | '<'
        | '='
        | '>'
        | '?'
        | '^'
        | '_'
        | '~' => true,
        _ => false,
    }
}

//we use the same definition of whitespace as the rust reference, with the addition of ','
pub(crate) fn char_is_whitespace(ch: char) -> bool {
    match ch {
        '\t' | '\n' | '\u{0B}' | '\u{0C}' | '\r' | ' ' | ',' | '\u{85}' | '\u{200E}'
        | '\u{200F}' | '\u{2028}' | '\u{2029}' => true,
        _ => false,
    }
}

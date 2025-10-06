use std::fmt;

use crate::tokens::{ControlWord, Property, Token};
use crate::utils::StrUtils;
use crate::{recursive_tokenize, recursive_tokenize_with_init};

#[derive(Debug, Clone)]
pub enum LexerError {
    Error(String),
    InvalidUnicode(String),
    InvalidLastChar,
}

impl std::error::Error for LexerError {}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let _ = write!(f, "[RTF Lexer] : ");
        let _ = match self {
            LexerError::InvalidLastChar => write!(f, "Invalid last char, should be '}}'"),
            LexerError::InvalidUnicode(uc) => write!(f, "Invalid unicode : {uc}"),
            LexerError::Error(msg) => write!(f, "{}", msg),
        };
        return Ok(());
    }
}

impl From<std::str::Utf8Error> for LexerError {
    fn from(value: std::str::Utf8Error) -> Self {
        return LexerError::Error(value.to_string());
    }
}

impl From<std::num::ParseIntError> for LexerError {
    fn from(value: std::num::ParseIntError) -> Self {
        return LexerError::Error(value.to_string());
    }
}

pub struct Lexer;

impl Lexer {
    pub fn scan(src: &str) -> Result<Vec<Token>, LexerError> {
        let src = src.trim(); // Sanitize src : Trim the leading whitespaces
    
        let mut tokens: Vec<Token> = Vec::new();
        let mut slice_start_index = 0;
        let mut previous_char = ' ';
    
        // Working with safe UTF-8 indices
        for (current_index, c) in src.char_indices() {
            match c {
                // TODO: Handle char over code 127 for escaped chars
                // Handle Escaped chars : "\" + any charcode below 127
                '{' | '}' | '\\' | '\n' if previous_char == '\\' => {}
                '{' | '}' | '\\' | '\n' => {
                    if slice_start_index < current_index {
                        // Extract a valid UTF-8 slice
                        let slice = &src[slice_start_index..current_index];
                        let slice_tokens = Self::tokenize(slice)?;
                        tokens.extend_from_slice(&slice_tokens);
                        slice_start_index = current_index;
                    }
                }
                _ => {}
            }
            previous_char = c;
        }
    
        // Handling the last token
        if slice_start_index < src.len() {
            let slice = &src[slice_start_index..];
            if slice != "}" {
                return Err(LexerError::InvalidLastChar);
            }
            tokens.push(Token::ClosingBracket);
        }
    
        Ok(tokens)
    }

    /// Get a string slice cut by the scanner and return the corresponding token(s)
    fn tokenize(slice: &str) -> Result<Vec<Token>, LexerError> {
        let mut starting_chars = slice.trim_matches(' ').chars().take(2);
        return match (starting_chars.next(), starting_chars.next()) {
            // If it starts with \ : escaped text or control word
            (Some('\\'), Some(c)) => match c {
                '{' | '}' | '\\' => {
                    // Handle escaped chars
                    let tail = slice.get(1..).unwrap_or("");
                    Ok(vec![Token::PlainText(tail)]) // Escaped single char -> plain text
                }
                '\'' => {
                    // Escaped unicode hex value: \'f0
                    let tail = slice.get(1..).unwrap_or("");
                    if tail.len() < 2 {
                        return Err(LexerError::InvalidUnicode(tail.into()));
                    }
                    let byte = u8::from_str_radix(&tail[1..3], 16)?;
                    let mut ret = vec![Token::ControlSymbol((ControlWord::Unicode, Property::Value(byte as i32)))];
                    recursive_tokenize!(&tail[3..], ret);
                    Ok(ret)
                }
                '\n' => {
                    // CRLF
                    let mut ret = vec![Token::CRLF];
                    if let Some(tail) = slice.get(2..) {
                        recursive_tokenize!(tail, ret);
                    }
                    Ok(ret)
                }
                'a'..='z' => {
                    // Identify control word
                    let (mut ident, tail) = slice.split_first_whitespace();
                    ident = if ident.ends_with(';') { &ident[..ident.len() - 1] } else { ident };
    
                    // Try parse control word, fallback for symbols like "-" in \pntext
                    let control_word = match ControlWord::from(ident) {
                        Ok(cw) => cw,
                        Err(_) => {
                            // Treat as plain text if it cannot be parsed as control word
                            return Ok(vec![Token::PlainText(slice)]);
                        }
                    };
    
                    let mut ret = vec![Token::ControlSymbol(control_word)];
                    recursive_tokenize!(tail, ret);
    
                    // Handle special case for \u1234 and trailing spaces
                    if control_word.0 == ControlWord::Unicode && !tail.trim().is_empty() && tail.trim().chars().all(|ch| ch.is_whitespace()) {
                        ret.push(Token::PlainText(tail));
                    }
    
                    Ok(ret)
                }
                '*' => Ok(vec![Token::IgnorableDestination]),
                _ => Ok(vec![]),
            },
            (Some('\n'), Some(_)) => recursive_tokenize!(&slice[1..]), // Ignore CRLF if not escaped
            // Handle brackets
            (Some('{'), None) => Ok(vec![Token::OpeningBracket]),
            (Some('}'), None) => Ok(vec![Token::ClosingBracket]),
            (Some('{'), Some(_)) => recursive_tokenize_with_init!(Token::OpeningBracket, &slice[1..]),
            (Some('}'), Some(_)) => recursive_tokenize_with_init!(Token::ClosingBracket, &slice[1..]),
            (None, None) => Err(LexerError::Error(format!("Empty token {}", &slice))),
            // Else, it's plain text
            _ => {
                let text = slice.trim();
                if text.is_empty() {
                    Ok(vec![])
                } else {
                    Ok(vec![Token::PlainText(slice)])
                }
            }
        };
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use crate::lexer::Lexer;
    use crate::tokens::ControlWord::{Ansi, Bold, ColorBlue, ColorNumber, ColorRed, FontNumber, FontSize, FontTable, Italic, Par, Pard, Rtf, Underline, Unicode, Unknown};
    use crate::tokens::Property::*;
    use crate::tokens::Token::*;
    use crate::tokens::{ControlWord, Property};

    #[test]
    fn simple_tokenize_test() {
        let tokens = Lexer::tokenize(r"\b Words in bold").unwrap();
        assert_eq!(tokens, vec![ControlSymbol((Bold, None)), PlainText("Words in bold"),]);
    }

    #[test]
    fn scan_entire_file_test() {
        let tokens = Lexer::scan(r#"{ \rtf1\ansi{\fonttbl\f0\fswiss Helvetica;}\f0\pard Voici du texte en {\b gras}.\par }"#);
        assert_eq!(
            tokens.unwrap(),
            vec![
                OpeningBracket,
                ControlSymbol((Rtf, Value(1))),
                ControlSymbol((Ansi, None)),
                OpeningBracket,
                ControlSymbol((FontTable, None)),
                ControlSymbol((FontNumber, Value(0))),
                ControlSymbol((Unknown("\\fswiss"), None)),
                PlainText("Helvetica;"),
                ClosingBracket,
                ControlSymbol((FontNumber, Value(0))),
                ControlSymbol((Pard, None)),
                PlainText("Voici du texte en "),
                OpeningBracket,
                ControlSymbol((Bold, None)),
                PlainText("gras"),
                ClosingBracket,
                PlainText("."),
                ControlSymbol((Par, None)),
                ClosingBracket,
            ]
        );
    }

    #[test]
    fn scan_escaped_text() {
        let tokens = Lexer::scan(
            r#"\f0\fs24 \cf0 test de code \
if (a == b) \{\
    test();\
\} else \{\
    return;\
\}}"#,
        );
        assert_eq!(
            tokens.unwrap(),
            vec![
                ControlSymbol((FontNumber, Value(0))),
                ControlSymbol((FontSize, Value(24))),
                ControlSymbol((ColorNumber, Value(0))),
                PlainText("test de code "),
                CRLF,
                PlainText("if (a == b) "),
                PlainText("{"),
                CRLF,
                PlainText("    test();"),
                CRLF,
                PlainText("} else "),
                PlainText("{"),
                CRLF,
                PlainText("    return;"),
                CRLF,
                PlainText("}"),
                ClosingBracket
            ],
        );
    }

    #[test]
    fn scan_ignorable_destination() {
        let text = r"{\*\expandedcolortbl;;}";
        let tokens = Lexer::scan(text);
        assert_eq!(
            tokens.unwrap(),
            vec![OpeningBracket, IgnorableDestination, ControlSymbol((Unknown(r"\expandedcolortbl;"), None)), ClosingBracket,]
        )
    }

    #[test]
    fn should_parse_control_symbol_ending_semicolon() {
        let text = r"{\red255\blue255;}";
        let tokens = Lexer::scan(text);
        assert_eq!(
            tokens.unwrap(),
            vec![OpeningBracket, ControlSymbol((ColorRed, Value(255))), ControlSymbol((ColorBlue, Value(255))), ClosingBracket]
        );
    }

    #[test]
    fn lex_with_leading_whitespaces() {
        // Try to parse without error
        let rtf_content = "\t {\\rtf1 }\n "; // Not raw str for the whitespace to be trimed
        let tokens = Lexer::scan(rtf_content).unwrap();
        assert_eq!(tokens, vec![OpeningBracket, ControlSymbol((Rtf, Value(1))), ClosingBracket]);
    }

    #[test]
    fn should_parse_line_return() {
        // From Microsoft's reference: "A carriage return (character value 13) or linefeed (character value 10)
        // will be treated as a \par control if the character is preceded by a backslash.
        // You must include the backslash; otherwise, RTF ignores the control word."
        let text = r#"{\partightenfactor0

\fs24 \cf0 Font size 12,
\f0\b bold text. \ul Underline,bold text.\
 }"#;
        let tokens = Lexer::scan(text).unwrap();
        assert_eq!(
            tokens,
            [
                OpeningBracket,
                ControlSymbol((Unknown("\\partightenfactor"), Value(0))),
                ControlSymbol((FontSize, Value(24))),
                ControlSymbol((ColorNumber, Value(0))),
                PlainText("Font size 12,"),
                ControlSymbol((FontNumber, Value(0))),
                ControlSymbol((Bold, None)),
                PlainText("bold text. "),
                ControlSymbol((Underline, None)),
                PlainText("Underline,bold text."),
                CRLF,
                ClosingBracket
            ]
        );
    }

    #[test]
    fn space_after_control_word() {
        let text = r"{in{\i cred}ible}";
        let tokens = Lexer::scan(text).unwrap();
        assert_eq!(
            tokens,
            [OpeningBracket, PlainText("in"), OpeningBracket, ControlSymbol((Italic, None)), PlainText("cred"), ClosingBracket, PlainText("ible"), ClosingBracket,]
        )
    }

    #[test]
    fn should_handle_escaped_char() {
        let rtf = r"{je suis une b\'eate}"; // ê = 0xea = 234
        let tokens = Lexer::scan(rtf).unwrap();
        assert_eq!(
            tokens,
            [OpeningBracket, PlainText("je suis une b"), ControlSymbol((Unicode, Value(234))), PlainText("te"), ClosingBracket,]
        );
    }
}

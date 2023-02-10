use std::iter::FromIterator;

use serde::Serialize;

use crate::errors::*;
pub type SrcLocation = (usize, usize);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize)]
pub enum TokenType {
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    Comma,
    Bang,
    LeftAngle,
    RightAngle,
    DoubleLeftAngle,
    DoubleRightAngle,

    Minus,
    Plus,
    Asterix,
    ForwardSlash,
    Percent,

    PlusEqual,
    MinusEqual,
    DivEqual,
    ModEqual,
    MulEqual,
    LessEqual,
    GreaterEqual,
    NotEqual,

    Equal,
    Colon,
    ColonEqual,
    DoubleColon,
    DoubleEqual,
    DoublePlus,
    DoubleMinus,
    DoubleAmpersand,
    DoublePipe,
    SemiColon,
    Ampersand,
    Hat,
    Dot,
    Ident,

    CompilerFunctionImport,

    KeywordIf,
    KeywordIn,
    KeywordGoto,
    KeywordFor,
    KeywordWhile,
    KeywordContinue,
    KeywordBreak,
    KeywordReturn,
    KeywordTrue,
    KeywordFalse,
    KeywordEnum,
    KeywordElse,
    KeywordBool,
    KeywordStruct,
    KeywordVoid,

    KeywordSignedInt,
    KeywordSignedInt8,
    KeywordSignedInt16,
    KeywordSignedInt32,
    KeywordSignedInt64,
    KeywordSignedInt128,

    KeywordUint,
    KeywordUint8,
    KeywordUint16,
    KeywordUint32,
    KeywordUint64,
    KeywordUint128,

    KeywordString,
    KeywordFloat32,
    KeywordFloat64,
    KeywordChar,

    Char,
    UnsignedInt,
    Float,
    StringLiteral,
}

impl TokenType {
    fn from_str(s: &str) -> Self {
        match s {
            "if" => Self::KeywordIf,
            "for" => Self::KeywordFor,
            "true" => Self::KeywordTrue,
            "false" => Self::KeywordFalse,

            "goto" => Self::KeywordGoto,
            "continue" => Self::KeywordContinue,
            "break" => Self::KeywordBreak,
            "return" => Self::KeywordReturn,
            "enum" => Self::KeywordEnum,
            "else" => Self::KeywordElse,
            "bool" => Self::KeywordBool,
            "struct" => Self::KeywordStruct,
            "void" => Self::KeywordVoid,

            "s8" => Self::KeywordSignedInt8,
            "s16" => Self::KeywordSignedInt16,
            "s32" => Self::KeywordSignedInt32,
            "s64" => Self::KeywordSignedInt64,
            "s128" => Self::KeywordSignedInt128,
            "int" => Self::KeywordSignedInt,

            "u8" => Self::KeywordUint8,
            "u16" => Self::KeywordUint16,
            "u32" => Self::KeywordUint32,
            "u64" => Self::KeywordUint64,
            "u128" => Self::KeywordUint128,
            "uint" => Self::KeywordUint,

            "string" => Self::KeywordString,
            "f64" => Self::KeywordFloat32,
            "f32" => Self::KeywordFloat64,
            "char" => Self::KeywordChar,
            _ => Self::Ident,
        }
    }
}
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Token {
    pub ty: TokenType,
    pub loc: SrcLocation,
    pub line: usize,
    pub col: usize,
}

impl Token {
    pub fn new(ty: TokenType, loc: (usize, usize), line: usize, col: usize) -> Self {
        Self {
            ty,
            loc: (loc.0, loc.1),
            line,
            col,
        }
    }
}

pub struct Tokenizer {
    filename: String,
    src: Vec<char>,
    cursor: usize,
    peek_cursor: usize,
    line: usize,
    col: usize,
}

fn is_letter(ch: char) -> bool {
    return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_';
}

fn is_whitespace(c: char) -> bool {
    return c == ' ' || c == '\n' || c == '\r' || c == '\t';
}

impl Tokenizer {
    pub fn new(filename: String, src: &str) -> Self {
        Self {
            filename,
            src: src.chars().collect(),
            cursor: 0,
            peek_cursor: 0,
            line: 0,
            col: 0,
        }
    }
    fn forward(&mut self) {
        self.peek_cursor += 1;
        self.cursor = self.peek_cursor;
    }
    fn current_char(&self) -> char {
        if self.peek_cursor >= self.src.len() {
            return 0 as char;
        } else {
            return self.src[self.peek_cursor];
        }
    }
    fn new_token(&mut self, typ: TokenType) -> Token {
        self.cursor = self.peek_cursor;
        self.peek_cursor += 1;
        return Token {
            ty: typ,
            loc: (self.cursor, self.peek_cursor - 1),
            line: 0,
            col: 0,
        };
    }
    fn ident_or_keyword(&mut self) -> Token {
        loop {
            self.peek_cursor += 1;
            let current_char = self.current_char();
            if is_whitespace(current_char) || current_char == 0 as char {
                break;
            }
        }

        let literal: String = self.src[self.cursor..self.peek_cursor].iter().collect();
        return self.new_token(TokenType::from_str(&literal));
    }
    pub fn peek(&mut self) -> Option<Token> {
        let cursor = self.cursor;
        let peek = self.peek_cursor;
        let peek_token = self.next();

        self.cursor = cursor;
        self.peek_cursor = peek;

        return peek_token;
        
    }
    pub fn next(&mut self) -> Option<Token> {
        match self.current_char() {
            '(' => Some(self.new_token(TokenType::OpenParen)),
            ')' => Some(self.new_token(TokenType::CloseParen)),
            '[' => Some(self.new_token(TokenType::OpenBracket)),
            ']' => Some(self.new_token(TokenType::CloseBracket)),
            '{' => Some(self.new_token(TokenType::OpenBrace)),
            '}' => Some(self.new_token(TokenType::CloseBrace)),

            '*' => Some(self.new_token(TokenType::Asterix)),
            '+' => Some(self.new_token(TokenType::Plus)),
            '%' => Some(self.new_token(TokenType::Percent)),
            '-' => Some(self.new_token(TokenType::Minus)),
            '/' => {
                self.peek_cursor += 1;
                if self.current_char() == '/' {
                    // comment line
                    loop {
                        self.peek_cursor += 1;

                        if self.current_char() == '\n' {
                            break;
                        }
                    }
                    self.forward();
                    return self.next();
                } else {
                    self.peek_cursor-=1;
                    Some(self.new_token(TokenType::ForwardSlash))
                }
            }

            '!' => Some(self.new_token(TokenType::Bang)),
            '^' => Some(self.new_token(TokenType::Hat)),

            '&' => {
                self.peek_cursor += 1;
                if self.current_char() == '&' {
                    Some(self.new_token(TokenType::DoubleAmpersand))
                } else {
                    self.peek_cursor -= 1;
                    Some(self.new_token(TokenType::Ampersand))
                }
            }
            ':' => {
                self.peek_cursor += 1;
                if self.current_char() == ':' {
                    Some(self.new_token(TokenType::DoubleColon))
                } else if self.current_char() == '=' {
                    Some(self.new_token(TokenType::ColonEqual))
                } else {
                    self.peek_cursor -= 1;
                    Some(self.new_token(TokenType::Colon))
                }
            }
            '<' => {
                self.peek_cursor += 1;

                if self.current_char() == '=' {
                    Some(self.new_token(TokenType::LessEqual))
                } else {
                    self.peek_cursor -= 1;
                    Some(self.new_token(TokenType::LeftAngle))
                }
            }
            '>' => {
                self.peek_cursor += 1;
                if self.current_char() == '=' {
                    Some(self.new_token(TokenType::GreaterEqual))
                } else {
                    self.peek_cursor -= 1;
                    Some(self.new_token(TokenType::RightAngle))
                }
            }
            '=' => {
                self.peek_cursor += 1;
                if self.current_char() == '=' {
                    Some(self.new_token(TokenType::DoubleEqual))
                } else {
                    self.peek_cursor -= 1;
                    Some(self.new_token(TokenType::Equal))
                }
            }
            ';' => Some(self.new_token(TokenType::SemiColon)),
            ',' => Some(self.new_token(TokenType::Comma)),
            '0' ..= '9' => {
                self.peek_cursor += 1;
                loop {
                    let current_char = self.current_char();
                    if is_whitespace(current_char) || current_char == 0 as char {
                        break;
                    }
                }
                return Some(self.new_token(TokenType::UnsignedInt));

            }
            ' ' | '\t' | '\r' | '\n' => {
                self.cursor+=1;
                self.peek_cursor+=1;
                self.next()
            }
            '\0' => None,
            _ => {
                if is_letter(self.current_char()) {
                    Some(self.ident_or_keyword())
                } else {
                    None
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::{TokenType, Tokenizer};

    #[test]
    fn test_basic_symbols() {
        let mut tokenizer = Tokenizer::new(String::from("some.file"), "[](){}+-*/%^,;");
        for sym in [
            TokenType::OpenBracket,
            TokenType::CloseBracket,
            TokenType::OpenParen,
            TokenType::CloseParen,
            TokenType::OpenBrace,
            TokenType::CloseBrace,
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Asterix,
            TokenType::ForwardSlash,
            TokenType::Percent,
            TokenType::Hat,
            TokenType::Comma,
            TokenType::SemiColon,
        ] {
            let token = tokenizer.next();
            assert!(token.is_some());
            let typ = token.unwrap().ty;
            assert_eq!(typ, sym, "Expected: '{:?}' Got: '{:?}'", typ, sym);
        }
    }

    #[test]
    fn test_numbers() {}

    #[test]
    fn test_keywords() {
        let mut hm: HashMap<&'static str, TokenType> = HashMap::new();
        hm.insert("if",  TokenType::KeywordIf);
        hm.insert("for",  TokenType::KeywordFor);
        hm.insert("true",  TokenType::KeywordTrue);
        hm.insert("false",  TokenType::KeywordFalse);
        hm.insert("goto",  TokenType::KeywordGoto);
        hm.insert("continue",  TokenType::KeywordContinue);
        hm.insert("break",  TokenType::KeywordBreak);
        hm.insert("return",  TokenType::KeywordReturn);
        hm.insert("enum",  TokenType::KeywordEnum);
        hm.insert("else",  TokenType::KeywordElse);
        hm.insert("bool",  TokenType::KeywordBool);
        hm.insert("struct",  TokenType::KeywordStruct);
        hm.insert("void",  TokenType::KeywordVoid);
        hm.insert("s8",  TokenType::KeywordSignedInt8);
        hm.insert("s16",  TokenType::KeywordSignedInt16);
        hm.insert("s32",  TokenType::KeywordSignedInt32);
        hm.insert("s64",  TokenType::KeywordSignedInt64);
        hm.insert("s128",  TokenType::KeywordSignedInt128);
        hm.insert("int",  TokenType::KeywordSignedInt);
        hm.insert("u8",  TokenType::KeywordUint8);
        hm.insert("u16",  TokenType::KeywordUint16);
        hm.insert("u32",  TokenType::KeywordUint32);
        hm.insert("u64",  TokenType::KeywordUint64);
        hm.insert("u128",  TokenType::KeywordUint128);
        hm.insert("uint",  TokenType::KeywordUint);
        hm.insert("string",  TokenType::KeywordString);
        hm.insert("f64",  TokenType::KeywordFloat32);
        hm.insert("f32",  TokenType::KeywordFloat64);
        hm.insert("char",  TokenType::KeywordChar);

        for (keyword, typ) in hm {
            let mut tokenizer = Tokenizer::new(String::from("some.file"), keyword);
            let token = tokenizer.next();
            assert!(token.is_some());
            assert_eq!(token.unwrap().ty, typ);
        }
    }
}

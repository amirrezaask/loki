use std::iter::FromIterator;

use serde::Serialize;

/*TODOS
- handle char literals
- handle string literals
- handle keywords
- handle identifiers
- line and column
 */

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
    Sharp,
    Dollor,
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
    AmpersandEqual,
    Hat,
    Atsign,
    Dot,
    SingleQuote,
    DoubleQuote,
    BackSlash,
    Pipe,
    PipeEqual,
    Ident,

    LoadDirective,
    HostDirective,
    CompilerFlagDirective,
    ForeignDirective,
    NoCodeGenDirective,
    CVarArgsDirective,
    CString,

    KeywordIf,
    KeywordIn,
    KeywordSwitch,
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
    KeywordUnion,
    KeywordVoid,

    KeywordInt,
    KeywordInt8,
    KeywordInt16,
    KeywordInt32,
    KeywordInt64,
    KeywordInt128,

    KeywordUint,
    KeywordUint8,
    KeywordUint16,
    KeywordUint32,
    KeywordUint64,
    KeywordUint128,

    KeywordIntPtr,

    KeywordString,
    KeywordFloat32,
    KeywordFloat64,
    KeywordChar,
    KeywordAs,

    Char,
    UnsignedInt,
    Float,
    StringLiteral,
}

impl TokenType {
    fn from_str(s: &str) -> Self {
        match s {
            "#load" => Self::LoadDirective,
            "#host" => Self::HostDirective,
            "#compiler_flag" => Self::CompilerFlagDirective,
            "#foreign" => Self::ForeignDirective,
            "#no_codegen" => Self::NoCodeGenDirective,
            "intptr" => Self::KeywordIntPtr,

            "#c_varargs" => Self::CVarArgsDirective,
            "#c_string" => Self::CString,

            "as" => Self::KeywordAs,
            "in" => Self::KeywordIn,
            "if" => Self::KeywordIf,
            "for" => Self::KeywordFor,
            "true" => Self::KeywordTrue,
            "false" => Self::KeywordFalse,
            "switch" => Self::KeywordSwitch,
            "goto" => Self::KeywordGoto,
            "while" => Self::KeywordWhile,
            "continue" => Self::KeywordContinue,
            "break" => Self::KeywordBreak,
            "return" => Self::KeywordReturn,
            "enum" => Self::KeywordEnum,
            "else" => Self::KeywordElse,
            "bool" => Self::KeywordBool,
            "struct" => Self::KeywordStruct,
            "union" => Self::KeywordUnion,
            "void" => Self::KeywordVoid,

            "s8" => Self::KeywordInt8,
            "s16" => Self::KeywordInt16,
            "s32" => Self::KeywordInt32,
            "s64" => Self::KeywordInt64,
            "s128" => Self::KeywordInt128,
            "int" => Self::KeywordInt,

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
    cur: usize,
    peek_cur: usize,
    reached_eof: bool,
    line: usize,
    col: usize,
}

impl Tokenizer {
    fn new_token(&mut self, t: TokenType) -> Token {
        let tok = Token {
            ty: t,
            loc: (self.cur, self.peek_cur),
            line: self.line,
            col: self.col,
        };
        self.cur = self.peek_cur;

        return tok;
    }
    pub fn new(filename: String, src: &str) -> Self {
        Tokenizer {
            filename,
            reached_eof: false,
            src: src.chars().collect(),
            cur: 0,
            peek_cur: 0,
            line: 1,
            col: 0,
        }
    }

    fn eof(&self) -> bool {
        self.peek_cur >= self.src.len()
    }

    fn forward_char(&mut self) {
        self.peek_cur += 1;
    }

    fn backward_char(&mut self) {
        self.peek_cur -= 1;
    }

    fn current_char(&self) -> char {
        if self.eof() {
            return 0 as char;
        }
        self.src[self.peek_cur]
    }

    pub fn next(&mut self) -> Option<Token> {
        if self.eof() {
            return None;
        }

        match self.current_char() {
            '(' => {
                self.forward_char();
                Some(self.new_token(TokenType::OpenParen))
            }
            ')' => {
                self.forward_char();
                Some(self.new_token(TokenType::CloseParen))
            }
            '[' => {
                self.forward_char();
                Some(self.new_token(TokenType::OpenBracket))
            }
            ']' => {
                self.forward_char();
                Some(self.new_token(TokenType::CloseBracket))
            }
            '{' => {
                self.forward_char();
                Some(self.new_token(TokenType::OpenBrace))
            }
            '}' => {
                self.forward_char();
                Some(self.new_token(TokenType::CloseBrace))
            }

            '!' => {
                self.forward_char();
                Some(self.new_token(TokenType::Bang))
            }
            '<' => {
                self.forward_char();
                match self.current_char() {
                    '=' => {
                        // <=
                        self.forward_char();
                        return Some(self.new_token(TokenType::LessEqual));
                    }
                    '<' => {
                        // <<
                        self.forward_char();
                        return Some(self.new_token(TokenType::DoubleLeftAngle));
                    }
                    _ => {
                        return Some(self.new_token(TokenType::LeftAngle));
                    }
                }
            }
            '>' => {
                self.forward_char();
                match self.current_char() {
                    '=' => {
                        // >=
                        self.forward_char();
                        return Some(self.new_token(TokenType::GreaterEqual));
                    }
                    '>' => {
                        // >>
                        self.forward_char();
                        return Some(self.new_token(TokenType::DoubleRightAngle));
                    }
                    _ => {
                        return Some(self.new_token(TokenType::RightAngle));
                    }
                }
            }
            '\'' => {
                self.forward_char();
                Some(self.new_token(TokenType::SingleQuote))
            }
            '&' => {
                self.forward_char();
                match self.current_char() {
                    '=' => {
                        // &=
                        self.forward_char();
                        return Some(self.new_token(TokenType::AmpersandEqual));
                    }
                    '&' => {
                        // &&
                        self.forward_char();
                        return Some(self.new_token(TokenType::DoubleAmpersand));
                    }
                    _ => {
                        return Some(self.new_token(TokenType::Ampersand));
                    }
                }
            }
            '|' => {
                self.forward_char();
                match self.current_char() {
                    '=' => {
                        // |=
                        self.forward_char();
                        return Some(self.new_token(TokenType::PipeEqual));
                    }
                    '|' => {
                        // ||
                        self.forward_char();
                        return Some(self.new_token(TokenType::DoublePipe));
                    }
                    _ => {
                        return Some(self.new_token(TokenType::Pipe));
                    }
                }
            }
            '=' => {
                self.forward_char();
                match self.current_char() {
                    '=' => {
                        // ==
                        self.forward_char();
                        return Some(self.new_token(TokenType::DoubleEqual));
                    }
                    _ => {
                        return Some(self.new_token(TokenType::Equal));
                    }
                }
            }
            '^' => {
                self.forward_char();
                Some(self.new_token(TokenType::Hat))
            }
            '"' => {
                self.forward_char();
                Some(self.new_token(TokenType::DoubleQuote))
            }

            '%' => {
                self.forward_char();
                match self.current_char() {
                    '=' => {
                        // %=
                        self.forward_char();
                        return Some(self.new_token(TokenType::ModEqual));
                    }
                    _ => {
                        return Some(self.new_token(TokenType::Percent));
                    }
                }
            }
            '+' => {
                self.forward_char();
                match self.current_char() {
                    '=' => {
                        // +=
                        self.forward_char();
                        return Some(self.new_token(TokenType::PlusEqual));
                    }
                    _ => {
                        return Some(self.new_token(TokenType::Plus));
                    }
                }
            }
            '-' => {
                self.forward_char();
                match self.current_char() {
                    '=' => {
                        // -=
                        self.forward_char();
                        return Some(self.new_token(TokenType::MinusEqual));
                    }
                    _ => {
                        return Some(self.new_token(TokenType::Minus));
                    }
                }
            }
            '*' => {
                self.forward_char();
                match self.current_char() {
                    '=' => {
                        // *=
                        self.forward_char();
                        return Some(self.new_token(TokenType::MulEqual));
                    }
                    _ => {
                        return Some(self.new_token(TokenType::Asterix));
                    }
                }
            }
            '/' => {
                self.forward_char();
                match self.current_char() {
                    '=' => {
                        // /=
                        self.forward_char();
                        return Some(self.new_token(TokenType::DivEqual));
                    }
                    _ => {
                        return Some(self.new_token(TokenType::ForwardSlash));
                    }
                }
            }

            ',' => {
                self.forward_char();
                Some(self.new_token(TokenType::Comma))
            }
            '.' => {
                self.forward_char();
                Some(self.new_token(TokenType::Dot))
            }
            ':' => {
                println!("here....");
                self.forward_char();
                match self.current_char() {
                    '=' => {
                        // /=
                        self.forward_char();
                        return Some(self.new_token(TokenType::ColonEqual));
                    }
                    ':' => {
                        println!(">>>>");
                        self.forward_char();
                        return Some(self.new_token(TokenType::DoubleColon));
                    }
                    _ => {
                        return Some(self.new_token(TokenType::Colon));
                    }
                }
            }
            ';' => {
                self.forward_char();
                Some(self.new_token(TokenType::SemiColon))
            }
            ' ' | '\t' | '\r' | '\n' => {
                self.forward_char();
                return self.next();
            }
            '1'..='9' | '0' => {
                let mut is_float = false;
                self.forward_char();
                loop {
                    match self.current_char() {
                        '1'..='9' | '0' => {
                            self.forward_char();
                        }
                        '.' => {
                            println!("saw the dot.");
                            self.forward_char();
                            is_float = true;
                        }
                        _ => break,
                    }
                }
                match is_float {
                    true => Some(self.new_token(TokenType::Float)),
                    false => Some(self.new_token(TokenType::UnsignedInt)),
                }
            }

            _ => unreachable!(),
        }
    }
}

#[test]
fn floats() {
    let src = "123.123";
    let mut tokenizer = Tokenizer::new("".to_string(), src);

    let num = tokenizer.next();

    assert!(num.is_some());
    let num = num.unwrap();
    assert_eq!("123.123", &src[num.loc.0..num.loc.1]);
    assert_eq!(TokenType::Float, num.ty);
}

#[test]
fn integers() {
    let src = "123";
    let mut tokenizer = Tokenizer::new("".to_string(), src);

    let num = tokenizer.next();

    assert!(num.is_some());
    let num = num.unwrap();
    assert_eq!("123", &src[num.loc.0..num.loc.1]);
}

#[test]
fn symbols() {
    let src = "+ - * / % ' \" & | || == <= >= >> << ! |= &= ( ) { } [ ] += -= %= /= *= , . :: : :=";
    let mut tokenizer = Tokenizer::new("".to_string(), src);

    for token_type in [
        TokenType::Plus,
        TokenType::Minus,
        TokenType::Asterix,
        TokenType::ForwardSlash,
        TokenType::Percent,
        TokenType::SingleQuote,
        TokenType::DoubleQuote,
        TokenType::Ampersand,
        TokenType::Pipe,
        TokenType::DoublePipe,
        TokenType::DoubleEqual,
        TokenType::LessEqual,
        TokenType::GreaterEqual,
        TokenType::DoubleRightAngle,
        TokenType::DoubleLeftAngle,
        TokenType::Bang,
        TokenType::PipeEqual,
        TokenType::AmpersandEqual,
        TokenType::OpenParen,
        TokenType::CloseParen,
        TokenType::OpenBrace,
        TokenType::CloseBrace,
        TokenType::OpenBracket,
        TokenType::CloseBracket,
        TokenType::PlusEqual,
        TokenType::MinusEqual,
        TokenType::ModEqual,
        TokenType::DivEqual,
        TokenType::MulEqual,
        TokenType::Comma,
        TokenType::Dot,
        TokenType::DoubleColon,
        TokenType::Colon,
        TokenType::ColonEqual,
    ] {
        let token = tokenizer.next();
        assert!(token.is_some());

        let token = token.unwrap();

        assert_eq!(token.ty, token_type);
    }
}

// #[test]
// fn const_decl_char() -> Result<()> {
//     let src = "c :: 'c';";
//     let mut tokenizer = Tokenizer::new("".to_string(), src);

//     let tok = tokenizer.next();
//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Ident, tok.ty);
//     assert_eq!("c", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::DoubleColon, tok.ty);
//     assert_eq!("::", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Char, tok.ty);
//     assert_eq!("'c'", &src[tok.loc.0..=tok.loc.1]);

//     Ok(())
// }
// #[test]
// fn const_decl_with_ti() -> Result<()> {
//     let src = "f :u32: 12;";
//     let mut tokenizer = Tokenizer::new("".to_string(), src);

//     let tok = tokenizer.next();
//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Ident, tok.ty);
//     assert_eq!("f", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Colon, tok.ty);
//     assert_eq!(":", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::KeywordUint32, tok.ty);
//     assert_eq!("u32", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Colon, tok.ty);
//     assert_eq!(":", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::UnsignedInt, tok.ty);
//     assert_eq!("12", &src[tok.loc.0..=tok.loc.1]);

//     Ok(())
// }

// #[test]
// fn const_decl() {
//     let src = "f :: 12;";
//     let mut tokenizer = Tokenizer::new("".to_string(), src);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!("f", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::DoubleColon, tok.ty);
//     assert_eq!("::", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::UnsignedInt, tok.ty);
//     assert_eq!("12", &src[tok.loc.0..=tok.loc.1]);
// }

// #[test]
// fn const_decl_fn() {
//     let src = "main :: (x: int, y: uint) void {\n\t printf(\"Hello World\");\n};";
//     let mut tokenizer = Tokenizer::new("".to_string(), src);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Ident, tok.ty);
//     assert_eq!("main", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::DoubleColon, tok.ty);
//     assert_eq!("::", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::OpenParen, tok.ty);
//     assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Ident, tok.ty);
//     assert_eq!("x", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Colon, tok.ty);
//     assert_eq!(":", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::KeywordInt, tok.ty);
//     assert_eq!("int", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Comma, tok.ty);
//     assert_eq!(",", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Ident, tok.ty);
//     assert_eq!("y", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Colon, tok.ty);
//     assert_eq!(":", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::KeywordUint, tok.ty);
//     assert_eq!("uint", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::CloseParen, tok.ty);
//     assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::KeywordVoid, tok.ty);
//     assert_eq!("void", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::OpenBrace, tok.ty);
//     assert_eq!("{", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Ident, tok.ty);
//     assert_eq!("printf", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::OpenParen, tok.ty);
//     assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::StringLiteral, tok.ty);
//     assert_eq!("Hello World", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::CloseParen, tok.ty);
//     assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::SemiColon, tok.ty);
//     assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::CloseBrace, tok.ty);
//     assert_eq!("}", &src[tok.loc.0..=tok.loc.1]);
// }

// #[test]
// fn fn_sign() {
//     let src = "(x: int, y: uint) void";
//     let mut tokenizer = Tokenizer::new("".to_string(), src);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::OpenParen, tok.ty);
//     assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Ident, tok.ty);
//     assert_eq!("x", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Colon, tok.ty);
//     assert_eq!(":", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::KeywordInt, tok.ty);
//     assert_eq!("int", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Comma, tok.ty);
//     assert_eq!(",", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Ident, tok.ty);
//     assert_eq!("y", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Colon, tok.ty);
//     assert_eq!(":", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::KeywordUint, tok.ty);
//     assert_eq!("uint", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::CloseParen, tok.ty);
//     assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::KeywordVoid, tok.ty);
//     assert_eq!("void", &src[tok.loc.0..=tok.loc.1]);
// }

// #[test]
// fn var_decl() {
//     let src = "f := 12;";
//     let mut tokenizer = Tokenizer::new("".to_string(), src);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!("f", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::ColonEqual, tok.ty);
//     assert_eq!(":=", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::UnsignedInt, tok.ty);
//     assert_eq!("12", &src[tok.loc.0..=tok.loc.1]);
// }

// #[test]
// fn var_decl_with_ti() -> Result<()> {
//     let src = "f :u32 = 12;";
//     let mut tokenizer = Tokenizer::new("".to_string(), src);

//     let tok = tokenizer.next();
//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Ident, tok.ty);
//     assert_eq!("f", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Colon, tok.ty);
//     assert_eq!(":", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::KeywordUint32, tok.ty);
//     assert_eq!("u32", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Equal, tok.ty);
//     assert_eq!("=", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::UnsignedInt, tok.ty);
//     assert_eq!("12", &src[tok.loc.0..=tok.loc.1]);

//     Ok(())
// }

// #[test]
// fn strings() {
//     let src = "\"amirreza\"";
//     let mut tokenizer = Tokenizer::new("".to_string(), src);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!("amirreza", &src[tok.loc.0..=tok.loc.1]);
// }
// #[test]
// fn load_directive() {
//     let src = "#load \"stdio\";";
//     let mut tokenizer = Tokenizer::new("".to_string(), src);

//     let tok = tokenizer.next();
//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::LoadDirective, tok.ty);
//     assert_eq!("#load", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::StringLiteral, tok.ty);
//     assert_eq!("stdio", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::SemiColon, tok.ty);
//     assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);
// }

// #[test]
// fn host_directive() {
//     let src = "#host \"cstdio\";";
//     let mut tokenizer = Tokenizer::new("".to_string(), src);

//     let tok = tokenizer.next();
//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::HostDirective, tok.ty);
//     assert_eq!("#host", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::StringLiteral, tok.ty);
//     assert_eq!("cstdio", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::SemiColon, tok.ty);
//     assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);
// }
// #[test]
// fn for_c() {
//     let src = "for (i := 0; i < 10; i++) {\n\tprint(i);\n}";
//     let mut tokenizer = Tokenizer::new("".to_string(), src);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::KeywordFor, tok.ty);
//     assert_eq!("for", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::OpenParen, tok.ty);
//     assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Ident, tok.ty);
//     assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::ColonEqual, tok.ty);
//     assert_eq!(":=", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::UnsignedInt, tok.ty);
//     assert_eq!("0", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::SemiColon, tok.ty);
//     assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Ident, tok.ty);
//     assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::LeftAngle, tok.ty);
//     assert_eq!("<", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::UnsignedInt, tok.ty);
//     assert_eq!("10", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::SemiColon, tok.ty);
//     assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Ident, tok.ty);
//     assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::DoublePlus, tok.ty);
//     assert_eq!("++", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::CloseParen, tok.ty);
//     assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::OpenBrace, tok.ty);
//     assert_eq!("{", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Ident, tok.ty);
//     assert_eq!("print", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::OpenParen, tok.ty);
//     assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Ident, tok.ty);
//     assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::CloseParen, tok.ty);
//     assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::SemiColon, tok.ty);
//     assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::CloseBrace, tok.ty);
//     assert_eq!("}", &src[tok.loc.0..=tok.loc.1]);
// }
// #[test]
// fn for_while() {
//     let src = "for (i < 10) {\n\tprint(i);\n}";
//     let mut tokenizer = Tokenizer::new("".to_string(), src);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::KeywordFor, tok.ty);
//     assert_eq!("for", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::OpenParen, tok.ty);
//     assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Ident, tok.ty);
//     assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::LeftAngle, tok.ty);
//     assert_eq!("<", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::UnsignedInt, tok.ty);
//     assert_eq!("10", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::CloseParen, tok.ty);
//     assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::OpenBrace, tok.ty);
//     assert_eq!("{", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Ident, tok.ty);
//     assert_eq!("print", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::OpenParen, tok.ty);
//     assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Ident, tok.ty);
//     assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::CloseParen, tok.ty);
//     assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::SemiColon, tok.ty);
//     assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::CloseBrace, tok.ty);
//     assert_eq!("}", &src[tok.loc.0..=tok.loc.1]);
// }
// #[test]
// fn for_each() {
//     let src = "for (i in items) {\n\tprint(i);\n}";
//     let mut tokenizer = Tokenizer::new("".to_string(), src);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::KeywordFor, tok.ty);
//     assert_eq!("for", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::OpenParen, tok.ty);
//     assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Ident, tok.ty);
//     assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::KeywordIn, tok.ty);
//     assert_eq!("in", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Ident, tok.ty);
//     assert_eq!("items", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::CloseParen, tok.ty);
//     assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::OpenBrace, tok.ty);
//     assert_eq!("{", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Ident, tok.ty);
//     assert_eq!("print", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::OpenParen, tok.ty);
//     assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Ident, tok.ty);
//     assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::CloseParen, tok.ty);
//     assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::SemiColon, tok.ty);
//     assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::CloseBrace, tok.ty);
//     assert_eq!("}", &src[tok.loc.0..=tok.loc.1]);
// }

// #[test]
// fn if_stmt() {
//     let src = "if (i < 10) {\n\tprint(i);\n}";
//     let mut tokenizer = Tokenizer::new("".to_string(), src);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::KeywordIf, tok.ty);
//     assert_eq!("if", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::OpenParen, tok.ty);
//     assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Ident, tok.ty);
//     assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::LeftAngle, tok.ty);
//     assert_eq!("<", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::UnsignedInt, tok.ty);
//     assert_eq!("10", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();

//     assert!(tok.is_ok());
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::CloseParen, tok.ty);
//     assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::OpenBrace, tok.ty);
//     assert_eq!("{", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Ident, tok.ty);
//     assert_eq!("print", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::OpenParen, tok.ty);
//     assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::Ident, tok.ty);
//     assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::CloseParen, tok.ty);
//     assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::SemiColon, tok.ty);
//     assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);

//     let tok = tokenizer.next();
//     let tok = tok.unwrap();
//     assert_eq!(TokenType::CloseBrace, tok.ty);
//     assert_eq!("}", &src[tok.loc.0..=tok.loc.1]);
// }

// #[test]
// fn struct_def() -> Result<()> {
//     let src = "struct { i: int, s: string }";
//     let mut tokenizer = Tokenizer::new("file.txt".to_string(), src);
//     let tokens = tokenizer.all()?;

//     assert_eq!(
//         tokens,
//         vec![
//             Token::new(TokenType::KeywordStruct, (0, 5), 1, 7),
//             Token::new(TokenType::OpenBrace, (7, 7), 1, 9),
//             Token::new(TokenType::Ident, (9, 9), 1, 12),
//             Token::new(TokenType::Colon, (10, 10), 1, 14),
//             Token::new(TokenType::KeywordInt, (12, 14), 1, 19),
//             Token::new(TokenType::Comma, (15, 15), 1, 20),
//             Token::new(TokenType::Ident, (17, 17), 1, 23),
//             Token::new(TokenType::Colon, (18, 18), 1, 25),
//             Token::new(TokenType::KeywordString, (20, 25), 1, 33),
//             Token::new(TokenType::CloseBrace, (27, 27), 1, 35)
//         ]
//     );

//     Ok(())
// }

// // #[test]
// // fn struct_init() -> Result<()> {
// //     let src = "{ i = 10, s = \"amirreza\" };";
// //     let mut tokenizer = Tokenizer::new(src);
// //     let tokens = tokenizer.all()?;

// //     assert_eq!(
// //         tokens,
// //         vec![
// //             Token::new(TokenType::OpenBrace, (0, 0)),
// //             Token::new(TokenType::Ident, (2, 2)),
// //             Token::new(TokenType::Equal, (4, 4)),
// //             Token::new(TokenType::UnsignedInt, (6, 7)),
// //             Token::new(TokenType::Comma, (8, 8)),
// //             Token::new(TokenType::Ident, (10, 10)),
// //             Token::new(TokenType::Equal, (12, 12)),
// //             Token::new(TokenType::StringLiteral, (15, 22)),
// //             Token::new(TokenType::CloseBrace, (25, 25)),
// //             Token::new(TokenType::SemiColon, (26, 26))
// //         ]
// //     );

// //     Ok(())
// // }

// #[test]
// fn comments() -> Result<()> {
//     let src = "
// // first comment
// /* second comment */
// ";
//     let mut tokenizer = Tokenizer::new("some".to_string(), src);
//     let tokens = tokenizer.all()?;

//     assert_eq!(tokens, vec![]);

//     Ok(())
// }

// #[test]
// fn ident_deref() -> Result<()> {
//     let src = "*a";
//     let mut tokenizer = Tokenizer::new("some".to_string(), src);
//     let tokens = tokenizer.all()?;

//     assert_eq!(tokens, vec![
//         Token::new(TokenType::Asterix, (0, 0), 1, 0),
//         Token::new(TokenType::Ident, (1,1), 1, 1),
//     ]);

//     Ok(())
// }

// #[test]
// fn ident_ref() -> Result<()> {
//     let src = "&a";
//     let mut tokenizer = Tokenizer::new("".to_string(), src);
//     let tokens = tokenizer.all()?;

//     assert_eq!(
//         tokens,
//         vec![
//             Token::new(TokenType::Ampersand, (0, 0), 1, 2),
//             Token::new(TokenType::Ident, (1, 1), 1, 3)
//         ]
//     );

//     Ok(())
// }

// #[test]
// fn div_equal_ref() -> Result<()> {
//     let src = "number/=3";
//     let mut tokenizer = Tokenizer::new("some".to_string(), src);
//     let tokens = tokenizer.all()?;

//     assert_eq!(
//         tokens,
//         vec![
//             Token::new(TokenType::Ident, (0, 5), 1, 7),
//             Token::new(TokenType::DivEqual, (6, 7), 1, 9),
//             Token::new(TokenType::UnsignedInt, (8, 8), 1, 10)
//         ]
//     );

//     Ok(())
// }

// #[test]
// fn new_pointer() -> Result<()> {
//     let src = ">>";
//     let mut tokenizer = Tokenizer::new("some".to_string(), src);
//     let tokens = tokenizer.all()?;

//     assert_eq!(
//         tokens,
//         vec![Token::new(TokenType::DoubleRightAngle, (0, 1), 1, 2),]
//     );

//     Ok(())
// }

// #[test]
// fn new_deref() -> Result<()> {
//     let src = "<<";
//     let mut tokenizer = Tokenizer::new("some".to_string(), src);
//     let tokens = tokenizer.all()?;

//     assert_eq!(
//         tokens,
//         vec![Token::new(TokenType::DoubleLeftAngle, (0, 1), 1, 2),]
//     );

//     Ok(())
// }

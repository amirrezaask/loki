use serde::Serialize;

use crate::errors::*;
pub type SrcLocation = (usize, usize);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize)]
pub enum TokenType {
    EOF,
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
    Hat,
    Atsign,
    Dot,
    SingleQuote,
    DoubleQuote,
    BackSlash,
    Pipe,
    Ident,

    LoadDirective,
    HostDirective,
    CompilerFlagDirective,
    ForeignDirective,
    NoCodeGenDirective,
    CVarArgsDirective,
    CString,
    SizeDirective,
    CastDirective,
    IntPtrDirective,
    UintPtrDirective,

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
            "#size" => Self::SizeDirective,
            "#cast" => Self::CastDirective,
            "#intptr" => Self::IntPtrDirective,
            "#uintptr" => Self::UintPtrDirective,

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

#[derive(Debug, PartialEq, Clone)]
pub enum State {
    Start,
    InStringLiteral(usize),
    InCharLiteral,
    Integer(usize),
    Float(usize),
    IdentOrKeyword(usize),
    SawEqual,
    SawBang,
    InLineComment,
    InComment,
    SawPlus,
    SawMinus,
    SawSlash,
    SawAstrix,
    SawPercent,
    SawColon,
    SawHat,
    SawLeftAngleBracket,
    SawRightAngleBracket,
    SawSharp(usize),
    SawAmpersand,
    SawPipe,
}

pub struct Tokenizer {
    filename: String,
    src: Vec<char>,
    cur: usize,
    state: State,
    reached_eof: bool,
    line: usize,
    col: usize,
}

impl Tokenizer {
    pub fn new(filename: String, src: &str) -> Self {
        Tokenizer {
            filename,
            reached_eof: false,
            src: src.chars().collect(),
            cur: 0,
            state: State::Start,
            line: 1,
            col: 0,
                
        }
        
    }

    fn eof(&self) -> bool {
        self.cur >= self.src.len()
    }

    fn forward_char(&mut self) {
        self.cur += 1;
    }

    fn current_char(&self) -> char {
        self.src[self.cur]
    }

    fn emit_current_token(&mut self) -> Result<Token> {
        match self.state {
            State::SawSharp(start) => {
                self.state = State::Start;
                let ident_or_keyword: String =
                    self.src[start..self.cur].to_vec().into_iter().collect();
                let tok = Token::new(TokenType::from_str(&ident_or_keyword), (start, self.cur - 1), self.line, self.col);
                return Ok(tok);
            }

            State::Integer(start) => {
                self.state = State::Start;
                return Ok(Token::new(TokenType::UnsignedInt, (start, self.cur - 1), self.line, self.col));
            }
            State::Float(start) => {
                self.state = State::Start;
                return Ok(Token::new(TokenType::Float, (start, self.cur - 1), self.line, self.col));
            }
            State::IdentOrKeyword(start) => {
                self.state = State::Start;

                let ident_or_keyword: String =
                    self.src[start..self.cur].to_vec().into_iter().collect();
                let tok = Token::new(TokenType::from_str(&ident_or_keyword), (start, self.cur - 1), self.line, self.col);

                return Ok(tok);
            }
            State::InStringLiteral(start) => {
                self.state = State::Start;
                let tok = Token::new(TokenType::StringLiteral, (start + 1, self.cur - 1), self.line, self.col);
                self.forward_char();
                return Ok(tok);
            }
            State::Start => {
                return Ok(Token::new(TokenType::EOF, (self.src.len(), self.src.len()), self.line, self.col));
            }
            _ => {
                return Err(CompilerError {
                    filename: self.filename.clone(),
                    line: self.line,
                    col: self.col,
                    reason: Reason::TokenizerError(TokenizerError::InvalidChar { state: self.state.clone() }),
                })
            }
        }
    }

    pub fn all(&mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::<Token>::new();
        loop {
            let tok = self.next()?;
            match tok.ty {
                TokenType::EOF => {
                    break;
                }
                _ => {
                    tokens.push(tok);
                }
            }
        }
        Ok(tokens)
    }

    pub fn next(&mut self) -> Result<Token> {
        loop {
            self.col += 1;
            if self.eof() {
                if (self.reached_eof) {
                    return Ok(Token::new(TokenType::EOF, (self.src.len(), self.src.len()), self.line, self.col));
                }
                self.reached_eof = true;
                if self.state == State::InLineComment {
                    return Ok(Token::new(TokenType::EOF, (self.src.len(), self.src.len()), self.line, self.col));
                }
                return Ok(self.emit_current_token()?);
            }
            match self.state {
                State::Start => {
                    match self.current_char() {
                        '{' => {
                            self.forward_char();
                            return Ok(Token::new(TokenType::OpenBrace, (self.cur - 1, self.cur - 1), self.line, self.col));
                        }
                        '}' => {
                            self.forward_char();
                            return Ok(Token::new(TokenType::CloseBrace, (self.cur - 1, self.cur - 1), self.line, self.col));
                        }
                        '(' => {
                            self.forward_char();
                            return Ok(Token::new(TokenType::OpenParen, (self.cur - 1, self.cur - 1), self.line, self.col));
                        }
                        ')' => {
                            self.forward_char();
                            return Ok(Token::new(TokenType::CloseParen, (self.cur - 1, self.cur - 1), self.line, self.col));
                        }
                        '[' => {
                            self.forward_char();
                            return Ok(Token::new(TokenType::OpenBracket, (self.cur - 1, self.cur - 1), self.line, self.col));
                        }
                        ']' => {
                            self.forward_char();
                            return Ok(Token::new(
                                TokenType::CloseBracket,
                                (self.cur - 1, self.cur - 1),
                                self.line, self.col
                            ));
                        }
                        '!' => {
                            self.state = State::SawBang;
                            self.forward_char();
                            continue;
                        }
                        '<' => {
                            self.state = State::SawLeftAngleBracket;
                            self.forward_char();
                            continue;
                        }
                        '>' => {
                            self.state = State::SawRightAngleBracket;
                            self.forward_char();
                            continue;
                        }
                        '1'..='9' | '0' => {
                            self.state = State::Integer(self.cur);
                            self.forward_char();
                            continue;
                        }
                        '\'' => {
                            self.state = State::InCharLiteral;
                            self.forward_char();
                            continue;
                        }
                        '&' => {
                            self.forward_char();
                            self.state = State::SawAmpersand;
                            continue;
                        }
                        '=' => {
                            self.state = State::SawEqual;
                            self.forward_char();
                            continue;
                        }
                        '^' => {
                            self.state = State::Start;
                            self.forward_char();
                            return Ok(Token::new(TokenType::Hat, (self.cur-1, self.cur-1), self.line, self.col));
                        }
                        '"' => {
                            self.state = State::InStringLiteral(self.cur);
                            self.forward_char();
                            continue;
                        }
                        '%' => {
                            self.state = State::SawPercent;
                            self.forward_char();
                            continue;
                        }
                        '+' => {
                            self.state = State::SawPlus;
                            self.forward_char();
                            continue;
                        }
                        '-' => {
                            self.state = State::SawMinus;
                            self.forward_char();
                            continue;
                        }
                        '|' => {
                            self.forward_char();
                            self.state = State::SawPipe;
                            continue;
                        }
                        '/' => {
                            self.state = State::SawSlash;
                            self.forward_char();
                            continue;
                        }
                        '*' => {
                            self.state = State::SawAstrix;
                            self.forward_char();
                            continue;
                        }
                        ',' => {
                            self.forward_char();
                            return Ok(Token::new(TokenType::Comma, (self.cur - 1, self.cur - 1), self.line, self.col));
                        }

                        '.' => {
                            self.state = State::Start;
                            self.forward_char();
                            return Ok(Token::new(TokenType::Dot, (self.cur - 1, self.cur - 1), self.line, self.col));
                        }

                        ':' => {
                            self.state = State::SawColon;
                            self.forward_char();
                            continue;
                        }
                        ';' => {
                            self.state = State::Start;
                            self.forward_char();
                            return Ok(Token::new(TokenType::SemiColon, (self.cur - 1, self.cur - 1), self.line, self.col));
                        }
                        ' ' | '\t' | '\r' => {
                            self.forward_char();
                            continue;
                        }
                        '\n' => {
                            self.line += 1;
                            self.col = 0;
                            self.forward_char();
                            continue;;
                        }
                        '#' => {
                            self.state = State::SawSharp(self.cur);
                            continue;
                        }
                        _ => {
                            self.state = State::IdentOrKeyword(self.cur);
                            self.forward_char();
                            continue;
                        }
                    };
                }
                State::SawSharp(start) => match self.current_char() {
                    ' ' | '\t' | '\n' | '\r' | ':' | ';' | '(' | ')' | ',' | '+' | '-' | '.'
                    | '{' | '}' | '[' | ']' | '^' | '*' | '&' | '/' | '%' | '<' | '>' | '=' | '!' => {
                        return Ok(self.emit_current_token()?);
                    }

                    _ => {
                        self.forward_char();
                    }
                },
                State::InCharLiteral => match self.current_char() {
                    '\\' => {
                        self.forward_char();
                        continue;
                    }
                    _ => {
                        self.state = State::Start;
                        let c = self.cur;
                        self.forward_char();
                        if self.current_char() != '\'' {
                            unreachable!();
                        }
                        self.forward_char();
                        return Ok(Token::new(TokenType::Char, (c - 1, c + 1), self.line, self.col));
                    }
                },
                State::SawBang => match self.current_char() {
                    '=' => {
                        self.state = State::Start;
                        self.forward_char();
                        return Ok(Token::new(TokenType::NotEqual, (self.cur - 2, self.cur - 1), self.line, self.col));
                    }
                    _ => {
                        self.state = State::Start;
                        return Ok(Token::new(TokenType::Bang, (self.cur - 1, self.cur - 1), self.line, self.col));
                    }
                },
                State::SawAmpersand => {
                    match self.current_char() {
                        '&' => {
                            self.state = State::Start;
                            self.forward_char();
                            return Ok(Token::new(TokenType::DoubleAmpersand, (self.cur - 2, self.cur - 1), self.line, self.col));
                        }
                        _ => {
                            self.state = State::Start;
                            return Ok(Token::new(TokenType::Ampersand, (self.cur - 1, self.cur - 1), self.line, self.col));
                        }
                    }

                }
                State::SawPipe => {
                    match self.current_char() {
                        '|' => {
                            self.state = State::Start;
                            self.forward_char();
                            return Ok(Token::new(TokenType::DoublePipe, (self.cur - 2, self.cur - 1), self.line, self.col));
                        }
                        _ => {
                            self.state = State::Start;
                            return Ok(Token::new(TokenType::Pipe, (self.cur - 1, self.cur - 1), self.line, self.col));
                        }
                    }

                }
                State::SawColon => match self.current_char() {
                    ':' => {
                        self.state = State::Start;
                        self.forward_char();
                        return Ok(Token::new(TokenType::DoubleColon, (self.cur - 2, self.cur - 1), self.line, self.col));
                    }
                    '=' => {
                        self.state = State::Start;
                        self.forward_char();
                        return Ok(Token::new(TokenType::ColonEqual, (self.cur - 2, self.cur - 1), self.line, self.col));
                    } 
                    _ => {
                        self.state = State::Start;
                        let tok = Token::new(TokenType::Colon, (self.cur - 1, self.cur - 1), self.line, self.col);
                        return Ok(tok);
                    }
                },
                State::SawEqual => match self.current_char() {
                    '=' => {
                        self.state = State::Start;
                        self.forward_char();
                        return Ok(Token::new(TokenType::DoubleEqual, (self.cur - 1, self.cur), self.line, self.col));
                    }
                    _ => {
                        self.state = State::Start;
                        return Ok(Token::new(TokenType::Equal, (self.cur - 1, self.cur - 1), self.line, self.col));
                    }
                },
                State::InStringLiteral(start) => match self.current_char() {
                    '"' => {
                        return Ok(self.emit_current_token()?);
                    }
                    _ => {
                        self.forward_char();
                        continue;
                    }
                },
                State::SawLeftAngleBracket => match self.current_char() {
                    '=' => {
                        self.state = State::Start;
                        let tok = Token::new(TokenType::LessEqual, (self.cur - 1, self.cur), self.line, self.col);
                        self.forward_char();
                        return Ok(tok);
                    }
                    '<' => {
                        self.state = State::Start;
                        let tok = Token::new(TokenType::DoubleLeftAngle, (self.cur - 1, self.cur), self.line, self.col);
                        self.forward_char();
                        return Ok(tok);
                    }
                    _ => {
                        self.state = State::Start;
                        return Ok(Token::new(TokenType::LeftAngle, (self.cur - 1, self.cur - 1), self.line, self.col));
                    }
                },
                State::SawRightAngleBracket => match self.current_char() {
                    '=' => {
                        let tok = Token::new(TokenType::GreaterEqual, (self.cur - 1, self.cur), self.line, self.col);
                        self.forward_char();
                        self.state = State::Start;
                        return Ok(tok);
                    }
                    '>' => {
                        let tok = Token::new(TokenType::DoubleRightAngle, (self.cur - 1, self.cur), self.line, self.col);
                        self.forward_char();
                        self.state = State::Start;

                        return Ok(tok);
                    }
                    _ => {
                        self.state = State::Start;
                        return Ok(Token::new(TokenType::RightAngle, (self.cur - 1, self.cur - 1), self.line, self.col));
                    }
                },

                State::IdentOrKeyword(_) => match self.current_char() {
                    ' ' | '\t' | '\n' | '\r' | ':' | ';' | '(' | ')' | ',' | '+' | '-' | '.'
                    | '{' | '}' | '[' | ']' | '^' | '*' | '&' | '/' | '%'| '<' | '>' | '=' | '!' => {
                        return Ok(self.emit_current_token()?);
                    }
                    _ => {
                        self.forward_char();
                        continue;
                    }
                },
                State::Float(start) => match self.current_char() {
                    ' ' | '\t' | '\n' | '\r' | ':' | ';' | '(' | ')' | ',' | '+' | '-' | '{'
                    | '}' | '[' | ']' | '^'| '*' | '&' | '/' | '%'| '<' | '>' | '=' | '!' => {
                        return Ok(self.emit_current_token()?);
                    }
                    '.' => {
                        unreachable!();
                    }
                    _ => {
                        self.forward_char();
                        continue;
                    }
                },
                State::Integer(start) => match self.current_char() {
                    ' ' | '\t' | '\n' | '\r' | ':' | ';' | '(' | ')' | ',' | '+' | '-' | '{'
                    | '}' | '[' | ']' | '^'| '*' | '&' | '/' | '%'| '<' | '>' | '=' | '!' => {
                        return Ok(self.emit_current_token()?);
                    }
                    '.' => {
                        self.state = State::Float(start);
                        self.forward_char();
                        continue;
                    }
                    _ => {
                        self.forward_char();
                        continue;
                    }
                },
                State::SawPlus => match self.current_char() {
                    '+' => {
                        let tok = Token::new(TokenType::DoublePlus, (self.cur - 1, self.cur), self.line, self.col);
                        self.state = State::Start;
                        self.forward_char();
                        return Ok(tok);
                    }
                    '=' => {
                        let tok = Token::new(TokenType::PlusEqual, (self.cur - 1, self.cur), self.line, self.col);
                        self.state = State::Start;
                        self.forward_char();
                        return Ok(tok);
                    }
                    _ => {
                        let tok = Token::new(TokenType::Plus, (self.cur - 1, self.cur), self.line, self.col);
                        self.state = State::Start;
                        return Ok(tok);
                    }
                },

                State::SawMinus => match self.current_char() {
                    '-' => {
                        let tok = Token::new(TokenType::DoubleMinus, (self.cur - 1, self.cur), self.line, self.col);
                        self.state = State::Start;
                        self.forward_char();
                        return Ok(tok);
                    }
                    '=' => {
                        let tok = Token::new(TokenType::MinusEqual, (self.cur - 1, self.cur), self.line, self.col);
                        self.state = State::Start;
                        self.forward_char();
                        return Ok(tok);
                    }
                    _ => {
                        let tok = Token::new(TokenType::Minus, (self.cur - 1, self.cur), self.line, self.col);
                        self.state = State::Start;
                        return Ok(tok);
                    }
                },
                State::SawSlash => match self.current_char() {
                    '/' => {
                        self.state = State::InLineComment;
                        self.forward_char();
                        continue;
                    }
                    '=' => {
                        let tok = Token::new(TokenType::DivEqual, (self.cur - 1, self.cur), self.line, self.col);
                        self.state = State::Start;
                        self.forward_char();
                        return Ok(tok);
                    }
                    '*' => {
                        self.state = State::InComment;
                        self.forward_char();
                        continue;
                    }
                    _ => {
                        let tok = Token::new(TokenType::ForwardSlash, (self.cur - 1, self.cur - 1), self.line, self.col);
                        self.state = State::Start;
                        return Ok(tok);
                    }
                },
                State::InComment => match self.current_char() {
                    '*' => {
                        self.forward_char();
                        match self.current_char() {
                            '/' => {
                                self.state = State::Start;
                                self.forward_char();
                                continue;
                            }
                            _ => continue,
                        }
                    }
                    _ => {
                        self.forward_char();
                        continue;
                    }
                },
                State::SawAstrix => match self.current_char() {
                    '=' => {
                        let tok = Token::new(TokenType::MulEqual, (self.cur - 1, self.cur), self.line, self.col);
                        self.state = State::Start;
                        self.forward_char();
                        return Ok(tok);
                    }
                    _ => {
                        let tok = Token::new(TokenType::Asterix, (self.cur - 1, self.cur - 1), self.line, self.col);
                        self.state = State::Start;
                        return Ok(tok);
                    }
                },
                State::SawPercent => match self.current_char() {
                    '=' => {
                        let tok = Token::new(TokenType::ModEqual, (self.cur - 1, self.cur), self.line, self.col);
                        self.state = State::Start;
                        self.forward_char();
                        return Ok(tok);
                    }
                    _ => {
                        let tok = Token::new(TokenType::Percent, (self.cur - 1, self.cur - 1), self.line, self.col);
                        self.state = State::Start;
                        return Ok(tok);
                    }
                },
                State::InLineComment => match self.current_char() {
                    '\n' => {
                        self.state = State::Start;
                        self.forward_char();
                    }
                    _ => {
                        self.forward_char();
                    }
                },
                _ => {
                    unreachable!();
                }
            }
        }
    }
}

#[test]
fn floats() {
    let src = "123.123";
    let mut tokenizer = Tokenizer::new("".to_string(), src);

    let num = tokenizer.next();

    assert!(num.is_ok());
    let num = num.unwrap();
    assert_eq!("123.123", &src[num.loc.0..=num.loc.1]);
    assert_eq!(TokenType::Float, num.ty);
}
#[test]
fn integers() {
    let src = "123";
    let mut tokenizer = Tokenizer::new("".to_string(),src);

    let num = tokenizer.next();

    assert!(num.is_ok());
    let num = num.unwrap();
    assert_eq!("123", &src[num.loc.0..=num.loc.1]);
}
#[test]
fn const_decl_char() -> Result<()> {
    let src = "c :: 'c';";
    let mut tokenizer = Tokenizer::new("".to_string(),src);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::Ident, tok.ty);
    assert_eq!("c", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::DoubleColon, tok.ty);
    assert_eq!("::", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::Char, tok.ty);
    assert_eq!("'c'", &src[tok.loc.0..=tok.loc.1]);

    Ok(())
}
#[test]
fn const_decl_with_ti() -> Result<()> {
    let src = "f :u32: 12;";
    let mut tokenizer = Tokenizer::new("".to_string(),src);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::Ident, tok.ty);
    assert_eq!("f", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::Colon, tok.ty);
    assert_eq!(":", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::KeywordUint32, tok.ty);
    assert_eq!("u32", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::Colon, tok.ty);
    assert_eq!(":", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::UnsignedInt, tok.ty);
    assert_eq!("12", &src[tok.loc.0..=tok.loc.1]);

    Ok(())
}

#[test]
fn const_decl() {
    let src = "f :: 12;";
    let mut tokenizer = Tokenizer::new("".to_string(),src);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!("f", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::DoubleColon, tok.ty);
    assert_eq!("::", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::UnsignedInt, tok.ty);
    assert_eq!("12", &src[tok.loc.0..=tok.loc.1]);
}

#[test]
fn const_decl_fn() {
    let src = "main :: (x: int, y: uint) void {\n\t printf(\"Hello World\");\n};";
    let mut tokenizer = Tokenizer::new("".to_string(),src);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::Ident, tok.ty);
    assert_eq!("main", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::DoubleColon, tok.ty);
    assert_eq!("::", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::Ident, tok.ty);
    assert_eq!("x", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::Colon, tok.ty);
    assert_eq!(":", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::KeywordInt, tok.ty);
    assert_eq!("int", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::Comma, tok.ty);
    assert_eq!(",", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::Ident, tok.ty);
    assert_eq!("y", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::Colon, tok.ty);
    assert_eq!(":", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::KeywordUint, tok.ty);
    assert_eq!("uint", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::CloseParen, tok.ty);
    assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::KeywordVoid, tok.ty);
    assert_eq!("void", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::OpenBrace, tok.ty);
    assert_eq!("{", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::Ident, tok.ty);
    assert_eq!("printf", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::StringLiteral, tok.ty);
    assert_eq!("Hello World", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::CloseParen, tok.ty);
    assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::SemiColon, tok.ty);
    assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::CloseBrace, tok.ty);
    assert_eq!("}", &src[tok.loc.0..=tok.loc.1]);
}

#[test]
fn fn_sign() {
    let src = "(x: int, y: uint) void";
    let mut tokenizer = Tokenizer::new("".to_string(),src);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::Ident, tok.ty);
    assert_eq!("x", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::Colon, tok.ty);
    assert_eq!(":", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::KeywordInt, tok.ty);
    assert_eq!("int", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::Comma, tok.ty);
    assert_eq!(",", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::Ident, tok.ty);
    assert_eq!("y", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::Colon, tok.ty);
    assert_eq!(":", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::KeywordUint, tok.ty);
    assert_eq!("uint", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::CloseParen, tok.ty);
    assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::KeywordVoid, tok.ty);
    assert_eq!("void", &src[tok.loc.0..=tok.loc.1]);
}

#[test]
fn var_decl() {
    let src = "f := 12;";
    let mut tokenizer = Tokenizer::new("".to_string(),src);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!("f", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::ColonEqual, tok.ty);
    assert_eq!(":=", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::UnsignedInt, tok.ty);
    assert_eq!("12", &src[tok.loc.0..=tok.loc.1]);
}

#[test]
fn var_decl_with_ti() -> Result<()> {
    let src = "f :u32 = 12;";
    let mut tokenizer = Tokenizer::new("".to_string(),src);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::Ident, tok.ty);
    assert_eq!("f", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::Colon, tok.ty);
    assert_eq!(":", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::KeywordUint32, tok.ty);
    assert_eq!("u32", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::Equal, tok.ty);
    assert_eq!("=", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::UnsignedInt, tok.ty);
    assert_eq!("12", &src[tok.loc.0..=tok.loc.1]);

    Ok(())
}

#[test]
fn strings() {
    let src = "\"amirreza\"";
    let mut tokenizer = Tokenizer::new("".to_string(),src);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!("amirreza", &src[tok.loc.0..=tok.loc.1]);
}
#[test]
fn load_directive() {
    let src = "#load \"stdio\";";
    let mut tokenizer = Tokenizer::new("".to_string(),src);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::LoadDirective, tok.ty);
    assert_eq!("#load", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::StringLiteral, tok.ty);
    assert_eq!("stdio", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::SemiColon, tok.ty);
    assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);
}

#[test]
fn host_directive() {
    let src = "#host \"cstdio\";";
    let mut tokenizer = Tokenizer::new("".to_string(),src);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::HostDirective, tok.ty);
    assert_eq!("#host", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::StringLiteral, tok.ty);
    assert_eq!("cstdio", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::SemiColon, tok.ty);
    assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);
}
#[test]
fn for_c() {
    let src = "for (i := 0; i < 10; i++) {\n\tprint(i);\n}";
    let mut tokenizer = Tokenizer::new("".to_string(),src);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::KeywordFor, tok.ty);
    assert_eq!("for", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::Ident, tok.ty);
    assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::ColonEqual, tok.ty);
    assert_eq!(":=", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::UnsignedInt, tok.ty);
    assert_eq!("0", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::SemiColon, tok.ty);
    assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::Ident, tok.ty);
    assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::LeftAngle, tok.ty);
    assert_eq!("<", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::UnsignedInt, tok.ty);
    assert_eq!("10", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::SemiColon, tok.ty);
    assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::Ident, tok.ty);
    assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::DoublePlus, tok.ty);
    assert_eq!("++", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::CloseParen, tok.ty);
    assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::OpenBrace, tok.ty);
    assert_eq!("{", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::Ident, tok.ty);
    assert_eq!("print", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::Ident, tok.ty);
    assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::CloseParen, tok.ty);
    assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::SemiColon, tok.ty);
    assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::CloseBrace, tok.ty);
    assert_eq!("}", &src[tok.loc.0..=tok.loc.1]);
}
#[test]
fn for_while() {
    let src = "for (i < 10) {\n\tprint(i);\n}";
    let mut tokenizer = Tokenizer::new("".to_string(),src);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::KeywordFor, tok.ty);
    assert_eq!("for", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::Ident, tok.ty);
    assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::LeftAngle, tok.ty);
    assert_eq!("<", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::UnsignedInt, tok.ty);
    assert_eq!("10", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::CloseParen, tok.ty);
    assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::OpenBrace, tok.ty);
    assert_eq!("{", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::Ident, tok.ty);
    assert_eq!("print", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::Ident, tok.ty);
    assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::CloseParen, tok.ty);
    assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::SemiColon, tok.ty);
    assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::CloseBrace, tok.ty);
    assert_eq!("}", &src[tok.loc.0..=tok.loc.1]);
}
#[test]
fn for_each() {
    let src = "for (i in items) {\n\tprint(i);\n}";
    let mut tokenizer = Tokenizer::new("".to_string(),src);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::KeywordFor, tok.ty);
    assert_eq!("for", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::Ident, tok.ty);
    assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::KeywordIn, tok.ty);
    assert_eq!("in", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::Ident, tok.ty);
    assert_eq!("items", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::CloseParen, tok.ty);
    assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::OpenBrace, tok.ty);
    assert_eq!("{", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::Ident, tok.ty);
    assert_eq!("print", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::Ident, tok.ty);
    assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::CloseParen, tok.ty);
    assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::SemiColon, tok.ty);
    assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::CloseBrace, tok.ty);
    assert_eq!("}", &src[tok.loc.0..=tok.loc.1]);
}

#[test]
fn if_stmt() {
    let src = "if (i < 10) {\n\tprint(i);\n}";
    let mut tokenizer = Tokenizer::new("".to_string(),src);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::KeywordIf, tok.ty);
    assert_eq!("if", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::Ident, tok.ty);
    assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::LeftAngle, tok.ty);
    assert_eq!("<", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::UnsignedInt, tok.ty);
    assert_eq!("10", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(TokenType::CloseParen, tok.ty);
    assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::OpenBrace, tok.ty);
    assert_eq!("{", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::Ident, tok.ty);
    assert_eq!("print", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::Ident, tok.ty);
    assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::CloseParen, tok.ty);
    assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::SemiColon, tok.ty);
    assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(TokenType::CloseBrace, tok.ty);
    assert_eq!("}", &src[tok.loc.0..=tok.loc.1]);
}

// #[test]
// fn struct_def() -> Result<()> {
//     let src = "struct { i: int, s: string }";
//     let mut tokenizer = Tokenizer::new(src);
//     let tokens = tokenizer.all()?;

//     assert_eq!(
//         tokens,
//         vec![
//             Token::new(TokenType::KeywordStruct, (0, 5)),
//             Token::new(TokenType::OpenBrace, (7, 7)),
//             Token::new(TokenType::Ident, (9, 9)),
//             Token::new(TokenType::Colon, (10, 10)),
//             Token::new(TokenType::KeywordInt, (12, 14)),
//             Token::new(TokenType::Comma, (15, 15)),
//             Token::new(TokenType::Ident, (17, 17)),
//             Token::new(TokenType::Colon, (18, 18)),
//             Token::new(TokenType::KeywordString, (20, 25)),
//             Token::new(TokenType::CloseBrace, (27, 27))
//         ]
//     );

//     Ok(())
// }

// #[test]
// fn struct_init() -> Result<()> {
//     let src = "{ i = 10, s = \"amirreza\" };";
//     let mut tokenizer = Tokenizer::new(src);
//     let tokens = tokenizer.all()?;

//     assert_eq!(
//         tokens,
//         vec![
//             Token::new(TokenType::OpenBrace, (0, 0)),
//             Token::new(TokenType::Ident, (2, 2)),
//             Token::new(TokenType::Equal, (4, 4)),
//             Token::new(TokenType::UnsignedInt, (6, 7)),
//             Token::new(TokenType::Comma, (8, 8)),
//             Token::new(TokenType::Ident, (10, 10)),
//             Token::new(TokenType::Equal, (12, 12)),
//             Token::new(TokenType::StringLiteral, (15, 22)),
//             Token::new(TokenType::CloseBrace, (25, 25)),
//             Token::new(TokenType::SemiColon, (26, 26))
//         ]
//     );

//     Ok(())
// }

// #[test]
// fn comments() -> Result<()> {
//     let src = "
// // first comment
// /* second comment */
// ";
//     let mut tokenizer = Tokenizer::new(src);
//     let tokens = tokenizer.all()?;

//     assert_eq!(tokens, vec![]);

//     Ok(())
// }



// #[test]
// fn ident_deref() -> Result<()> {
//     let src = "*a";
//     let mut tokenizer = Tokenizer::new(src);
//     let tokens = tokenizer.all()?;

//     assert_eq!(tokens, vec![
//         Token::new(TokenType::Asterix, (0, 0)),
//         Token::new(TokenType::Ident, (1,1)),
//     ]);

//     Ok(())
// }

// #[test]
// fn ident_ref() -> Result<()> {
//     let src = "&a";
//     let mut tokenizer = Tokenizer::new(src);
//     let tokens = tokenizer.all()?;

//     assert_eq!(tokens, vec![
//         Token::new(TokenType::Ampersand, (0, 0)),
//         Token::new(TokenType::Ident, (1,1))
//     ]);

//     Ok(())
// }

// #[test]
// fn div_equal_ref() -> Result<()> {
//     let src = "number/=3";
//     let mut tokenizer = Tokenizer::new(src);
//     let tokens = tokenizer.all()?;

//     assert_eq!(tokens, vec![
//         Token::new(TokenType::Ident, (0, 5)),
//         Token::new(TokenType::DivEqual, (6,7)),
//         Token::new(TokenType::UnsignedInt, (8,8))

//     ]);

//     Ok(())
// }
// #[test]
// fn new_pointer() -> Result<()> {
//     let src = ">>";
//     let mut tokenizer = Tokenizer::new(src);
//     let tokens = tokenizer.all()?;

//     assert_eq!(tokens, vec![
//         Token::new(TokenType::DoubleRightAngle, (0, 1)),
//     ]);

//     Ok(())
// }

// #[test]
// fn new_deref() -> Result<()> {
//     let src = "<<";
//     let mut tokenizer = Tokenizer::new(src);
//     let tokens = tokenizer.all()?;

//     assert_eq!(tokens, vec![
//         Token::new(TokenType::DoubleLeftAngle, (0, 1)),
//     ]);

//     Ok(())
// }

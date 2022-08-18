use anyhow::Result;
use serde::Serialize;

pub type SrcLocation = (usize, usize);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize)]
pub enum Type {
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
    C_CompilerFlagDirective,

    KeywordConst,
    KeywordVar,
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

impl Type {
    fn from_str(s: &str) -> Self {
        match s {
            "#load" => Self::LoadDirective,
            "#host" => Self::HostDirective,

            "as" => Self::KeywordAs,
            "in" => Self::KeywordIn,
            "if" => Self::KeywordIf,
            "var" => Self::KeywordVar,
            "const" => Self::KeywordConst,
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
#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub ty: Type,
    pub loc: SrcLocation,
}

impl Token {
    pub fn new(ty: Type, loc: (usize, usize)) -> Self {
        Self {
            ty,
            loc: (loc.0, loc.1),
        }
    }
}

#[derive(Debug)]
enum State {
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
}

pub struct Tokenizer {
    src: Vec<char>,
    cur: usize,
    state: State,
    reached_eof: bool,
}

impl Tokenizer {
    pub fn new(src: &str) -> Self {
        Tokenizer {
            reached_eof: false,
            src: src.chars().collect(),
            cur: 0,
            state: State::Start,
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

    fn emit_current_token(&mut self) -> Token {
        // println!(
        //     "emitting current token idx {:?}, state: {:?}",
        //     self.cur, self.state
        // );
        match self.state {
            State::SawSharp(start) => {
                self.state = State::Start;
                let ident_or_keyword: String =
                    self.src[start..self.cur].to_vec().into_iter().collect();
                let tok = Token::new(Type::from_str(&ident_or_keyword), (start, self.cur - 1));
                return tok;
            }

            State::Integer(start) => {
                self.state = State::Start;
                return Token::new(Type::UnsignedInt, (start, self.cur - 1));
            }
            State::Float(start) => {
                self.state = State::Start;
                return Token::new(Type::Float, (start, self.cur - 1));
            }
            State::IdentOrKeyword(start) => {
                self.state = State::Start;

                let ident_or_keyword: String =
                    self.src[start..self.cur].to_vec().into_iter().collect();
                let tok = Token::new(Type::from_str(&ident_or_keyword), (start, self.cur - 1));

                return tok;
            }
            State::InStringLiteral(start) => {
                self.state = State::Start;
                let tok = Token::new(Type::StringLiteral, (start + 1, self.cur - 1));
                self.forward_char();
                return tok;
            }
            State::Start => {
                return Token::new(Type::EOF, (self.src.len(), self.src.len()));
            }
            _ => {
                unreachable!();
            }
        }
    }

    pub fn all(&mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::<Token>::new();
        loop {
            let tok = self.next()?;
            match tok.ty {
                Type::EOF => {
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
            // println!("@state: {:?}", self.state);

            if self.eof() {
                if (self.reached_eof) {
                    return Ok(Token::new(Type::EOF, (self.src.len(), self.src.len())));
                }
                self.reached_eof = true;
                return Ok(self.emit_current_token());
            }
            // println!("@current_char: {}", self.current_char());
            match self.state {
                State::Start => {
                    match self.current_char() {
                        '{' => {
                            self.forward_char();
                            return Ok(Token::new(Type::OpenBrace, (self.cur - 1, self.cur - 1)));
                        }
                        '}' => {
                            self.forward_char();
                            return Ok(Token::new(Type::CloseBrace, (self.cur - 1, self.cur - 1)));
                        }
                        '(' => {
                            self.forward_char();
                            return Ok(Token::new(Type::OpenParen, (self.cur - 1, self.cur - 1)));
                        }
                        ')' => {
                            self.forward_char();
                            return Ok(Token::new(Type::CloseParen, (self.cur - 1, self.cur - 1)));
                        }
                        '[' => {
                            self.forward_char();
                            return Ok(Token::new(Type::OpenBracket, (self.cur - 1, self.cur - 1)));
                        }
                        ']' => {
                            self.forward_char();
                            return Ok(Token::new(
                                Type::CloseBracket,
                                (self.cur - 1, self.cur - 1),
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
                            return Ok(Token::new(Type::Ampersand, (self.cur-1, self.cur-1)));
                        }
                        '=' => {
                            self.state = State::SawEqual;
                            self.forward_char();
                            continue;
                        }
                        '^' => {
                            self.state = State::Start;
                            self.forward_char();
                            return Ok(Token::new(Type::Hat, (self.cur-1, self.cur-1)));
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
                            return Ok(Token::new(Type::Comma, (self.cur - 1, self.cur - 1)));
                        }

                        '.' => {
                            self.state = State::Start;
                            self.forward_char();
                            return Ok(Token::new(Type::Dot, (self.cur - 1, self.cur - 1)));
                        }

                        ':' => {
                            self.state = State::SawColon;
                            self.forward_char();
                            continue;
                        }
                        ';' => {
                            self.state = State::Start;
                            self.forward_char();
                            return Ok(Token::new(Type::SemiColon, (self.cur - 1, self.cur - 1)));
                        }
                        ' ' | '\t' | '\r' | '\n' => {
                            self.forward_char();
                            continue;
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
                        return Ok(self.emit_current_token());
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
                        return Ok(Token::new(Type::Char, (c - 1, c + 1)));
                    }
                },
                State::SawBang => match self.current_char() {
                    '=' => {
                        self.state = State::Start;
                        self.forward_char();
                        return Ok(Token::new(Type::NotEqual, (self.cur - 2, self.cur - 1)));
                    }
                    _ => {
                        self.state = State::Start;
                        return Ok(Token::new(Type::Bang, (self.cur - 1, self.cur - 1)));
                    }
                },
                State::SawColon => match self.current_char() {
                    ':' => {
                        self.state = State::Start;
                        self.forward_char();
                        return Ok(Token::new(Type::DoubleColon, (self.cur - 2, self.cur - 1)));
                    }
                    '=' => {
                        self.state = State::Start;
                        self.forward_char();
                        return Ok(Token::new(Type::ColonEqual, (self.cur - 2, self.cur - 1)));
                    }
                    _ => {
                        self.state = State::Start;
                        let tok = Token::new(Type::Colon, (self.cur - 1, self.cur - 1));
                        return Ok(tok);
                    }
                },
                State::SawEqual => match self.current_char() {
                    '=' => {
                        self.state = State::Start;
                        self.forward_char();
                        return Ok(Token::new(Type::DoubleEqual, (self.cur - 1, self.cur)));
                    }
                    _ => {
                        self.state = State::Start;
                        return Ok(Token::new(Type::Equal, (self.cur - 1, self.cur - 1)));
                    }
                },
                State::InStringLiteral(start) => match self.current_char() {
                    '"' => {
                        return Ok(self.emit_current_token());
                    }
                    _ => {
                        self.forward_char();
                        continue;
                    }
                },
                State::SawLeftAngleBracket => match self.current_char() {
                    '=' => {
                        let tok = Token::new(Type::LessEqual, (self.cur - 1, self.cur));
                        self.forward_char();
                        return Ok(tok);
                    }
                    _ => {
                        self.state = State::Start;
                        return Ok(Token::new(Type::LeftAngle, (self.cur - 1, self.cur - 1)));
                    }
                },
                State::SawRightAngleBracket => match self.current_char() {
                    '=' => {
                        let tok = Token::new(Type::GreaterEqual, (self.cur - 1, self.cur));
                        self.forward_char();
                        self.state = State::Start;
                        return Ok(tok);
                    }
                    _ => {
                        self.state = State::Start;
                        return Ok(Token::new(Type::RightAngle, (self.cur - 1, self.cur - 1)));
                    }
                },

                State::IdentOrKeyword(_) => match self.current_char() {
                    ' ' | '\t' | '\n' | '\r' | ':' | ';' | '(' | ')' | ',' | '+' | '-' | '.'
                    | '{' | '}' | '[' | ']' | '^' | '*' | '&' | '/' | '%'| '<' | '>' | '=' | '!' => {
                        return Ok(self.emit_current_token());
                    }
                    _ => {
                        self.forward_char();
                        continue;
                    }
                },
                State::Float(start) => match self.current_char() {
                    ' ' | '\t' | '\n' | '\r' | ':' | ';' | '(' | ')' | ',' | '+' | '-' | '{'
                    | '}' | '[' | ']' | '^'| '*' | '&' | '/' | '%'| '<' | '>' | '=' | '!' => {
                        return Ok(self.emit_current_token());
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
                        return Ok(self.emit_current_token());
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
                        let tok = Token::new(Type::DoublePlus, (self.cur - 1, self.cur));
                        self.state = State::Start;
                        self.forward_char();
                        return Ok(tok);
                    }
                    '=' => {
                        let tok = Token::new(Type::PlusEqual, (self.cur - 1, self.cur));
                        self.state = State::Start;
                        self.forward_char();
                        return Ok(tok);
                    }
                    _ => {
                        let tok = Token::new(Type::Plus, (self.cur - 1, self.cur));
                        self.state = State::Start;
                        return Ok(tok);
                    }
                },

                State::SawMinus => match self.current_char() {
                    '-' => {
                        let tok = Token::new(Type::DoubleMinus, (self.cur - 1, self.cur));
                        self.state = State::Start;
                        self.forward_char();
                        return Ok(tok);
                    }
                    '=' => {
                        let tok = Token::new(Type::MinusEqual, (self.cur - 1, self.cur));
                        self.state = State::Start;
                        self.forward_char();
                        return Ok(tok);
                    }
                    _ => {
                        let tok = Token::new(Type::Minus, (self.cur - 1, self.cur));
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
                        let tok = Token::new(Type::DivEqual, (self.cur - 1, self.cur));
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
                        let tok = Token::new(Type::ForwardSlash, (self.cur - 1, self.cur - 1));
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
                        let tok = Token::new(Type::MulEqual, (self.cur - 1, self.cur));
                        self.state = State::Start;
                        self.forward_char();
                        return Ok(tok);
                    }
                    _ => {
                        let tok = Token::new(Type::Asterix, (self.cur - 1, self.cur - 1));
                        self.state = State::Start;
                        return Ok(tok);
                    }
                },
                State::SawPercent => match self.current_char() {
                    '=' => {
                        let tok = Token::new(Type::ModEqual, (self.cur - 1, self.cur));
                        self.state = State::Start;
                        self.forward_char();
                        return Ok(tok);
                    }
                    _ => {
                        let tok = Token::new(Type::Asterix, (self.cur - 1, self.cur - 1));
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
    let mut tokenizer = Tokenizer::new(src);

    let num = tokenizer.next();

    assert!(num.is_ok());
    let num = num.unwrap();
    assert_eq!("123.123", &src[num.loc.0..=num.loc.1]);
    assert_eq!(Type::Float, num.ty);
}
#[test]
fn integers() {
    let src = "123";
    let mut tokenizer = Tokenizer::new(src);

    let num = tokenizer.next();

    assert!(num.is_ok());
    let num = num.unwrap();
    assert_eq!("123", &src[num.loc.0..=num.loc.1]);
}
#[test]
fn const_decl_char() -> Result<()> {
    let src = "c :: 'c';";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::Ident, tok.ty);
    assert_eq!("c", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::DoubleColon, tok.ty);
    assert_eq!("::", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::Char, tok.ty);
    assert_eq!("'c'", &src[tok.loc.0..=tok.loc.1]);

    Ok(())
}
#[test]
fn const_decl_with_ti() -> Result<()> {
    let src = "f :uint32: 12;";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::Ident, tok.ty);
    assert_eq!("f", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::Colon, tok.ty);
    assert_eq!(":", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordUint32, tok.ty);
    assert_eq!("uint32", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::Colon, tok.ty);
    assert_eq!(":", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::UnsignedInt, tok.ty);
    assert_eq!("12", &src[tok.loc.0..=tok.loc.1]);

    Ok(())
}

#[test]
fn const_decl() {
    let src = "f :: 12;";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!("f", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::DoubleColon, tok.ty);
    assert_eq!("::", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::UnsignedInt, tok.ty);
    assert_eq!("12", &src[tok.loc.0..=tok.loc.1]);
}

#[test]
fn const_decl_fn() {
    let src = "main :: (x: int, y: uint) void {\n\t printf(\"Hello World\");\n};";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Ident, tok.ty);
    assert_eq!("main", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::DoubleColon, tok.ty);
    assert_eq!("::", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Ident, tok.ty);
    assert_eq!("x", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Colon, tok.ty);
    assert_eq!(":", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordInt, tok.ty);
    assert_eq!("int", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Comma, tok.ty);
    assert_eq!(",", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Ident, tok.ty);
    assert_eq!("y", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Colon, tok.ty);
    assert_eq!(":", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordUint, tok.ty);
    assert_eq!("uint", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::CloseParen, tok.ty);
    assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordVoid, tok.ty);
    assert_eq!("void", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::OpenBrace, tok.ty);
    assert_eq!("{", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Ident, tok.ty);
    assert_eq!("printf", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::StringLiteral, tok.ty);
    assert_eq!("Hello World", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::CloseParen, tok.ty);
    assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::SemiColon, tok.ty);
    assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::CloseBrace, tok.ty);
    assert_eq!("}", &src[tok.loc.0..=tok.loc.1]);
}

#[test]
fn fn_sign() {
    let src = "(x: int, y: uint) void";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Ident, tok.ty);
    assert_eq!("x", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Colon, tok.ty);
    assert_eq!(":", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordInt, tok.ty);
    assert_eq!("int", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Comma, tok.ty);
    assert_eq!(",", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Ident, tok.ty);
    assert_eq!("y", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Colon, tok.ty);
    assert_eq!(":", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordUint, tok.ty);
    assert_eq!("uint", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::CloseParen, tok.ty);
    assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordVoid, tok.ty);
    assert_eq!("void", &src[tok.loc.0..=tok.loc.1]);
}

#[test]
fn var_decl() {
    let src = "f := 12;";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!("f", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::ColonEqual, tok.ty);
    assert_eq!(":=", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::UnsignedInt, tok.ty);
    assert_eq!("12", &src[tok.loc.0..=tok.loc.1]);
}

#[test]
fn var_decl_with_ti() -> Result<()> {
    let src = "f :uint32 = 12;";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::Ident, tok.ty);
    assert_eq!("f", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::Colon, tok.ty);
    assert_eq!(":", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordUint32, tok.ty);
    assert_eq!("uint32", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::Equal, tok.ty);
    assert_eq!("=", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::UnsignedInt, tok.ty);
    assert_eq!("12", &src[tok.loc.0..=tok.loc.1]);

    Ok(())
}

#[test]
fn strings() {
    let src = "\"amirreza\"";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!("amirreza", &src[tok.loc.0..=tok.loc.1]);
}
#[test]
fn load_directive() {
    let src = "#load \"stdio\";";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::LoadDirective, tok.ty);
    assert_eq!("#load", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::StringLiteral, tok.ty);
    assert_eq!("stdio", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::SemiColon, tok.ty);
    assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);
}

#[test]
fn host_directive() {
    let src = "#host \"cstdio\";";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::HostDirective, tok.ty);
    assert_eq!("#host", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::StringLiteral, tok.ty);
    assert_eq!("cstdio", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::SemiColon, tok.ty);
    assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);
}
#[test]
fn for_c() {
    let src = "for (var i = 0; i < 10; i++) {\n\tprint(i);\n}";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordFor, tok.ty);
    assert_eq!("for", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordVar, tok.ty);
    assert_eq!("var", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::Ident, tok.ty);
    assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::Equal, tok.ty);
    assert_eq!("=", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::UnsignedInt, tok.ty);
    assert_eq!("0", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::SemiColon, tok.ty);
    assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::Ident, tok.ty);
    assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::LeftAngle, tok.ty);
    assert_eq!("<", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::UnsignedInt, tok.ty);
    assert_eq!("10", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::SemiColon, tok.ty);
    assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::Ident, tok.ty);
    assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::DoublePlus, tok.ty);
    assert_eq!("++", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::CloseParen, tok.ty);
    assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::OpenBrace, tok.ty);
    assert_eq!("{", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Ident, tok.ty);
    assert_eq!("print", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Ident, tok.ty);
    assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::CloseParen, tok.ty);
    assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::SemiColon, tok.ty);
    assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::CloseBrace, tok.ty);
    assert_eq!("}", &src[tok.loc.0..=tok.loc.1]);
}
#[test]
fn for_while() {
    let src = "for (i < 10) {\n\tprint(i);\n}";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordFor, tok.ty);
    assert_eq!("for", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::Ident, tok.ty);
    assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::LeftAngle, tok.ty);
    assert_eq!("<", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::UnsignedInt, tok.ty);
    assert_eq!("10", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::CloseParen, tok.ty);
    assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::OpenBrace, tok.ty);
    assert_eq!("{", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Ident, tok.ty);
    assert_eq!("print", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Ident, tok.ty);
    assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::CloseParen, tok.ty);
    assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::SemiColon, tok.ty);
    assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::CloseBrace, tok.ty);
    assert_eq!("}", &src[tok.loc.0..=tok.loc.1]);
}
#[test]
fn for_each() {
    let src = "for (i in items) {\n\tprint(i);\n}";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordFor, tok.ty);
    assert_eq!("for", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::Ident, tok.ty);
    assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordIn, tok.ty);
    assert_eq!("in", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::Ident, tok.ty);
    assert_eq!("items", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::CloseParen, tok.ty);
    assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::OpenBrace, tok.ty);
    assert_eq!("{", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Ident, tok.ty);
    assert_eq!("print", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Ident, tok.ty);
    assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::CloseParen, tok.ty);
    assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::SemiColon, tok.ty);
    assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::CloseBrace, tok.ty);
    assert_eq!("}", &src[tok.loc.0..=tok.loc.1]);
}

#[test]
fn if_stmt() {
    let src = "if (i < 10) {\n\tprint(i);\n}";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordIf, tok.ty);
    assert_eq!("if", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::Ident, tok.ty);
    assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::LeftAngle, tok.ty);
    assert_eq!("<", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::UnsignedInt, tok.ty);
    assert_eq!("10", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::CloseParen, tok.ty);
    assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::OpenBrace, tok.ty);
    assert_eq!("{", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Ident, tok.ty);
    assert_eq!("print", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Ident, tok.ty);
    assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::CloseParen, tok.ty);
    assert_eq!(")", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::SemiColon, tok.ty);
    assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::CloseBrace, tok.ty);
    assert_eq!("}", &src[tok.loc.0..=tok.loc.1]);
}

#[test]
fn struct_def() -> Result<()> {
    let src = "struct { i: int, s: string }";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.all()?;

    assert_eq!(
        tokens,
        vec![
            Token::new(Type::KeywordStruct, (0, 5)),
            Token::new(Type::OpenBrace, (7, 7)),
            Token::new(Type::Ident, (9, 9)),
            Token::new(Type::Colon, (10, 10)),
            Token::new(Type::KeywordInt, (12, 14)),
            Token::new(Type::Comma, (15, 15)),
            Token::new(Type::Ident, (17, 17)),
            Token::new(Type::Colon, (18, 18)),
            Token::new(Type::KeywordString, (20, 25)),
            Token::new(Type::CloseBrace, (27, 27))
        ]
    );

    Ok(())
}

#[test]
fn struct_init() -> Result<()> {
    let src = "{ i = 10, s = \"amirreza\" };";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.all()?;

    assert_eq!(
        tokens,
        vec![
            Token::new(Type::OpenBrace, (0, 0)),
            Token::new(Type::Ident, (2, 2)),
            Token::new(Type::Equal, (4, 4)),
            Token::new(Type::UnsignedInt, (6, 7)),
            Token::new(Type::Comma, (8, 8)),
            Token::new(Type::Ident, (10, 10)),
            Token::new(Type::Equal, (12, 12)),
            Token::new(Type::StringLiteral, (15, 22)),
            Token::new(Type::CloseBrace, (25, 25)),
            Token::new(Type::SemiColon, (26, 26))
        ]
    );

    Ok(())
}

#[test]
fn comments() -> Result<()> {
    let src = "
// first comment
/* second comment */
";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.all()?;

    assert_eq!(tokens, vec![]);

    Ok(())
}



#[test]
fn ident_deref() -> Result<()> {
    let src = "*a";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.all()?;

    assert_eq!(tokens, vec![
        Token::new(Type::Asterix, (0, 0)),
        Token::new(Type::Ident, (1,1)),
    ]);

    Ok(())
}

#[test]
fn ident_ref() -> Result<()> {
    let src = "&a";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.all()?;

    assert_eq!(tokens, vec![
        Token::new(Type::Ampersand, (0, 0)),
        Token::new(Type::Ident, (1,1))
    ]);

    Ok(())
}

#[test]
fn div_equal_ref() -> Result<()> {
    let src = "number/=3";
    let mut tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.all()?;

    assert_eq!(tokens, vec![
        Token::new(Type::Ident, (0, 5)),
        Token::new(Type::DivEqual, (6,7)),
        Token::new(Type::UnsignedInt, (8,8))

    ]);

    Ok(())
}

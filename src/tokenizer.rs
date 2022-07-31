use anyhow::Result;
pub type SrcLocation = (usize, usize);
/*TODO:
- handle floats


*/
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
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
    Identifier,

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
    KeywordImport,
    KeywordFn,
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
    KeywordFloat,
    KeywordChar,
    KeywordAs,

    Char,
    UnsignedInt,
    Float,
    StringLiteral,
}

impl Type {
    fn to_vec_str() -> Vec<&'static str> {
        vec![
            "in", "as", "var", "const", "if", "switch", "goto", "for", "while", "continue",
            "break", "import", "fn", "return", "true", "false", "enum", "else", "bool", "struct",
            "union", "void", "int", "uint", "string", "float", "char",
        ]
    }
    fn from_str(s: &str) -> Self {
        match s {
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
            "import" => Self::KeywordImport,
            "fn" => Self::KeywordFn,
            "return" => Self::KeywordReturn,
            "enum" => Self::KeywordEnum,
            "else" => Self::KeywordElse,
            "bool" => Self::KeywordBool,
            "struct" => Self::KeywordStruct,
            "union" => Self::KeywordUnion,
            "void" => Self::KeywordVoid,

            "int8" => Self::KeywordInt8,
            "int16" => Self::KeywordInt16,
            "int32" => Self::KeywordInt32,
            "int64" => Self::KeywordInt64,
            "int128" => Self::KeywordInt128,
            "int" => Self::KeywordInt,

            "uint8" => Self::KeywordUint8,
            "uint16" => Self::KeywordUint16,
            "uint32" => Self::KeywordUint32,
            "uint64" => Self::KeywordUint64,
            "uint128" => Self::KeywordUint128,
            "uint" => Self::KeywordUint,

            "string" => Self::KeywordString,
            "float" => Self::KeywordFloat,
            "char" => Self::KeywordChar,
            _ => Self::Identifier,
        }
    }
}
#[derive(Debug)]
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
    IdentifierOrKeyword(usize),
    SawEqual,
    SawBang,
    InLineComment,
    SawPlus,
    SawMinus,
    SawSlash,
    SawAstrix,
    SawPercent,
    SawColon,
    SawHat,
    SawLeftAngleBracket,
    SawRightAngleBracket,
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
            State::Integer(start) => {
                self.state = State::Start;
                return Token::new(Type::UnsignedInt, (start, self.cur - 1));
            }
            State::Float(start) => {
                self.state = State::Start;
                return Token::new(Type::Float, (start, self.cur - 1));
            }
            State::IdentifierOrKeyword(start) => {
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
                        '=' => {
                            self.state = State::SawEqual;
                            self.forward_char();
                            continue;
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
                        _ => {
                            self.state = State::IdentifierOrKeyword(self.cur);
                            self.forward_char();
                            continue;
                        }
                    };
                }
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

                State::IdentifierOrKeyword(_) => match self.current_char() {
                    ' ' | '\t' | '\n' | '\r' | ':' | ';' | '(' | ')' | ',' | '+' | '-' | '.'
                    | '{' | '}' | '[' | ']' => {
                        return Ok(self.emit_current_token());
                    }
                    _ => {
                        self.forward_char();
                        continue;
                    }
                },
                State::Float(start) => match self.current_char() {
                    ' ' | '\t' | '\n' | '\r' | ';' | ')' | '(' => {
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
                State::Integer(start) => match self.current_char() {
                    ' ' | '\t' | '\n' | '\r' | ';' | ')' | '(' => {
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
                    _ => {
                        let tok = Token::new(Type::ForwardSlash, (self.cur - 1, self.cur - 1));
                        self.state = State::Start;
                        return Ok(tok);
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
fn keywords() {
    for keyword in Type::to_vec_str() {
        let mut tokenizer = Tokenizer::new(keyword);

        let tok = tokenizer.next();

        assert!(tok.is_ok());
        let tok = tok.unwrap();
        assert_eq!(&keyword[0..keyword.len()], &keyword[tok.loc.0..=tok.loc.1]);
    }
}

#[test]
fn const_decl_char() -> Result<()> {
    let src = "c :: 'c';";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::Identifier, tok.ty);
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
    assert_eq!(Type::Identifier, tok.ty);
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
    let src = "main :: fn(x: int, y: uint) void {\n\t printf(\"Hello World\");\n};";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Identifier, tok.ty);
    assert_eq!("main", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::DoubleColon, tok.ty);
    assert_eq!("::", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordFn, tok.ty);
    assert_eq!("fn", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Identifier, tok.ty);
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
    assert_eq!(Type::Identifier, tok.ty);
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
    assert_eq!(Type::Identifier, tok.ty);
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
    let src = "fn(x: int, y: uint) void";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordFn, tok.ty);
    assert_eq!("fn", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Identifier, tok.ty);
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
    assert_eq!(Type::Identifier, tok.ty);
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
    assert_eq!(Type::Identifier, tok.ty);
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
fn import_with_as() {
    let src = "import \"stdio.h\" as std;";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordImport, tok.ty);
    assert_eq!("import", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::StringLiteral, tok.ty);
    assert_eq!("stdio.h", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordAs, tok.ty);
    assert_eq!("as", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::Identifier, tok.ty);
    assert_eq!("std", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::SemiColon, tok.ty);
    assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);
}

#[test]
fn import_no_as() {
    let src = "import \"stdio.h\";";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordImport, tok.ty);
    assert_eq!("import", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::StringLiteral, tok.ty);
    assert_eq!("stdio.h", &src[tok.loc.0..=tok.loc.1]);

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
    assert_eq!(Type::Identifier, tok.ty);
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
    assert_eq!(Type::Identifier, tok.ty);
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
    assert_eq!(Type::Identifier, tok.ty);
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
    assert_eq!(Type::Identifier, tok.ty);
    assert_eq!("print", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Identifier, tok.ty);
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
    assert_eq!(Type::Identifier, tok.ty);
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
    assert_eq!(Type::Identifier, tok.ty);
    assert_eq!("print", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Identifier, tok.ty);
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
    assert_eq!(Type::Identifier, tok.ty);
    assert_eq!("i", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordIn, tok.ty);
    assert_eq!("in", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::Identifier, tok.ty);
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
    assert_eq!(Type::Identifier, tok.ty);
    assert_eq!("print", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Identifier, tok.ty);
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
    assert_eq!(Type::Identifier, tok.ty);
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
    assert_eq!(Type::Identifier, tok.ty);
    assert_eq!("print", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::OpenParen, tok.ty);
    assert_eq!("(", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Identifier, tok.ty);
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

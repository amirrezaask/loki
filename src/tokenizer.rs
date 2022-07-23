use anyhow::Result;

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
    KeywordUint,
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
            "as", "var", "const", "if", "switch", "goto", "for", "while", "continue", "break",
            "import", "fn", "return", "true", "false", "enum", "else", "bool", "struct", "union",
            "void", "int", "uint", "string", "float", "char",
        ]
    }
    fn from_str(s: &str) -> Self {
        println!("in from_str type: {}", s);
        match s {
            "as" => Self::KeywordAs,
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
            "int" => Self::KeywordInt,
            "uint" => Self::KeywordUint,
            "string" => Self::StringLiteral,
            "float" => Self::KeywordFloat,
            "char" => Self::KeywordChar,
            _ => Self::Identifier,
        }
    }
}
#[derive(Debug)]
pub struct Token {
    ty: Type,
    loc: (usize, usize),
}

impl Token {
    pub fn new(ty: Type, loc: (usize, usize)) -> Self {
        Self { ty, loc }
    }
}

struct Tokenizer {
    src: Vec<char>,
    cur: usize,
    state: State,
}

#[derive(Debug)]
enum State {
    Start,
    InStringLiteral(usize),
    InCharLiteral,
    Integer(usize),
    IdentifierOrKeyword(usize),
    SawEqual,
    SawBang,
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

impl Tokenizer {
    pub fn new(src: &str) -> Self {
        Tokenizer {
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
        println!(
            "emitting current token idx {:?}, state: {:?}",
            self.cur, self.state
        );
        match self.state {
            State::Integer(start) => {
                self.state = State::Start;
                return Token::new(Type::UnsignedInt, (start, self.cur - 1));
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
            _ => {
                unreachable!();
            }
        }
    }

    pub fn next(&mut self) -> Result<Token> {
        loop {
            println!("@state: {:?}", self.state);

            if self.eof() {
                return Ok(self.emit_current_token());
            }
            println!("@current_char: {}", self.current_char());
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
                State::SawColon => match self.current_char() {
                    ':' => {
                        self.state = State::Start;
                        self.forward_char();
                        return Ok(Token::new(Type::DoubleColon, (self.cur - 2, self.cur - 1)));
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
                    ' ' | '\t' | '\n' | '\r' | ':' | ';' | '(' | ')' | ',' | '+' | '-' => {
                        return Ok(self.emit_current_token());
                    }
                    _ => {
                        self.forward_char();
                        continue;
                    }
                },
                State::Integer(_) => match self.current_char() {
                    ' ' | '\t' | '\n' | '\r' | ';' | ')' | '(' => {
                        return Ok(self.emit_current_token());
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
                    _ => {
                        let tok = Token::new(Type::Plus, (self.cur - 1, self.cur));
                        self.state = State::Start;
                        return Ok(tok);
                    }
                },

                State::SawMinus => match self.current_char() {
                    '+' => {
                        let tok = Token::new(Type::DoubleMinus, (self.cur - 1, self.cur));
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
                _ => {
                    unreachable!();
                }
            }
        }
    }
}

#[test]
fn integers() {
    let src = "123";
    let mut tokenizer = Tokenizer::new(src);

    let num = tokenizer.next();

    assert!(num.is_ok());
    let num = num.unwrap();
    println!("{:?}", num);
    assert_eq!("123", &src[num.loc.0..=num.loc.1]);
}

#[test]
fn keywords() {
    for keyword in Type::to_vec_str() {
        let mut tokenizer = Tokenizer::new(keyword);

        let tok = tokenizer.next();

        assert!(tok.is_ok());
        let tok = tok.unwrap();
        println!("{:?}", tok);
        assert_eq!(&keyword[0..keyword.len()], &keyword[tok.loc.0..=tok.loc.1]);
    }
}

#[test]
fn const_decl_with_type() {
    let src = "const f: int = 12;";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordConst, tok.ty);
    assert_eq!("const", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!("f", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::Colon, tok.ty);
    assert_eq!(":", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordInt, tok.ty);
    assert_eq!("int", &src[tok.loc.0..=tok.loc.1]);

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

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::SemiColon, tok.ty);
    assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);
}

#[test]
fn const_decl_no_type() {
    let src = "const f = 12;";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    println!("%{:?}", tok);
    assert_eq!(Type::KeywordConst, tok.ty);
    assert_eq!("const", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    println!("%{:?}", tok);
    assert_eq!("f", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    println!("%{:?}", tok);
    assert_eq!(Type::Equal, tok.ty);
    assert_eq!("=", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    println!("%{:?}", tok);
    assert_eq!(Type::UnsignedInt, tok.ty);
    assert_eq!("12", &src[tok.loc.0..=tok.loc.1]);
}

#[test]
fn const_decl_fn() {
    let src = "const main = fn(x: int, y: uint) void {\n\t printf(\"Hello World\");\n};";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordConst, tok.ty);
    assert_eq!("const", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Identifier, tok.ty);
    assert_eq!("main", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::Equal, tok.ty);
    assert_eq!("=", &src[tok.loc.0..=tok.loc.1]);

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
fn var_decl_with_type() {
    let src = "var f: int = 12;";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordVar, tok.ty);
    assert_eq!("var", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!("f", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::Colon, tok.ty);
    assert_eq!(":", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordInt, tok.ty);
    assert_eq!("int", &src[tok.loc.0..=tok.loc.1]);

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

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    assert_eq!(Type::SemiColon, tok.ty);
    assert_eq!(";", &src[tok.loc.0..=tok.loc.1]);
}

#[test]
fn var_decl_no_type() {
    let src = "var f = 12;";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!(Type::KeywordVar, tok.ty);
    assert_eq!("var", &src[tok.loc.0..=tok.loc.1]);

    let tok = tokenizer.next();
    let tok = tok.unwrap();
    assert_eq!("f", &src[tok.loc.0..=tok.loc.1]);

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
}

#[test]
fn strings() {
    let src = "\"amirreza\"";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    println!("{:?}", tok);
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
fn for_each() {}

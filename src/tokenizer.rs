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

    Char,
    UnsignedInt,
    Float,
    StringLiteral,
}

impl Type {
    fn to_vec_str() -> Vec<&'static str> {
        vec![
            "if ",
            "switch ",
            "goto ",
            "for ",
            "while ",
            "continue ",
            "break ",
            "import ",
            "fn ",
            "return ",
            "true ",
            "false ",
            "enum ",
            "else ",
            "bool ",
            "struct ",
            "union ",
            "void ",
            "int ",
            "uint ",
            "string ",
            "float ",
            "char ",
        ]
    }
    fn from_str(s: &str) -> Self {
        println!("in from_str type: {}", s);
        match s {
            "if" => Self::KeywordIf,
            "for" => Self::KeywordFor,
            "true" => Self::KeywordTrue,
            "false" => Self::KeywordFalse,
            "switch" => Self::KeywordSwitch,
            "goto" => Self::KeywordGoto,
            "for" => Self::KeywordFor,
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
            "uint" => Self::UnsignedInt,
            "string" => Self::StringLiteral,
            "float" => Self::KeywordFloat,
            "char" => Self::KeywordChar,
            _ => {
                unreachable!();
            }
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
}

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

    fn at_eof(&mut self, state: State) -> Token {
        match state {
            State::Integer(start) => {}
            _ => {
                unreachable!();
            }
        }
    }

    pub fn next(&mut self) -> Result<Token> {
        let mut state = State::Start;
        loop {
            if (self.eof()) {
                return Ok(self.at_eof(state));
            }
            match state {
                State::Start => {
                    match self.current_char() {
                        '{' => return Ok(Token::new(Type::OpenBrace, (self.cur, self.cur))),
                        '}' => return Ok(Token::new(Type::CloseBrace, (self.cur, self.cur))),
                        '(' => return Ok(Token::new(Type::OpenParen, (self.cur, self.cur))),
                        ')' => return Ok(Token::new(Type::CloseParen, (self.cur, self.cur))),
                        '[' => return Ok(Token::new(Type::OpenBracket, (self.cur, self.cur))),
                        ']' => return Ok(Token::new(Type::CloseBracket, (self.cur, self.cur))),
                        '<' => {
                            state = State::SawLeftAngleBracket;
                            self.forward_char();
                            continue;
                        }
                        '>' => {
                            state = State::SawRightAngleBracket;
                            self.forward_char();
                            continue;
                        }
                        '1'..='9' | '0' => {
                            state = State::Integer(self.cur);
                            self.forward_char();
                            continue;
                        }
                        '=' => {
                            state = State::SawEqual;
                            self.forward_char();
                            continue;
                        }
                        '"' => {
                            state = State::InStringLiteral(self.cur);
                            self.forward_char();
                            continue;
                        }
                        '%' => {
                            state = State::SawPercent;
                            self.forward_char();
                            continue;
                        }
                        '+' => {
                            state = State::SawPlus;
                            self.forward_char();
                            continue;
                        }
                        '-' => {
                            state = State::SawMinus;
                            self.forward_char();
                            continue;
                        }
                        '/' => {
                            state = State::SawSlash;
                            self.forward_char();
                            continue;
                        }
                        '*' => {
                            state = State::SawAstrix;
                            self.forward_char();
                            continue;
                        }
                        _ => {
                            state = State::IdentifierOrKeyword(self.cur);
                            self.forward_char();
                            continue;
                        }
                        ':' => {
                            state = State::SawColon;
                            self.forward_char();
                            continue;
                        }

                        ';' => {
                            state = State::Start;
                            self.forward_char();
                            return Ok(Token::new(Type::SemiColon, (self.cur - 1, self.cur - 1)));
                        }
                    };
                }
                State::SawEqual => match self.current_char() {
                    '=' => {
                        state = State::Start;
                        return Ok(Token::new(Type::DoubleEqual, (self.cur - 1, self.cur)));
                    }
                    _ => {
                        state = State::Start;
                        continue;
                    }
                },
                State::InStringLiteral(start) => match self.current_char() {
                    '"' => {
                        state = State::Start;
                        let tok = Token::new(Type::StringLiteral, (start + 1, self.cur));
                        return Ok(tok);
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
                        state = State::Start;
                        return Ok(Token::new(Type::LeftAngle, (self.cur - 1, self.cur - 1)));
                    }
                },
                State::SawRightAngleBracket => match self.current_char() {
                    '=' => {
                        let tok = Token::new(Type::GreaterEqual, (self.cur - 1, self.cur));
                        self.forward_char();
                        state = State::Start;
                        return Ok(tok);
                    }
                    _ => {
                        state = State::Start;
                        return Ok(Token::new(Type::RightAngle, (self.cur - 1, self.cur - 1)));
                    }
                },

                State::IdentifierOrKeyword(start) => match self.current_char() {
                    ' ' | '\t' | '\n' | '\r' => {
                        state = State::Start;

                        let ident_or_keyword: String =
                            self.src[start..self.cur].to_vec().into_iter().collect();

                        self.forward_char();
                        return Ok(Token::new(
                            Type::from_str(&ident_or_keyword),
                            (start, self.cur - 1),
                        ));
                    }
                    _ => {
                        self.forward_char();
                        continue;
                    }
                },
                State::Integer(start) => match self.current_char() {
                    ' ' | '\t' | '\n' | '\r' => {
                        state = State::Start;
                        self.forward_char();
                        return Ok(Token::new(Type::UnsignedInt, (start, self.cur - 1)));
                    }
                    _ => {
                        self.forward_char();
                        continue;
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
    let src = "123 ";
    let mut tokenizer = Tokenizer::new(src);

    let num = tokenizer.next();

    assert!(num.is_ok());
    let num = num.unwrap();
    println!("{:?}", num);
    assert_eq!("123", &src[num.loc.0..num.loc.1]);
}

#[test]
fn keywords() {
    for keyword in Type::to_vec_str() {
        let mut tokenizer = Tokenizer::new(keyword);

        let tok = tokenizer.next();

        assert!(tok.is_ok());
        let tok = tok.unwrap();
        println!("{:?}", tok);
        assert_eq!(
            &keyword[0..keyword.len() - 1],
            &keyword[tok.loc.0..tok.loc.1]
        );
    }
}
#[test]
fn strings() {
    let src = "\"amirreza\"";
    let mut tokenizer = Tokenizer::new(src);

    let tok = tokenizer.next();

    assert!(tok.is_ok());
    let tok = tok.unwrap();
    println!("{:?}", tok);
    assert_eq!("amirreza", &src[tok.loc.0..tok.loc.1]);
}

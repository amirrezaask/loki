#![allow(dead_code)]

mod expr;
#[cfg(test)]
mod tests;
pub use expr::expr;

#[derive(Clone, Debug, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Div,
    Mod,
    Multiply,
    Equality,
    Lesser,
    Greater,
    LesserEq,
    GreaterEq,
}

impl Operator {
    pub fn from_char(c: String) -> Self {
        match c.as_ref() {
            "+" => Self::Plus,
            "-" => Self::Minus,
            "/" => Self::Div,
            "%" => Self::Mod,
            "*" => Self::Multiply,
            "<" => Self::Lesser,
            ">" => Self::Greater,
            ">=" => Self::GreaterEq,
            "<=" => Self::LesserEq,
            _ => panic!(),
        } 
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Operation {
    pub lhs: Node,
    pub op: Operator,
    pub rhs: Node,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    IntTy,
    UintTy,
    FloatTy,
    CharTy,
    StringTy,
    BooleanTy,
    VoidTy,
    Char(char),
    Operator(Operator),
    Operation(Box<Operation>),
    Uint(usize),
    Int(isize),
    Float(f64),
    Str(String),
    Keyword(String),
    Ident(String),
    Bool(bool),
    List(Vec<Node>),
    Decl(Box<Node>, Box<Option<Node>>, Box<Node>),
    Application(Box<Application>),
    StructTy(Vec<IdentAndTy>),
    Return(Box<Node>),
    Inc(Box<Node>),
    Dec(Box<Node>),
    FnDef(Box<FnDef>),
    FnTy(Box<FnTy>),
    ArrayTy(Box<ArrayTy>),
    Stmt(Box<Node>),
    Block(Vec<Node>),
    If(Box<If>),
    For(Box<For>),
    While(Box<While>),
    Empty,
}

impl Node {
    fn primitive(self) -> Self {
        match &self {
            Node::Ident(name) => match name.as_ref() {
                "int" => Self::IntTy,
                "uint" => Self::UintTy,
                "float" => Self::FloatTy,
                "string" => Self::StringTy,
                "char" => Self::CharTy,
                "bool" => Self::BooleanTy,
                "void" => Self::VoidTy,
                _ => self,
            },
            _ => self,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct IdentAndTy {
    pub ident: Node,
    pub ty: Node,
}

#[derive(Clone, Debug, PartialEq)]
pub struct For {
    pub init: Node,
    pub cond: Node,
    pub cont: Node,
    pub body: Node,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnDef {
    // FnTy
    pub ty: FnTy,
    pub block: Node,
}

#[derive(Clone, Debug, PartialEq)]
pub struct While {
    pub cond: Node,
    pub block: Node,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnTy {
    pub args: Vec<IdentAndTy>,
    pub return_ty: Node,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Application {
    pub name: Node,
    pub args: Vec<Node>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArrayTy {
    size: Option<Node>,
    inner_ty: Node,
}
#[derive(Clone, Debug, PartialEq)]
pub struct If {
    cond: Node,
    block: Node,
}

struct Decl {
    name: Node,
    ty: Option<Node>,
    value: Node,
}

impl Decl {
    fn new(name: Node, ty: Option<Node>, value: Node) -> Self {
        return Self { name, ty, value };
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseErr {
    // unexpected (expected, found, location)
    Unexpected(String, String, u64),
    Unknown(String),
}
impl std::fmt::Display for ParseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unexpected(_, _, _) => f.write_fmt(format_args!("{:?}", self)),
            Self::Unknown(msg) => f.write_fmt(format_args!("{}", msg)),
            _ => unreachable!(),
        }
    }
}
impl std::error::Error for ParseErr {}

type ParseResult = Result<(String, Node), ParseErr>;

fn any_of(parsers: Vec<impl Fn(String) -> ParseResult>) -> impl Fn(String) -> ParseResult {
    return move |input: String| {
        for parser in parsers.iter() {
            match parser(input.clone()) {
                Ok((remaining, parsed)) => return Ok((remaining, parsed)),
                Err(err) => continue,
            }
        }
        return Err(ParseErr::Unexpected("".to_string(), "".to_string(), 0));
    };
}

fn zero_or_more(parser: impl Fn(String) -> ParseResult) -> impl Fn(String) -> ParseResult {
    return move |mut input: String| {
        let mut result = Vec::new();
        while let Ok((remains, parsed)) = parser(input.clone()) {
            input = remains;
            result.push(parsed);
        }
        return Ok((input.clone(), Node::List(result)));
    };
}

fn zero_or_one(parser: impl Fn(String) -> ParseResult) -> impl Fn(String) -> ParseResult {
    return move |mut input: String| {
        if let Ok((remains, parsed)) = parser(input.clone()) {
            return Ok((remains, Node::Char('-')));
        }
        return Ok((input, Node::Empty));
    };
}

fn one_or_more(parser: impl Fn(String) -> ParseResult) -> impl Fn(String) -> ParseResult {
    return move |mut input: String| {
        let mut result = Vec::new();

        // we should first try to get one, if can't it's a parse error
        match parser(input.clone()) {
            Ok((remains, parsed)) => {
                input = remains;
                result.push(parsed);
            }
            Err(err) => {
                return Err(err);
            }
        }
        while let Ok((remains, parsed)) = parser(input.clone()) {
            input = remains;
            result.push(parsed);
        }
        return Ok((input.clone(), Node::List(result)));
    };
}
fn any() -> impl Fn(String) -> ParseResult {
    return move |input: String| {
        if input.len() < 1 {
            return ParseResult::Err(ParseErr::Unexpected(
                "any".to_string(),
                "nothing".to_string(),
                0,
            ));
        }

        return ParseResult::Ok((
            input[1..].to_string(),
            Node::Char(input.chars().nth(0).unwrap()),
        ));
    };
}
fn parse_char(c: char) -> impl Fn(String) -> ParseResult {
    return move |input: String| {
        if input.len() < 1 {
            return ParseResult::Err(ParseErr::Unexpected(
                c.to_string(),
                "nothing".to_string(),
                0,
            ));
        }
        if input.chars().nth(0).unwrap() == c.clone() {
            return ParseResult::Ok((input[1..].to_string(), Node::Char(c)));
        }
        return ParseResult::Err(ParseErr::Unexpected(
            c.to_string(),
            input.chars().nth(0).unwrap().to_string(),
            0,
        ));
    };
}
fn _char(input: String) -> ParseResult {
    let (remains, _) = parse_char('\'')(input)?;
    let c = Node::Char(remains.chars().nth(0).unwrap());
    let (remains, _) = parse_char('\'')(remains[1..].to_string())?;
    Ok((remains, c))
}

fn string(input: String) -> ParseResult {
    let (remains, _) = parse_char('"')(input)?;
    let mut end: usize = 0;
    for (idx, c) in remains.chars().enumerate() {
        if c == '"' {
            if remains.chars().nth(idx - 1).is_some() {
                if remains.chars().nth(idx - 1).unwrap() != '\\' {
                    end = idx;
                }
            }
        }
    }
    if end != 0 {
        return Ok((
            remains[end + 1..].to_string(),
            Node::Str(remains[..end].to_string()),
        ));
    } else {
        return Err(ParseErr::Unknown("cannot find end of string".to_string()));
    }
}

fn keyword(word: String) -> impl Fn(String) -> ParseResult {
    return move |mut input: String| {
        let word_chars = word.chars();
        for c in word_chars {
            match parse_char(c)(input) {
                Ok((remains, _)) => input = remains,
                Err(err) => return Err(err),
            }
        }
        return Ok((input, Node::Keyword(word.clone())));
    };
}

fn any_whitespace() -> impl Fn(String) -> ParseResult {
    let sp = parse_char(' ');
    let tab = parse_char('\t');
    let newline = parse_char('\n');
    return any_of(vec![sp, tab, newline]);
}

fn whitespace() -> impl Fn(String) -> ParseResult {
    return zero_or_more(any_whitespace());
}

fn parse_chars(chars: &str) -> impl Fn(String) -> ParseResult {
    let parsers = chars.chars().map(|c| parse_char(c)).collect();
    return any_of(parsers);
}

fn digit() -> impl Fn(String) -> ParseResult {
    return parse_chars("0123456789");
}

fn ident(input: String) -> ParseResult {
    match one_or_more(parse_chars(
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
    ))(input)
    {
        Ok((remains, Node::List(chars_parse_objects))) => {
            let mut name = String::new();

            for po in chars_parse_objects {
                match po {
                    Node::Char(c) => name.push(c),
                    _ => {
                        return Err(ParseErr::Unexpected(
                            "a char".to_string(),
                            format!("{:?}", po),
                            0,
                        ))
                    }
                }
            }
            return Ok((remains, Node::Ident(name)));
        }
        Ok((_, obj)) => {
            return Err(ParseErr::Unexpected(
                "list of chars".to_string(),
                format!("{:?}", obj),
                0,
            ))
        }
        Err(err) => Err(err),
    }
}

fn fn_call(input: String) -> ParseResult {
    let (remains, obj) = ident(input)?;
    let mut identifier = "".to_string();
    match obj {
        Node::Ident(i) => identifier = i,
        _ => {
            return Err(ParseErr::Unexpected(
                "ident".to_string(),
                format!("{:?}", obj),
                0,
            ))
        }
    }
    let (mut remains, _) = whitespace()(remains)?;
    let (mut remains, _) = parse_char('(')(remains)?;
    let (mut remains, _) = whitespace()(remains)?;
    // we know it's a function call
    let mut args: Vec<Node> = Vec::new();
    if remains.chars().nth(0).is_some() && remains.chars().nth(0).unwrap() != ')' {
        loop {
            // fn(1,2,3,4)
            let ws_res = whitespace()(remains)?;
            remains = ws_res.0;
            let expr_res = expr(remains.clone())?;
            remains = expr_res.0;
            let obj = expr_res.1;
            args.push(obj);
            let ws_res = whitespace()(remains)?;
            remains = ws_res.0;
            //,2)
            let comma = parse_char(',')(remains.clone());
            if let Ok((r, _)) = comma {
                remains = r;
            } else {
                break;
            }
        }
    }
    let ws_res = whitespace()(remains)?;
    remains = ws_res.0;
    let close_paren_res = parse_char(')')(remains)?;
    remains = close_paren_res.0;

    return Ok((
        remains,
        Node::Application(Box::new(Application {
            name: Node::Ident(identifier),
            args: args,
        })),
    ));
}

fn semicolon(input: String) -> ParseResult {
    return parse_char(';')(input);
}

fn sequence(parsers: Vec<impl Fn(String) -> ParseResult>) -> impl Fn(String) -> ParseResult {
    return move |input: String| {
        let mut parsed = Vec::new();
        for p in parsers.iter() {
            let res = p(input.clone());
            match res {
                Ok((input, parsedObj)) => parsed.push(parsedObj),
                Err(e) => return Err(e),
            }
        }

        return Ok((input, Node::List(parsed)));
    };
}

fn uint(input: String) -> ParseResult {
    match one_or_more(digit())(input) {
        Ok((remains, Node::List(_digits))) => {
            let mut number = String::new();
            for d in _digits {
                match d {
                    Node::Char(c) => {
                        number.push(c);
                    }
                    _ => unreachable!(),
                }
            }
            let number: usize = number.parse().unwrap();
            Ok((remains, Node::Uint(number)))
        }
        Err(err) => return Err(err),
        _ => unreachable!(),
    }
}

fn int(mut input: String) -> ParseResult {
    // int = sign uint
    let sign = zero_or_one(parse_char('-'));
    let mut with_sign: i8 = 1;
    if let (remains, Node::Char('-')) = sign(input.clone())? {
        input = remains;
        with_sign = -1;
    } else {
        with_sign = 1;
    }

    let (input, obj) = uint(input)?;
    match obj {
        Node::Uint(num) => return Ok((input, Node::Int(with_sign as isize * num as isize))),
        _ => Err(ParseErr::Unknown(format!(
            "expected a uint found {:?}",
            obj
        ))),
    }
}

fn _bool(input: String) -> ParseResult {
    let _true = keyword("true".to_string());
    let _false = keyword("false".to_string());
    let (remains, bool_parsed) = any_of(vec![_true, _false])(input)?;
    if let Node::Keyword(b) = bool_parsed {
        return Ok((remains, Node::Bool(b == "true")));
    } else {
        unreachable!()
    }
}

fn _struct(input: String) -> ParseResult {
    // struct { ident: type, }
    let (mut remains, _) = keyword("struct".to_string())(input)?;
    let (mut remains, _) = whitespace()(remains)?;
    let (mut remains, _) = parse_char('{')(remains)?;

    // we know it's a function call
    let mut idents_tys: Vec<IdentAndTy> = Vec::new();

    if remains.chars().nth(0).is_some() && remains.chars().nth(0).unwrap() != '}' {
        loop {
            let whitespace_res = whitespace()(remains.clone())?;
            remains = whitespace_res.0;

            let ident_res = ident(remains.clone())?;
            remains = ident_res.0;

            let ident_obj = ident_res.1;

            let whitespace_res = whitespace()(remains)?;
            remains = whitespace_res.0;

            let colon_res = parse_char(':')(remains.clone())?;
            remains = colon_res.0;

            let whitespace_res = whitespace()(remains)?;
            remains = whitespace_res.0;

            let type_res = expr(remains.clone())?;
            remains = type_res.0;

            let type_obj = type_res.1;
            idents_tys.push(IdentAndTy {
                ident: ident_obj.clone(),
                ty: type_obj.primitive().clone(),
            });

            let whitespace_res = whitespace()(remains)?;
            remains = whitespace_res.0;

            let comma = parse_char(',')(remains.clone());
            if let Ok((r, _)) = comma {
                remains = r;
            } else {
                break;
            }
        }
    }
    let (mut remains, _) = parse_char('}')(remains)?;
    return Ok((remains, Node::StructTy(idents_tys)));
}
fn dec(input: String) -> ParseResult {
    let (remains, _) = whitespace()(input)?;
    let (remains, _) = keyword("dec".to_string())(remains)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, e) = expr(remains)?;
    Ok((remains, Node::Dec(Box::new(e))))
}
fn inc(input: String) -> ParseResult {
    let (remains, _) = whitespace()(input)?;
    let (remains, _) = keyword("inc".to_string())(remains)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, e) = expr(remains)?;
    Ok((remains, Node::Inc(Box::new(e))))
}
fn _return(input: String) -> ParseResult {
    let (remains, _) = whitespace()(input)?;
    let (remains, _) = keyword("return".to_string())(remains)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, e) = expr(remains)?;
    Ok((remains, Node::Return(Box::new(e))))
}

fn array(input: String) -> ParseResult {
    let (remains, _) = parse_char('[')(input)?;
    let (mut remains, ty) = expr(remains)?;
    let ty = ty.primitive();
    let semicolon_res = semicolon(remains.clone());
    let mut size: Option<Node> = None;
    match semicolon_res {
        Ok((r, Node::Char(';'))) => {
            let (r, size_obj) = expr(r)?;
            remains = r;
            size = Some(size_obj);
        }
        _ => (),
    };
    let (remains, _) = parse_char(']')(remains)?;
    return Ok((
        remains,
        Node::ArrayTy(Box::new(ArrayTy {
            size: size,
            inner_ty: ty,
        })),
    ));
}

fn statement(input: String) -> ParseResult {
    let parsers: Vec<fn(String) -> Result<(String, Node), ParseErr>> = vec![_for, decl, expr];
    let (remains, _) = whitespace()(input.clone())?;
    let (remains, stmt) = any_of(parsers)(remains)?;
    let (remains, _) = parse_char(';')(remains.clone())?;
    return Ok((remains, stmt));
}

fn block(input: String) -> ParseResult {
    return match zero_or_more(statement)(input)? {
        (remains, Node::List(items)) =>{
            let (remains, _) = whitespace()(remains)?;
            Ok((remains, Node::Block(items)))
        },
        (_remains, obj) => Err(ParseErr::Unexpected(
            "ParseObj::List".to_string(),
            format!("{:?}", obj),
            0,
        )),
    };
}

fn _if(input: String) -> ParseResult {
    let (remains, _) = keyword("if".to_string())(input)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, cond) = expr(remains)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, _) = parse_char('{')(remains)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, _block) = block(remains)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, _) = parse_char('}')(remains)?;
    return Ok((
        remains,
        Node::If(Box::new(If {
            cond: cond,
            block: _block,
        })),
    ));
}
fn _for(input: String) -> ParseResult {
    let (remains, _) = keyword("for".to_string())(input)?;
    let parsers: Vec<fn(String) -> ParseResult> = vec![_for_c, _for_while];
    return any_of(parsers)(remains);
}

fn _for_while(input: String) -> ParseResult {
    let (remains, _) = whitespace()(input)?;
    let (remains, cond) = expr(remains)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, _) = parse_char('{')(remains)?;
    let (remains, body) = block(remains)?;
    let (remains, _) = parse_char('}')(remains)?;
    return Ok((
        remains,
        Node::While(Box::new(While {
            cond: cond,
            block: body,
        })),
    ));
}

fn _for_c(input: String) -> ParseResult {
    let (remains, _) = whitespace()(input)?;
    let (remains, init) = decl(remains)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, _) = parse_char(';')(remains)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, cond) = expr(remains)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, _) = parse_char(';')(remains)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, cont) = expr(remains)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, _) = parse_char('{')(remains)?;
    let (remains, body) = block(remains)?;
    let (remains, _) = parse_char('}')(remains)?;
    return Ok((
        remains,
        Node::For(Box::new(For {
            init,
            cond,
            cont,
            body,
        })),
    ));
}
fn fn_ty(input: String) -> ParseResult {
    let (mut remains, _) = keyword("fn".to_string())(input)?;
    let (mut remains, _) = whitespace()(remains)?;
    let (mut remains, _) = parse_char('(')(remains)?;
    let mut args_tys: Vec<IdentAndTy> = Vec::new();
    if remains.chars().nth(0).is_some() && remains.chars().nth(0).unwrap() != ')' {
        loop {
            let whitespace_res = whitespace()(remains.clone())?;
            remains = whitespace_res.0;

            let ident_res = ident(remains.clone())?;
            remains = ident_res.0;

            let ident_obj = ident_res.1;

            let whitespace_res = whitespace()(remains)?;
            remains = whitespace_res.0;

            let colon_res = parse_char(':')(remains.clone())?;
            remains = colon_res.0;

            let whitespace_res = whitespace()(remains)?;
            remains = whitespace_res.0;

            let type_res = expr(remains.clone())?;
            remains = type_res.0;

            let type_obj = type_res.1;
            let type_obj = type_obj.primitive();
            args_tys.push(IdentAndTy {
                ident: ident_obj.clone(),
                ty: type_obj.primitive().clone(),
            });

            let whitespace_res = whitespace()(remains)?;
            remains = whitespace_res.0;

            let comma = parse_char(',')(remains.clone());
            if let Ok((r, _)) = comma {
                remains = r;
            } else {
                break;
            }
        }
    }
    let (remains, _) = whitespace()(remains)?;
    let (remains, _) = parse_char(')')(remains)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, return_ty) = expr(remains)?;
    let return_ty = return_ty.primitive();
    return Ok((
        remains,
        Node::FnTy(Box::new(FnTy {
            args: args_tys,
            return_ty: return_ty,
        })),
    ));
}

pub fn fn_def(input: String) -> ParseResult {
    let (remains, _ty) = fn_ty(input)?;
    let mut ty: Option<FnTy> = None;
    if let Node::FnTy(__ty) = _ty {
        ty = Some(*__ty);
    } else {
        unreachable!()
    }
    let (remains, _) = whitespace()(remains)?;
    let (remains, _) = parse_char('{')(remains)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, _block) = block(remains)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, _) = parse_char('}')(remains)?;
    return Ok((
        remains,
        Node::FnDef(Box::new(FnDef {
            ty: ty.unwrap(),
            block: _block,
        })),
    ));
}

fn float(input: String) -> ParseResult {
    // int parse_char('.') uint
    if let (remains, Node::Int(int_part)) = int(input)? {
        if let (remains, _) = parse_char('.')(remains)? {
            let parsed = uint(remains)?;
            if let (remains, Node::Uint(float_part)) = parsed {
                let float_str = format!("{}.{}", int_part, float_part);
                let float: f64 = float_str.parse().unwrap();
                Ok((remains, Node::Float(float)))
            } else {
                return Err(ParseErr::Unexpected(
                    "uint".to_string(),
                    format!("{:?}", parsed),
                    0,
                ));
            }
        } else {
            return Err(ParseErr::Unknown("not a float without a .".to_string()));
        }
    } else {
        return Err(ParseErr::Unknown("not a number at all".to_string()));
    }
}

fn decl(input: String) -> ParseResult {
    // ident: expr = expr;
    let (remains, _) = whitespace()(input.clone())?;
    let (remains, obj) = ident(remains)?;
    let mut identifier = "".to_string();
    match obj {
        Node::Ident(i) => identifier = i,
        _ => {
            return Err(ParseErr::Unexpected(
                "ident".to_string(),
                format!("{:?}", obj),
                0,
            ))
        }
    }
    let (mut remains, _) = whitespace()(remains)?;
    let mut ty: Option<Node> = None;
    let colon_res = parse_char(':')(remains.clone());
    match colon_res {
        Ok((r, Node::Char(':'))) => {
            let ty_res = expr(r)?;
            remains = ty_res.0;
            ty = Some(ty_res.1.primitive());
        }
        _ => {}
    }
    let (remains, _) = parse_char('=')(remains)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, e) = expr(remains)?;
    return Ok((
        remains,
        Node::Decl(Box::new(Node::Ident(identifier)), Box::new(ty), Box::new(e)),
    ));
}

pub fn module(input: String) -> ParseResult {
    return zero_or_more(statement)(input);
}

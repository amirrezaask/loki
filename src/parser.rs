#![allow(dead_code)]

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
    Import(Box<Import>),
    EnumTy(Vec<Node>),
    UnionTy(Vec<IdentAndTy>),
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
pub struct Import {
    pub path: Node,
    pub _as: Option<Node> 
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
pub enum Error {
    // unexpected (expected, found, location)
    Unexpected(String, String, u64),
    Unknown(String),
}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unexpected(exp, fou, _) => f.write_fmt(format_args!("expected '{}' found '{}'", exp, fou)),
            Self::Unknown(msg) => f.write_fmt(format_args!("{}", msg)),
            _ => unreachable!(),
        }
    }
}
impl std::error::Error for Error {}

type ParseResult = Result<(String, Node), Error>;

fn any_of(context: String, parsers: Vec<impl Fn(String) -> ParseResult>) -> impl Fn(String) -> ParseResult {
    return move |input: String| {
        let mut errs: Vec<String> = vec![];
        for parser in parsers.iter() {
            match parser(input.clone()) {
                Ok((remaining, parsed)) => return Ok((remaining, parsed)),
                Err(err) => errs.push(err.to_string()),
            }
        }
        return Err(Error::Unknown(format!("{} no parser matched: errs => {:?}", context, errs)));
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
        } else {
            return Ok((input, Node::Empty));
        }
    };
}

fn one_or_more(parser: impl Fn(String) -> ParseResult, report_errs: bool) -> impl Fn(String) -> ParseResult {
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
        loop {
            let resp = parser(input.clone());
            if let Ok((remains, parsed)) = resp {
                input = remains;
                result.push(parsed);
            } else if let Err(e) = resp {
                if report_errs {
                    println!("error: {}", e);
                }
                break;
            }

        } 
        return Ok((input.clone(), Node::List(result)));
    };
}
fn any() -> impl Fn(String) -> ParseResult {
    return move |input: String| {
        if input.len() < 1 {
            return ParseResult::Err(Error::Unexpected(
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
            return ParseResult::Err(Error::Unexpected(
                c.to_string(),
                "nothing".to_string(),
                0,
            ));
        }
        if input.chars().nth(0).unwrap() == c.clone() {
            return ParseResult::Ok((input[1..].to_string(), Node::Char(c)));
        }
        return ParseResult::Err(Error::Unexpected(
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
                    break;
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
        return Err(Error::Unknown("cannot find end of string".to_string()));
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
    return any_of(format!("any_whitespace"), vec![sp, tab, newline]);
}

fn whitespace() -> impl Fn(String) -> ParseResult {
    return zero_or_more(any_whitespace());
}

fn parse_chars(chars: &str) -> impl Fn(String) -> ParseResult {
    let parsers = chars.chars().map(|c| parse_char(c)).collect();
    return any_of(format!("parse_char: {}", chars), parsers);
}

fn digit() -> impl Fn(String) -> ParseResult {
    return parse_chars("0123456789");
}

fn ident(input: String) -> ParseResult {
    match one_or_more(parse_chars(
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
    ), false)(input)
    {
        Ok((remains, Node::List(chars_parse_objects))) => {
            let mut name = String::new();

            for po in chars_parse_objects {
                match po {
                    Node::Char(c) => name.push(c),
                    _ => {
                        return Err(Error::Unexpected(
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
            return Err(Error::Unexpected(
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
            return Err(Error::Unexpected(
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
    match one_or_more(digit(), false)(input) {
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
        _ => Err(Error::Unknown(format!(
            "expected a uint found {:?}",
            obj
        ))),
    }
}

fn _bool(input: String) -> ParseResult {
    let _true = keyword("true".to_string());
    let _false = keyword("false".to_string());
    let (remains, bool_parsed) = any_of(format!("boolean"), vec![_true, _false])(input)?;
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
            if remains.chars().nth(0).is_some() && remains.chars().nth(0).unwrap() == '}' {
                break
            }
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
    let parsers: Vec<fn(String) -> Result<(String, Node), Error>> = vec![_import, _for, decl, expr];
    let (remains, _) = whitespace()(input.clone())?;
    let (remains, stmt) = any_of("statement".to_string(), parsers)(remains)?;
    let (remains, _) = parse_char(';')(remains.clone())?;
    return Ok((remains, stmt));
}

fn _import(input: String) -> ParseResult {
    let (remains, _) = whitespace()(input)?;
    let (remains, _) = keyword("import".to_string())(remains)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, name) = string(remains)?;
    let (remains, _) = whitespace()(remains)?;
    Ok((remains, Node::Import(Box::new(Import {
        path: name,
        _as: None,
    }))))
}

fn block(input: String) -> ParseResult {
    return match one_or_more(statement, true)(input)? {
        (remains, Node::List(items)) =>{
            let (remains, _) = whitespace()(remains)?;
            Ok((remains, Node::Block(items)))
        },
        (_remains, obj) => Err(Error::Unexpected(
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
    return any_of("for".to_string(), parsers)(remains);
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

fn union(input: String) -> ParseResult {
    let (mut remains, _) = whitespace()(input)?;
    let (mut remains, _) = keyword("union".to_string())(remains)?;
    let (mut remains, _) = whitespace()(remains)?;
    let (mut remains, _) = parse_char('{')(remains)?;

    // we know it's a function call
    let mut idents_tys: Vec<IdentAndTy> = Vec::new();

    if remains.chars().nth(0).is_some() && remains.chars().nth(0).unwrap() != '}' {
        loop {
            let whitespace_res = whitespace()(remains.clone())?;
            remains = whitespace_res.0;
            if remains.chars().nth(0).is_some() && remains.chars().nth(0).unwrap() == '}' {
                break
            }
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
    return Ok((remains, Node::UnionTy(idents_tys)));

}

fn _enum(input: String) -> ParseResult {
    let (remains, _) = whitespace()(input)?;
    let (remains, _) = keyword("enum".to_string())(remains)?;
    let (mut remains, _) = whitespace()(remains)?;
    let (mut remains, _) = parse_char('{')(remains)?;
    let (mut remains, _) = whitespace()(remains)?;

    // we know it's a function call
    let mut variants: Vec<Node> = Vec::new();

    if remains.chars().nth(0).is_some() && remains.chars().nth(0).unwrap() != '}' {
        loop {
            let whitespace_res = whitespace()(remains.clone())?;
            remains = whitespace_res.0;
            if remains.chars().nth(0).is_some() && remains.chars().nth(0).unwrap() == '}' {
                break
            }

            let ident_res = ident(remains.clone())?;
            remains = ident_res.0;

            let ident_obj = ident_res.1;

            let whitespace_res = whitespace()(remains)?;
            remains = whitespace_res.0;

            variants.push(ident_obj);

            let comma = parse_char(',')(remains.clone());
            if let Ok((r, _)) = comma {
                remains = r;
            } else {
                break;
            }
        }
    }
    let (remains, _) = parse_char('}')(remains)?;
    return Ok((remains, Node::EnumTy(variants)));

}
fn fn_ty(input: String) -> ParseResult {
    let (remains, _) = whitespace()(input)?;
    let (mut remains, _) = keyword("fn".to_string())(remains)?;
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
                return Err(Error::Unexpected(
                    "uint".to_string(),
                    format!("{:?}", parsed),
                    0,
                ));
            }
        } else {
            return Err(Error::Unknown("not a float without a .".to_string()));
        }
    } else {
        return Err(Error::Unknown("not a number at all".to_string()));
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
            return Err(Error::Unexpected(
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

pub fn expr(input: String) -> ParseResult {
    A(input)
}

fn A(input: String) -> ParseResult {
    let (remains, lhs) = B(input)?;
    let (remains, _) = whitespace()(remains)?;
    match add_minus(remains.clone()) {
        Ok((remains, n)) => match n {
            Node::Char(c) => {
                let operator = Operator::from_char(c.to_string());
                let (remains, rhs) = B(remains)?;
                Ok((
                    remains,
                    Node::Operation(Box::new(Operation {
                        lhs: lhs,
                        op: operator,
                        rhs: rhs,
                    })),
                ))
            },
            Node::Keyword(k) => {
                let operator = Operator::from_char(k);
                let (remains, rhs) = B(remains)?;
                Ok((
                    remains,
                    Node::Operation(Box::new(Operation {
                        lhs: lhs,
                        op: operator,
                        rhs: rhs,
                    })),
                ))
 
            }
            _ => unreachable!()
        },
        Err(e) => Ok((remains, lhs)),
        _ => unreachable!(),
    }
}
fn B(input: String) -> ParseResult {
    let (remains, _) = whitespace()(input)?;
    let (remains, lhs) = C(remains)?;
    let (remains, _) = whitespace()(remains)?;
    match mul_div_mod(remains.clone()) {
        Ok((remains, Node::Char(c))) => {
            let operator = Operator::from_char(c.to_string());
            let (remains, rhs) = C(remains)?;
            Ok((
                remains,
                Node::Operation(Box::new(Operation {
                    lhs: lhs,
                    op: operator,
                    rhs: rhs,
                })),
            ))
        }
        Err(e) => Ok((remains, lhs)),
        _ => unreachable!(),
    }
}
fn C(input: String) -> ParseResult {
    let parsers: Vec<fn(String) -> Result<(String, Node), Error>> = vec![
        float,
        uint,
        int,
        _bool,
        string,
        _char,
        _if,
        fn_def,
        _enum,
        union,
        _struct,
        inc,
        dec,
        _return,
        fn_call,
        ident,
        array,
        inside_paren,
    ];
    return any_of("expr".to_string(), parsers)(input);
}

fn inside_paren(input: String) -> ParseResult {
    let (remains, _) = parse_char('(')(input)?;
    let (remains, e) = expr(remains)?;
    let (remains, _) = parse_char(')')(remains)?;
    Ok((remains, e))
}

fn comparisons(input: String) -> ParseResult {
    let p2 = vec![keyword("<=".to_string()), keyword(">=".to_string())];
    return any_of("<=>=".to_string(), p2)(input);
}

fn add_minus(input: String) -> ParseResult {
    match comparisons(input.clone()) {
        Ok((remains, p)) => Ok((remains, p)),
        Err(_) => any_of("+-<>".to_string(), vec![
            parse_char('+'),
            parse_char('-'),
            parse_char('<'),
            parse_char('>'),
        ])(input.clone()),
    }
}

fn mul_div_mod(input: String) -> ParseResult {
    return any_of("*/%".to_string(), vec![parse_char('*'), parse_char('/'), parse_char('%')])(input);
}

pub fn module(input: String) -> ParseResult {
    block(input)
}


use super::*;
// use pretty_assertions::assert_eq;

#[test]
fn test_decl() {
    let decl_res = decl("a = false".to_string());
    assert!(decl_res.is_ok());
    let none: Box<Option<Node>> = Box::new(None);
    if let (_, Node::Decl(name, none, be)) = decl_res.unwrap() {
        assert_eq!(name, Box::new(Node::Ident("a".to_string())));
        assert_eq!(be, Box::new(Node::Bool(false)));
    } else {
        assert!(false);
    }

    let decl_res = decl("a = -2".to_string());
    assert!(decl_res.is_ok());
    let none: Box<Option<Node>> = Box::new(None);
    if let (_, Node::Decl(name, none, be)) = decl_res.unwrap() {
        assert_eq!(name, Box::new(Node::Ident("a".to_string())));
        assert_eq!(be, Box::new(Node::Int(-2)));
    } else {
        assert!(false);
    }

    let decl_res = decl("a = \"amirreza\"".to_string());
    assert!(decl_res.is_ok());

    let none: Box<Option<Node>> = Box::new(None);
    if let (_, Node::Decl(name, none, be)) = decl_res.unwrap() {
        assert_eq!(name, Box::new(Node::Ident("a".to_string())));
        assert_eq!(be, Box::new(Node::Str("amirreza".to_string())));
    } else {
        assert!(false);
    }

    let decl_res = decl("a = 2".to_string());
    assert!(decl_res.is_ok());
    let none: Box<Option<Node>> = Box::new(None);
    if let (_, Node::Decl(name, none, be)) = decl_res.unwrap() {
        assert_eq!(name, Box::new(Node::Ident("a".to_string())));
        assert_eq!(be, Box::new(Node::Uint(2)));
    } else {
        assert!(false);
    }
    let decl_res = decl(
        "sum = fn() void {
    return 1;
};"
            .to_string(),
    );
    assert!(decl_res.is_ok());
    let none: Box<Option<Node>> = Box::new(None);
    if let (_, Node::Decl(name, none, f)) = decl_res.unwrap() {
        assert_eq!(name, Box::new(Node::Ident("sum".to_string())));
        assert_eq!(
            f,
            Box::new(Node::FnDef(Box::new(FnDef {
                ty: FnTy {
                    args: vec![],
                    return_ty: Node::VoidTy,
                },
                block: Node::Block(vec![Node::Return(Box::new(Node::Uint(1)))]),
            }))),
        );
    } else {
        assert!(false);
    }

    // enums
    let decl_res = decl(" Human = enum { Man, Woman };".to_string());
    assert!(decl_res.is_ok());
    let none: Box<Option<Node>> = Box::new(None);
    if let (_, Node::Decl(name, none, f)) = decl_res.unwrap() {
        assert_eq!(name, Box::new(Node::Ident("Human".to_string())));
        assert_eq!(
            f,
            Box::new(Node::EnumTy(vec![
                Node::Ident("Man".to_string()),
                Node::Ident("Woman".to_string()),
            ])),
        );
    } else {
        assert!(false);
    }

    let decl_res = decl("f = fn() void {\n\tprintln(\"Salam donya!\");\n}".to_string());
    assert!(decl_res.is_ok());
    let none: Box<Option<Node>> = Box::new(None);
    if let (_, Node::Decl(name, none, f)) = decl_res.unwrap() {
        assert_eq!(name, Box::new(Node::Ident("f".to_string())));
        assert_eq!(
            f,
            Box::new(Node::FnDef(Box::new(FnDef {
                ty: FnTy {
                    args: vec![],
                    return_ty: Node::VoidTy,
                },
                block: Node::Block(vec![Node::Application(Box::new(Application {
                    name: Node::Ident("println".to_string()),
                    args: vec![Node::Str("Salam donya!".to_string())],
                }))]),
            }))),
        );
    } else {
        assert!(false);
    }
}

#[test]
fn test_parse_single_digit() {
    assert_eq!(
        digit()("1AB".to_string()),
        ParseResult::Ok(("AB".to_string(), Node::Char('1'), ))
    );
}

#[test]
fn test_parse_float() {
    assert_eq!(
        float("4.2AB".to_string()),
        ParseResult::Ok(("AB".to_string(), Node::Float(4.2)))
    );
}

#[test]
fn test_parse_char() {
    assert_eq!(
        _char("'c'".to_string()),
        ParseResult::Ok(("".to_string(), Node::Char('c')))
    );
}

#[test]
fn test_parse_string() {
    assert_eq!(
        string("\"amirreza\" abc".to_string()),
        ParseResult::Ok((" abc".to_string(), Node::Str("amirreza".to_string())))
    );
}

#[test]
fn test_parse_int() {
    assert_eq!(
        int("-1234AB".to_string()),
        ParseResult::Ok(("AB".to_string(), Node::Int(-1234)))
    );
    assert_eq!(
        int("1234AB".to_string()),
        ParseResult::Ok(("AB".to_string(), Node::Int(1234)))
    );
}

#[test]
fn test_parse_uint() {
    assert_eq!(
        uint("1234AB".to_string()),
        ParseResult::Ok(("AB".to_string(), Node::Uint(1234)))
    );
}

#[test]
fn test_parse_keyword() {
    assert_eq!(
        keyword("struct".to_string())("struct name".to_string()),
        ParseResult::Ok((" name".to_string(), Node::Keyword("struct".to_string())))
    );
}

#[test]
fn test_parse_fn_ty() {
    assert_eq!(
        fn_ty("fn(a: int) string".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::FnTy(Box::new(FnTy {
                args: vec![IdentAndTy {
                    ident: Node::Ident("a".to_string()),
                    ty: Node::IntTy,
                }],
                return_ty: Node::StringTy,
            }))
        ))
    )
}

#[test]
fn test_parse_fn_def() {
    assert_eq!(
        fn_def("fn(a: int) string {\n\tprint(a);\n\t }".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::FnDef(Box::new(FnDef {
                ty: FnTy {
                    args: vec![IdentAndTy {
                        ident: Node::Ident("a".to_string()),
                        ty: Node::IntTy,
                    }],
                    return_ty: Node::StringTy,
                },
                block: Node::Block(vec![Node::Application(Box::new(Application {
                    name: Node::Ident("print".to_string()),
                    args: vec![Node::Ident("a".to_string())],
                }))]),
            }))
        ))
    );
}

#[test]
fn test_parse_ident() {
    assert_eq!(
        ident("name".to_string()),
        ParseResult::Ok(("".to_string(), Node::Ident("name".to_string())))
    );
    assert_eq!(
        ident("name_str".to_string()),
        ParseResult::Ok(("".to_string(), Node::Ident("name_str".to_string()), ))
    );
}

#[test]
fn test_parse_payload_string_as_ident() {
    assert_eq!(
        ident("payload".to_string()),
        ParseResult::Ok(("".to_string(), Node::Ident("payload".to_string())))
    );
}

#[test]
fn test_parse_fn_call() {
    assert_eq!(
        fn_call("name()".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::Application(Box::new(Application {
                name: Node::Ident("name".to_string()),
                args: vec![],
            }))
        ))
    );
    assert_eq!(
        fn_call("name(1,2)".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::Application(Box::new(Application {
                name: Node::Ident("name".to_string()),
                args: vec![Node::Uint(1), Node::Uint(2)],
            }))
        ))
    );
    assert_eq!(
        fn_call("name(1,fn(2))".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::Application(Box::new(Application {
                name: Node::Ident("name".to_string()),
                args: vec![
                    Node::Uint(1),
                    Node::Application(Box::new(Application {
                        name: Node::Ident("fn".to_string()),
                        args: vec![Node::Uint(2)],
                    })),
                ],
            }))
        ))
    );
}

#[test]
fn test_parse_bool() {
    assert_eq!(
        _bool("truesomeshitaftertrue".to_string()),
        ParseResult::Ok(("someshitaftertrue".to_string(), Node::Bool(true)))
    );
    assert_eq!(
        _bool("falsesomeshitaftertrue".to_string()),
        ParseResult::Ok(("someshitaftertrue".to_string(), Node::Bool(false), ))
    );
}

#[test]
fn test_block() {
    assert_eq!(
        block(
            "


        printf(\"salam %d\", i);
        
        
        
        "
                .to_string()
        ),
        Ok((
            "".to_string(),
            Node::Block(vec![Node::Application(Box::new(Application {
                name: Node::Ident("printf".to_string()),
                args: vec![
                    Node::Str("salam %d".to_string()),
                    Node::Ident("i".to_string()),
                ],
            })), ])
        ))
    )
}

#[test]
fn test_parse_for_while() {
    assert_eq!(
        _for("for i<=10 {\n\tprintf(\"salam %d\", i);\n   }".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::While(Box::new(While {
                cond: Node::Operation(Box::new(Operation {
                    lhs: Node::Ident("i".to_string()),
                    op: Operator::LesserEq,
                    rhs: Node::Uint(10),
                })),
                block: Node::Block(vec![Node::Application(Box::new(Application {
                    name: Node::Ident("printf".to_string()),
                    args: vec![
                        Node::Str("salam %d".to_string()),
                        Node::Ident("i".to_string()),
                    ],
                }))]),
            }))
        ))
    );
}

#[test]
fn test_parse_for_c() {
    assert_eq!(
        _for("for i:int = 0;i<=10;inc i {\n\tprintf(\"salam %d\", i);\n}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::For(Box::new(For {
                init: Node::Decl(
                    Box::new(Node::Ident("i".to_string())),
                    Box::new(Some(Node::IntTy)),
                    Box::new(Node::Uint(0)),
                ),
                cond: Node::Operation(Box::new(Operation {
                    lhs: Node::Ident("i".to_string()),
                    op: Operator::LesserEq,
                    rhs: Node::Uint(10),
                })),
                cont: Node::Inc(Box::new(Node::Ident("i".to_string()))),
                body: Node::Block(vec![Node::Application(Box::new(Application {
                    name: Node::Ident("printf".to_string()),
                    args: vec![
                        Node::Str("salam %d".to_string()),
                        Node::Ident("i".to_string()),
                    ],
                }))]),
            }))
        ))
    );
}

#[test]
fn test_parse_import() {
    assert_eq!(
        _import("import \"c:stdio.h\" abc".to_string()),
        ParseResult::Ok((
            "abc".to_string(),
            Node::Import(Box::new(Import {
                path: Node::Str("c:stdio.h".to_string()),
                _as: None,
            }))
        ))
    );
    assert_eq!(
        _import("    import         \"c:stdio.h\"".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::Import(Box::new(Import {
                path: Node::Str("c:stdio.h".to_string()),
                _as: None,
            }))
        ))
    )
}

#[test]
fn test_parse_struct() {
    assert_eq!(
        _struct("struct {\n\tname: string,\n\tage:int\n}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::StructTy(vec![
                IdentAndTy {
                    ident: Node::Ident("name".to_string()),
                    ty: Node::StringTy,
                },
                IdentAndTy {
                    ident: Node::Ident("age".to_string()),
                    ty: Node::IntTy,
                },
            ])
        ))
    );
}

#[test]
fn test_parse_decl_struct() {
    let decl_res =
        decl("s = struct {name: string, age: int, meta: struct {mature: bool}}".to_string());
    assert!(decl_res.is_ok());
    let none: Box<Option<Node>> = Box::new(None);
    if let (_, Node::Decl(name, none, be)) = decl_res.unwrap() {
        assert_eq!(name, Box::new(Node::Ident("s".to_string())));
        assert_eq!(
            be,
            Box::new(Node::StructTy(vec![
                IdentAndTy {
                    ident: Node::Ident("name".to_string()),
                    ty: Node::StringTy,
                },
                IdentAndTy {
                    ident: Node::Ident("age".to_string()),
                    ty: Node::IntTy,
                },
                IdentAndTy {
                    ident: Node::Ident("meta".to_string()),
                    ty: Node::StructTy(vec![IdentAndTy {
                        ident: Node::Ident("mature".to_string()),
                        ty: Node::BooleanTy,
                    }]),
                },
            ]))
        );
    } else {
        assert!(false);
    }
}

#[test]
fn test_parse_array_type() {
    assert_eq!(
        expr("[int]".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::ArrayTy(Box::new(ArrayTy {
                size: None,
                inner_ty: Node::IntTy,
            }))
        ))
    );
    assert_eq!(
        expr("[int;2]".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::ArrayTy(Box::new(ArrayTy {
                size: Some(Node::Uint(2)),
                inner_ty: Node::IntTy,
            }))
        ))
    );
}

#[test]
fn test_parse_if() {
    assert_eq!(
        _if("if true {\n\tfn(1);\n\tfn(2);}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::If(Box::new(If {
                cond: Node::Bool(true),
                block: Node::Block(vec![
                    Node::Application(Box::new(Application {
                        name: Node::Ident("fn".to_string()),
                        args: vec![Node::Uint(1)],
                    })),
                    Node::Application(Box::new(Application {
                        name: Node::Ident("fn".to_string()),
                        args: vec![Node::Uint(2)],
                    })),
                ]),
            }))
        ))
    );
}

#[test]
fn test_parse_return() {
    assert_eq!(
        _return("return 1".to_string()),
        ParseResult::Ok(("".to_string(), Node::Return(Box::new(Node::Uint(1)))))
    )
}

#[test]
fn test_parse_expr() {
    assert_eq!(
        expr("true".to_string()),
        ParseResult::Ok(("".to_string(), Node::Bool(true)))
    );
    assert_eq!(
        expr("false".to_string()),
        ParseResult::Ok(("".to_string(), Node::Bool(false)))
    );
    assert_eq!(
        expr("12".to_string()),
        ParseResult::Ok(("".to_string(), Node::Uint(12)))
    );
    assert_eq!(
        expr("-12".to_string()),
        ParseResult::Ok(("".to_string(), Node::Int(-12)))
    );
    assert_eq!(
        expr("12.2".to_string()),
        ParseResult::Ok(("".to_string(), Node::Float(12.2)))
    );
    assert_eq!(
        expr("-12.2".to_string()),
        ParseResult::Ok(("".to_string(), Node::Float(-12.2)))
    );
    assert_eq!(
        expr("name".to_string()),
        ParseResult::Ok(("".to_string(), Node::Ident("name".to_string())))
    );
    assert_eq!(
        expr("'c'".to_string()),
        ParseResult::Ok(("".to_string(), Node::Char('c')))
    );
    assert_eq!(
        expr("\"name\"".to_string()),
        ParseResult::Ok(("".to_string(), Node::Str("name".to_string())))
    );
    assert_eq!(
        expr("struct {\n\tname: string,\n\tupdated_at: date}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::StructTy(vec![
                IdentAndTy {
                    ident: Node::Ident("name".to_string()),
                    ty: Node::StringTy,
                },
                IdentAndTy {
                    ident: Node::Ident("updated_at".to_string()),
                    ty: Node::Ident("date".to_string()),
                },
            ])
        ))
    );

    assert_eq!(
        expr("struct {\n\tname: string,\n\tpayload: struct {created_at: date}}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::StructTy(vec![
                IdentAndTy {
                    ident: Node::Ident("name".to_string()),
                    ty: Node::StringTy,
                },
                IdentAndTy {
                    ident: Node::Ident("payload".to_string()),
                    ty: Node::StructTy(vec![IdentAndTy {
                        ident: Node::Ident("created_at".to_string()),
                        ty: Node::Ident("date".to_string()),
                    }]),
                },
            ])
        ))
    );
    assert_eq!(
        expr("fn_call(1,2)".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::Application(Box::new(Application {
                name: Node::Ident("fn_call".to_string()),
                args: vec![Node::Uint(1), Node::Uint(2)],
            }))
        ))
    );
    assert_eq!(
        expr("[struct {name: string}]".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::ArrayTy(Box::new(ArrayTy {
                size: None,
                inner_ty: Node::StructTy(vec![IdentAndTy {
                    ident: Node::Ident("name".to_string()),
                    ty: Node::StringTy,
                }]),
            }))
        ))
    );
    assert_eq!(
        expr("[struct {name: string};2]".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::ArrayTy(Box::new(ArrayTy {
                size: Some(Node::Uint(2)),
                inner_ty: Node::StructTy(vec![IdentAndTy {
                    ident: Node::Ident("name".to_string()),
                    ty: Node::StringTy,
                }]),
            }))
        ))
    );
    assert_eq!(
        expr("if cond(true) {\n\ta = 1;fn(a);\n}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::If(Box::new(If {
                cond: Node::Application(Box::new(Application {
                    name: Node::Ident("cond".to_string()),
                    args: vec![Node::Bool(true)],
                })),
                block: Node::Block(vec![
                    Node::Decl(
                        Box::new(Node::Ident("a".to_string())),
                        Box::new(None),
                        Box::new(Node::Uint(1)),
                    ),
                    Node::Application(Box::new(Application {
                        name: Node::Ident("fn".to_string()),
                        args: vec![Node::Ident("a".to_string())],
                    })),
                ]),
            }))
        ))
    );
    assert_eq!(
        expr("fn() void {\n\t print(\"salam\");\n\t}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::FnDef(Box::new(FnDef {
                ty: FnTy {
                    args: vec![],
                    return_ty: Node::VoidTy,
                },
                block: Node::Block(vec![Node::Application(Box::new(Application {
                    name: Node::Ident("print".to_string()),
                    args: vec![Node::Str("salam".to_string())],
                }))]),
            }))
        ))
    );
    assert_eq!(
        expr("fn(a: struct { b: string }) void {\n\t print(\"salam\");\n\t}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            Node::FnDef(Box::new(FnDef {
                ty: FnTy {
                    args: vec![IdentAndTy {
                        ident: Node::Ident("a".to_string()),
                        ty: Node::StructTy(vec![IdentAndTy {
                            ident: Node::Ident("b".to_string()),
                            ty: Node::StringTy,
                        }]),
                    }],
                    return_ty: Node::VoidTy,
                },
                block: Node::Block(vec![Node::Application(Box::new(Application {
                    name: Node::Ident("print".to_string()),
                    args: vec![Node::Str("salam".to_string())],
                }))]),
            }))
        ))
    );
    assert_eq!(
        expr("a*2".to_string()),
        Ok((
            "".to_string(),
            Node::Operation(Box::new(Operation {
                lhs: Node::Ident("a".to_string()),
                op: Operator::Multiply,
                rhs: Node::Uint(2),
            }))
        ))
    );
    assert_eq!(
        expr("a%2".to_string()),
        Ok((
            "".to_string(),
            Node::Operation(Box::new(Operation {
                lhs: Node::Ident("a".to_string()),
                op: Operator::Mod,
                rhs: Node::Uint(2),
            }))
        ))
    );
    assert_eq!(
        expr("a/2".to_string()),
        Ok((
            "".to_string(),
            Node::Operation(Box::new(Operation {
                lhs: Node::Ident("a".to_string()),
                op: Operator::Div,
                rhs: Node::Uint(2),
            }))
        ))
    );
    assert_eq!(
        expr("a-2".to_string()),
        Ok((
            "".to_string(),
            Node::Operation(Box::new(Operation {
                lhs: Node::Ident("a".to_string()),
                op: Operator::Minus,
                rhs: Node::Uint(2),
            }))
        ))
    );
    assert_eq!(
        expr("sum(1,2)+mins(1,2,3,4)".to_string()),
        Ok((
            "".to_string(),
            Node::Operation(Box::new(Operation {
                lhs: Node::Application(Box::new(Application {
                    name: Node::Ident("sum".to_string()),
                    args: vec![Node::Uint(1), Node::Uint(2)],
                })),
                op: Operator::Plus,
                rhs: Node::Application(Box::new(Application {
                    name: Node::Ident("mins".to_string()),
                    args: vec![Node::Uint(1), Node::Uint(2), Node::Uint(3), Node::Uint(4)],
                })),
            }))
        ))
    );

    assert_eq!(
        expr("a+2".to_string()),
        Ok((
            "".to_string(),
            Node::Operation(Box::new(Operation {
                lhs: Node::Ident("a".to_string()),
                op: Operator::Plus,
                rhs: Node::Uint(2),
            }))
        ))
    );
    assert_eq!(
        expr("union { i:int, f:float }".to_string()),
        Ok((
            "".to_string(),
            Node::UnionTy(vec![
                IdentAndTy {
                    ident: Node::Ident("i".to_string()),
                    ty: Node::IntTy,
                },
                IdentAndTy {
                    ident: Node::Ident("f".to_string()),
                    ty: Node::FloatTy,
                },
            ])
        ))
    );
    assert_eq!(
        expr("enum { first, second }".to_string()),
        Ok((
            "".to_string(),
            Node::EnumTy(vec![
                Node::Ident("first".to_string()),
                Node::Ident("second".to_string()),
            ])
        ))
    );
}

#[test]
fn test_stmt_import() {
    assert_eq!(
        statement("import \"c:stdio.h\";".to_string()),
        Ok((
            "".to_string(),
            Node::Import(Box::new(Import {
                path: Node::Str("c:stdio.h".to_string()),
                _as: None,
            }))
        ))
    );
}

#[test]
fn test_parse_import_in_module() {
    assert_eq!(
        module(
            "import \"stdio.h\";
main = fn() void {
    printf(\"Hello world\");
};"
                .to_string()
        ),
        ParseResult::Ok((
            "".to_string(),
            Node::Block(vec![
                Node::Import(Box::new(Import {
                    path: Node::Str("stdio.h".to_string()),
                    _as: None,
                })),
                Node::Decl(
                    Box::new(Node::Ident("main".to_string())),
                    Box::new(None),
                    Box::new(Node::FnDef(Box::new(FnDef {
                        ty: FnTy {
                            args: vec![],
                            return_ty: Node::VoidTy,
                        },
                        block: Node::Block(vec![Node::Application(Box::new(Application {
                            name: Node::Ident("printf".to_string()),
                            args: vec![Node::Str("Hello world".to_string())],
                        }))]),
                    }))),
                ),
            ])
        ))
    );
}

#[test]
fn test_union() {
    assert_eq!(
        union(" union {\n\ti: int , f: float\n}".to_string()),
        Ok((
            "".to_string(),
            Node::UnionTy(vec![
                IdentAndTy {
                    ident: Node::Ident("i".to_string()),
                    ty: Node::IntTy,
                },
                IdentAndTy {
                    ident: Node::Ident("f".to_string()),
                    ty: Node::FloatTy,
                },
            ])
        ))
    )
}

#[test]
fn test_enum() {
    assert_eq!(
        _enum(" enum {\n\tShanbe , YekShanbe\n}".to_string()),
        Ok((
            "".to_string(),
            Node::EnumTy(vec![
                Node::Ident("Shanbe".to_string()),
                Node::Ident("YekShanbe".to_string()),
            ])
        ))
    )
}

#[test]
fn another_test() {
    let code = "


   i: int = 0;
   for i <= 10 {
     printf(\"salam %d\", i); 
     inc i;
   };

";
    assert_eq!(
        block(code.to_string()),
        Ok((
            "".to_string(),
            Node::Block(vec![
                Node::Decl(
                    Box::new(Node::Ident("i".to_string())),
                    Box::new(Some(Node::IntTy)),
                    Box::new(Node::Uint(0)),
                ),
                Node::While(Box::new(While {
                    cond: Node::Operation(Box::new(Operation {
                        lhs: Node::Ident("i".to_string()),
                        op: Operator::LesserEq,
                        rhs: Node::Uint(10),
                    })),
                    block: Node::Block(vec![
                        Node::Application(Box::new(Application {
                            name: Node::Ident("printf".to_string()),
                            args: vec![
                                Node::Str("salam %d".to_string()),
                                Node::Ident("i".to_string()),
                            ],
                        })),
                        Node::Inc(Box::new(Node::Ident("i".to_string()))),
                    ]),
                })),
            ]),
        ))
    )
}

#[test]
fn test_parse_module() {
    assert_eq!(
        module(
            "import \"stdio.h\";
a =2;
b =    32.2;
c   = false;
sum = fn ( a :int , b : int) int {
     return   a   + b;
};
main = fn() void {
     println ( \"Hello World\");
     return sum (1, 2);
};"
                .to_string()
        ),
        Ok((
            "".to_string(),
            Node::Block(vec![
                Node::Import(Box::new(Import {
                    path: Node::Str("stdio.h".to_string()),
                    _as: None,
                })),
                Node::Decl(
                    Box::new(Node::Ident("a".to_string())),
                    Box::new(None),
                    Box::new(Node::Uint(2)),
                ),
                Node::Decl(
                    Box::new(Node::Ident("b".to_string())),
                    Box::new(None),
                    Box::new(Node::Float(32.2)),
                ),
                Node::Decl(
                    Box::new(Node::Ident("c".to_string())),
                    Box::new(None),
                    Box::new(Node::Bool(false)),
                ),
                Node::Decl(
                    Box::new(Node::Ident("sum".to_string())),
                    Box::new(None),
                    Box::new(Node::FnDef(Box::new(FnDef {
                        ty: FnTy {
                            args: vec![
                                IdentAndTy {
                                    ident: Node::Ident("a".to_string()),
                                    ty: Node::IntTy,
                                },
                                IdentAndTy {
                                    ident: Node::Ident("b".to_string()),
                                    ty: Node::IntTy,
                                },
                            ],
                            return_ty: Node::IntTy,
                        },
                        block: Node::Block(vec![Node::Return(Box::new(Node::Operation(
                            Box::new(Operation {
                                lhs: Node::Ident("a".to_string()),
                                op: Operator::Plus,
                                rhs: Node::Ident("b".to_string()),
                            })
                        )))]),
                    }))),
                ),
                Node::Decl(
                    Box::new(Node::Ident("main".to_string())),
                    Box::new(None),
                    Box::new(Node::FnDef(Box::new(FnDef {
                        ty: FnTy {
                            args: vec![],
                            return_ty: Node::VoidTy,
                        },
                        block: Node::Block(vec![
                            Node::Application(Box::new(Application {
                                name: Node::Ident("println".to_string()),
                                args: vec![Node::Str("Hello World".to_string())],
                            })),
                            Node::Return(Box::new(Node::Application(Box::new(Application {
                                name: Node::Ident("sum".to_string()),
                                args: vec![Node::Uint(1), Node::Uint(2)],
                            })))),
                        ]),
                    }))),
                ),
            ])
        ))
    );
}


#[test]
fn test_parse_module_with_struct_enum_union() {
    let out = module(
        "import \"stdio.h\"
Human = struct {
    name: string,
    age: int
};

Kind = enum {
    Animal,
    Human
};

U = union {
    i: int,
    s: string,
};
".to_string()
    );

    assert_eq!(
    out    ,
        Ok((
            "".to_string(),
            Node::Block(vec![
                Node::Import(Box::new(Import {
                    path: Node::Str("stdio.h".to_string()),
                    _as: None,
                })),
                Node::Decl(
                    Box::new(Node::Ident("Human".to_string())),
                    Box::new(None),
                    Box::new(Node::StructTy(vec![
                        IdentAndTy { ident: Node::Ident("name".to_string()), ty: Node::StringTy },
                        IdentAndTy { ident: Node::Ident("age".to_string()), ty: Node::IntTy },
                    ]))),
                Node::Decl(
                    Box::new(Node::Ident("Kind".to_string())),
                    Box::new(None),
                    Box::new(Node::EnumTy(vec![
                        Node::Ident("Animal".to_string()),
                        Node::Ident("Human".to_string()),
                    ]))),
                Node::Decl(
                    Box::new(Node::Ident("U".to_string())),
                    Box::new(None),
                    Box::new(Node::UnionTy(vec![
                        IdentAndTy { ident: Node::Ident("i".to_string()), ty: Node::IntTy },
                        IdentAndTy { ident: Node::Ident("s".to_string()), ty: Node::StringTy },
                    ])))]))))
}
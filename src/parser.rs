#![allow(dead_code)]
/*TODO
    - for
        - c syntax
        - foreach
        - while syntax
    - interface
    - operator expressions
*/
#[derive(Clone, Debug, PartialEq)]
pub enum ParseObj {
    Char(char),
    Uint(usize),
    Int(isize),
    Float(f64),
    Str(String),
    Keyword(String),
    Ident(String),
    Bool(bool),
    List(Vec<ParseObj>),
    Decl(String, Box<Option<ParseObj>>, Box<ParseObj>),
    FnCall(String, Vec<ParseObj>),
    Struct(Vec<(ParseObj, ParseObj)>),
    Fn(Vec<(ParseObj, ParseObj)>, Box<ParseObj>, Box<ParseObj>),
    Array(Box<Option<ParseObj>>, Box<ParseObj>),
    Stmt(Box<ParseObj>),
    Block(Vec<ParseObj>),
    If(Box<ParseObj>, Box<ParseObj>),
    ForC(Box<ParseObj>, Box<ParseObj>, Box<ParseObj>, Box<ParseObj>),
    Empty,
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

type ParseResult = Result<(String, ParseObj), ParseErr>;

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
        return Ok((input.clone(), ParseObj::List(result)));
    };
}

fn zero_or_one(parser: impl Fn(String) -> ParseResult) -> impl Fn(String) -> ParseResult {
    return move |mut input: String| {
        if let Ok((remains, parsed)) = parser(input.clone()) {
            return Ok((remains, ParseObj::Char('-')));
        }
        return Ok((input, ParseObj::Empty));
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
        return Ok((input.clone(), ParseObj::List(result)));
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
            ParseObj::Char(input.chars().nth(0).unwrap()),
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
            return ParseResult::Ok((input[1..].to_string(), ParseObj::Char(c)));
        }
        return ParseResult::Err(ParseErr::Unexpected(
            c.to_string(),
            input.chars().nth(0).unwrap().to_string(),
            0,
        ));
    };
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
            ParseObj::Str(remains[..end].to_string()),
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
        return Ok((input, ParseObj::Keyword(word.clone())));
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
        Ok((remains, ParseObj::List(chars_parse_objects))) => {
            let mut name = String::new();

            for po in chars_parse_objects {
                match po {
                    ParseObj::Char(c) => name.push(c),
                    _ => {
                        return Err(ParseErr::Unexpected(
                            "a char".to_string(),
                            format!("{:?}", po),
                            0,
                        ))
                    }
                }
            }
            return Ok((remains, ParseObj::Ident(name)));
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
        ParseObj::Ident(i) => identifier = i,
        _ => {
            return Err(ParseErr::Unexpected(
                "ident".to_string(),
                format!("{:?}", obj),
                0,
            ))
        }
    }
    let (mut remains, _) = parse_char('(')(remains)?;
    // we know it's a function call
    let mut args: Vec<ParseObj> = Vec::new();
    if remains.chars().nth(0).is_some() && remains.chars().nth(0).unwrap() != ')' {
        loop {
            // fn(1,2,3,4)
            let expr_res = expr(remains.clone())?;
            remains = expr_res.0;
            let obj = expr_res.1;
            args.push(obj);
            //,2)
            let comma = parse_char(',')(remains.clone());
            if let Ok((r, _)) = comma {
                remains = r;
            } else {
                break;
            }
        }
    }

    let close_paren_res = parse_char(')')(remains)?;
    remains = close_paren_res.0;

    return Ok((remains, ParseObj::FnCall(identifier, args)));
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

        return Ok((input, ParseObj::List(parsed)));
    };
}

fn uint(input: String) -> ParseResult {
    match one_or_more(digit())(input) {
        Ok((remains, ParseObj::List(_digits))) => {
            let mut number = String::new();
            for d in _digits {
                match d {
                    ParseObj::Char(c) => {
                        number.push(c);
                    }
                    _ => unreachable!(),
                }
            }
            let number: usize = number.parse().unwrap();
            Ok((remains, ParseObj::Uint(number)))
        }
        Err(err) => return Err(err),
        _ => unreachable!(),
    }
}

fn int(mut input: String) -> ParseResult {
    // int = sign uint
    let sign = zero_or_one(parse_char('-'));
    let mut with_sign: i8 = 1;
    if let (remains, ParseObj::Char('-')) = sign(input.clone())? {
        input = remains;
        with_sign = -1;
    } else {
        with_sign = 1;
    }

    let (input, obj) = uint(input)?;
    match obj {
        ParseObj::Uint(num) => {
            return Ok((input, ParseObj::Int(with_sign as isize * num as isize)))
        }
        _ => Err(ParseErr::Unknown(format!(
            "expected a uint found {:?}",
            obj
        ))),
    }
}

fn bool(input: String) -> ParseResult {
    let _true = keyword("true".to_string());
    let _false = keyword("false".to_string());
    let (remains, bool_parsed) = any_of(vec![_true, _false])(input)?;
    if let ParseObj::Keyword(b) = bool_parsed {
        return Ok((remains, ParseObj::Bool(b == "true")));
    } else {
        unreachable!()
    }
}

pub fn _struct(input: String) -> ParseResult {
    // struct { ident: type, }
    let (mut remains, _) = keyword("struct".to_string())(input)?;
    let (mut remains, _) = whitespace()(remains)?;
    let (mut remains, _) = parse_char('{')(remains)?;

    // we know it's a function call
    let mut idents_tys: Vec<(ParseObj, ParseObj)> = Vec::new();

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
            idents_tys.push((ident_obj.clone(), type_obj.clone()));

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
    return Ok((remains, ParseObj::Struct(idents_tys)));
}

fn array(input: String) -> ParseResult {
    let (remains, _) = parse_char('[')(input)?;
    let (mut remains, ty) = expr(remains)?;
    let semicolon_res = semicolon(remains.clone());
    let mut size: Option<ParseObj> = None;
    match semicolon_res {
        Ok((r, ParseObj::Char(';'))) => {
            let (r, size_obj) = expr(r)?;
            remains = r;
            size = Some(size_obj);
        }
        _ => (),
    };
    let (remains, _) = parse_char(']')(remains)?;
    return Ok((remains, ParseObj::Array(Box::new(size), Box::new(ty))));
}

fn statement(input: String) -> ParseResult {
    let parsers: Vec<fn(String) -> Result<(String, ParseObj), ParseErr>> = vec![decl, expr]; //TODO: add _for
    let (remains, _) = whitespace()(input.clone())?;
    let (remains, stmt) = any_of(parsers)(remains)?;
    let (remains, _) = parse_char(';')(remains.clone())?;
    return Ok((remains, stmt));
}

fn block(input: String) -> ParseResult {
    return match zero_or_more(statement)(input)? {
        (remains, ParseObj::List(items)) => Ok((remains, ParseObj::Block(items))),
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
    return Ok((remains, ParseObj::If(Box::new(cond), Box::new(_block))));
}

fn _for(input: String) -> ParseResult {
    let (remains, _) = keyword("for".to_string())(input)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, init) = decl(remains)?;
    let (remains, _) = parse_char(';')(remains)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, cond) = expr(remains)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, _) = parse_char(';')(remains)?;
    let (remains, cont) = expr(remains)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, _) = parse_char('{')(remains)?;
    let (remains, body) = block(remains)?;
    let (remains, _) = parse_char('}')(remains)?;
    return Ok((
        remains,
        ParseObj::ForC(
            Box::new(init),
            Box::new(cond),
            Box::new(cont),
            Box::new(body),
        ),
    ));
}

fn fn_def(input: String) -> ParseResult {
    let (mut remains, _) = keyword("fn".to_string())(input)?;
    let (mut remains, _) = whitespace()(remains)?;
    let (mut remains, _) = parse_char('(')(remains)?;
    let mut args_tys: Vec<(ParseObj, ParseObj)> = Vec::new();
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
            args_tys.push((ident_obj.clone(), type_obj.clone()));

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
    let (remains, _) = parse_char(')')(remains)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, ty) = expr(remains)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, _) = parse_char('{')(remains)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, _block) = block(remains)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, _) = parse_char('}')(remains)?;
    return Ok((
        remains,
        ParseObj::Fn(args_tys, Box::new(ty), Box::new(_block)),
    ));
}

fn float(input: String) -> ParseResult {
    // int parse_char('.') uint
    if let (remains, ParseObj::Int(int_part)) = int(input)? {
        if let (remains, _) = parse_char('.')(remains)? {
            let parsed = uint(remains)?;
            if let (remains, ParseObj::Uint(float_part)) = parsed {
                let float_str = format!("{}.{}", int_part, float_part);
                let float: f64 = float_str.parse().unwrap();
                Ok((remains, ParseObj::Float(float)))
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

fn expr(input: String) -> ParseResult {
    // bool
    // ident
    // String
    // int, uint, float
    // fn_call
    // fn_def
    // if
    let parsers: Vec<fn(String) -> Result<(String, ParseObj), ParseErr>> = vec![
        float, uint, int, bool, string, _if, fn_def, _struct, fn_call, ident, array,
    ];
    return any_of(parsers)(input);
}

fn decl(mut input: String) -> ParseResult {
    // ident: expr = expr;
    let (remains, _) = whitespace()(input.clone())?;
    let (remains, obj) = ident(remains)?;
    let mut identifier = "".to_string();
    match obj {
        ParseObj::Ident(i) => identifier = i,
        _ => {
            return Err(ParseErr::Unexpected(
                "ident".to_string(),
                format!("{:?}", obj),
                0,
            ))
        }
    }
    println!("ident: {} remains: \"{}\"", identifier, remains);
    let (mut remains, _) = whitespace()(remains)?;
    let mut ty: Option<ParseObj> = None;
    let colon_res = parse_char(':')(remains.clone());
    match colon_res {
        Ok((r, ParseObj::Char(':'))) => {
            let ty_res = expr(r)?;
            remains = ty_res.0;
            ty = Some(ty_res.1);
        }
        _ => {}
    }
    let (remains, _) = parse_char('=')(remains)?;
    let (remains, _) = whitespace()(remains)?;
    println!("remains: \"{}\"", remains);
    let (remains, e) = expr(remains)?;
    println!("expr: {:?} remains: \"{}\"", e, remains);
    return Ok((
        remains,
        ParseObj::Decl(identifier, Box::new(ty), Box::new(e)),
    ));
}
fn module(input: String) -> ParseResult {
    return zero_or_more(statement)(input);
}
#[test]
fn test_parse_decl_bool() {
    let decl_res = decl("a = false".to_string());
    assert!(decl_res.is_ok());
    let none: Box<Option<ParseObj>> = Box::new(None);
    if let (_, ParseObj::Decl(name, none, be)) = decl_res.unwrap() {
        assert_eq!(name, "a");
        assert_eq!(be, Box::new(ParseObj::Bool(false)));
    } else {
        assert!(false);
    }
}
#[test]
fn test_parse_decl_int() {
    let decl_res = decl("a = -2".to_string());
    assert!(decl_res.is_ok());
    let none: Box<Option<ParseObj>> = Box::new(None);
    if let (_, ParseObj::Decl(name, none, be)) = decl_res.unwrap() {
        assert_eq!(name, "a");
        assert_eq!(be, Box::new(ParseObj::Int(-2)));
    } else {
        assert!(false);
    }
}

#[test]
fn test_parse_decl_str() {
    let decl_res = decl("a = \"amirreza\"".to_string());
    assert!(decl_res.is_ok());

    let none: Box<Option<ParseObj>> = Box::new(None);
    if let (_, ParseObj::Decl(name, none, be)) = decl_res.unwrap() {
        assert_eq!(name, "a");
        assert_eq!(be, Box::new(ParseObj::Str("amirreza".to_string())));
    } else {
        assert!(false);
    }
}
#[test]
fn test_parse_decl_uint() {
    let decl_res = decl("a = 2".to_string());
    assert!(decl_res.is_ok());
    let none: Box<Option<ParseObj>> = Box::new(None);
    if let (_, ParseObj::Decl(name, none, be)) = decl_res.unwrap() {
        assert_eq!(name, "a");
        assert_eq!(be, Box::new(ParseObj::Uint(2)));
    } else {
        assert!(false);
    }
}
#[test]
fn test_parse_single_digit() {
    assert_eq!(
        digit()("1AB".to_string()),
        ParseResult::Ok(("AB".to_string(), ParseObj::Char('1'),))
    );
}

#[test]
fn test_parse_float() {
    assert_eq!(
        float("4.2AB".to_string()),
        ParseResult::Ok(("AB".to_string(), ParseObj::Float(4.2)))
    );
}
#[test]
fn test_parse_string() {
    assert_eq!(
        string("\"amirreza\"".to_string()),
        ParseResult::Ok(("".to_string(), ParseObj::Str("amirreza".to_string())))
    );
}
#[test]
fn test_parse_int() {
    assert_eq!(
        int("-1234AB".to_string()),
        ParseResult::Ok(("AB".to_string(), ParseObj::Int(-1234)))
    );
    assert_eq!(
        int("1234AB".to_string()),
        ParseResult::Ok(("AB".to_string(), ParseObj::Int(1234)))
    );
}

#[test]
fn test_parse_uint() {
    assert_eq!(
        uint("1234AB".to_string()),
        ParseResult::Ok(("AB".to_string(), ParseObj::Uint(1234)))
    );
}

#[test]
fn test_parse_keyword() {
    assert_eq!(
        keyword("struct".to_string())("struct name".to_string()),
        ParseResult::Ok((" name".to_string(), ParseObj::Keyword("struct".to_string())))
    );
}
#[test]
fn test_parse_fn() {
    assert_eq!(
        fn_def("fn(a: int) string {\n\tprint(a);\n\t}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            ParseObj::Fn(
                vec![(
                    ParseObj::Ident("a".to_string()),
                    ParseObj::Ident("int".to_string())
                )],
                Box::new(ParseObj::Ident("string".to_string())),
                Box::new(ParseObj::Block(vec![ParseObj::FnCall(
                    "print".to_string(),
                    vec![ParseObj::Ident("a".to_string())]
                )]))
            )
        ))
    );
}

#[test]
fn test_parse_decl_fn() {
    let decl_res = decl("f = fn() void {\n\tprintln(\"Salam donya!\");\n}".to_string());
    assert!(decl_res.is_ok());
    let none: Box<Option<ParseObj>> = Box::new(None);
    if let (_, ParseObj::Decl(name, none, f)) = decl_res.unwrap() {
        assert_eq!(name, "f");
        assert_eq!(
            f,
            Box::new(ParseObj::Fn(
                vec![],
                Box::new(ParseObj::Ident("void".to_string())),
                Box::new(ParseObj::Block(vec![ParseObj::FnCall(
                    "println".to_string(),
                    vec![ParseObj::Str("Salam donya!".to_string())]
                )]))
            ))
        );
    } else {
        assert!(false);
    }
}
#[test]
fn test_parse_ident() {
    assert_eq!(
        ident("name".to_string()),
        ParseResult::Ok(("".to_string(), ParseObj::Ident("name".to_string())))
    );
    assert_eq!(
        ident("name_str".to_string()),
        ParseResult::Ok(("".to_string(), ParseObj::Ident("name_str".to_string()),))
    );
}

#[test]
fn test_parse_payload_string_as_ident() {
    assert_eq!(
        ident("payload".to_string()),
        ParseResult::Ok(("".to_string(), ParseObj::Ident("payload".to_string())))
    );
}
#[test]
fn test_parse_fn_call() {
    assert_eq!(
        fn_call("name()".to_string()),
        ParseResult::Ok(("".to_string(), ParseObj::FnCall("name".to_string(), vec![])))
    );
    assert_eq!(
        fn_call("name(1,2)".to_string()),
        ParseResult::Ok((
            "".to_string(),
            ParseObj::FnCall(
                "name".to_string(),
                vec![ParseObj::Uint(1), ParseObj::Uint(2)]
            )
        ))
    );
    assert_eq!(
        fn_call("name(1,fn(2))".to_string()),
        ParseResult::Ok((
            "".to_string(),
            ParseObj::FnCall(
                "name".to_string(),
                vec![
                    ParseObj::Uint(1),
                    ParseObj::FnCall("fn".to_string(), vec![ParseObj::Uint(2)]),
                ]
            )
        ))
    );
}
#[test]
fn test_parse_bool() {
    assert_eq!(
        bool("truesomeshitaftertrue".to_string()),
        ParseResult::Ok(("someshitaftertrue".to_string(), ParseObj::Bool(true)))
    );
    assert_eq!(
        bool("falsesomeshitaftertrue".to_string()),
        ParseResult::Ok(("someshitaftertrue".to_string(), ParseObj::Bool(false),))
    );
}

#[test]
fn test_parse_struct() {
    assert_eq!(
        _struct("struct {\n\tname: string,\n\tage:int\n}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            ParseObj::Struct(vec![
                (
                    ParseObj::Ident("name".to_string()),
                    ParseObj::Ident("string".to_string())
                ),
                (
                    ParseObj::Ident("age".to_string()),
                    ParseObj::Ident("int".to_string())
                ),
            ])
        ))
    );
}

#[test]
fn test_parse_decl_struct() {
    let decl_res =
        decl("s = struct {name: string, age: int, meta: struct {mature: bool}}".to_string());
    assert!(decl_res.is_ok());
    let none: Box<Option<ParseObj>> = Box::new(None);
    if let (_, ParseObj::Decl(name, none, be)) = decl_res.unwrap() {
        assert_eq!(name, "s");
        assert_eq!(
            be,
            Box::new(ParseObj::Struct(vec![
                (
                    ParseObj::Ident("name".to_string()),
                    ParseObj::Ident("string".to_string())
                ),
                (
                    ParseObj::Ident("age".to_string()),
                    ParseObj::Ident("int".to_string())
                ),
                (
                    ParseObj::Ident("meta".to_string()),
                    ParseObj::Struct(vec![(
                        ParseObj::Ident("mature".to_string()),
                        ParseObj::Ident("bool".to_string())
                    )])
                )
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
            ParseObj::Array(Box::new(None), Box::new(ParseObj::Ident("int".to_string())))
        ))
    );
    assert_eq!(
        expr("[int;2]".to_string()),
        ParseResult::Ok((
            "".to_string(),
            ParseObj::Array(
                Box::new(Some(ParseObj::Uint(2))),
                Box::new(ParseObj::Ident("int".to_string()))
            )
        ))
    );
}
// #[test]
// fn test_parse_for_c() {
//     assert_eq!(
//         _if("for i=0;i<10;i++ { print(i); }".to_string()),
//         ParseResult::Ok((
//             "".to_string(),
//             ParseObj::ForC(
//                 Box::new(ParseObj::Decl(ParseObj::Ident("i".to_string()), Box::new(None), Box::new(ParseObj::Uint(0)))),
//                 )
//             )
//         ));
// }
#[test]
fn test_parse_if() {
    assert_eq!(
        _if("if true {\n\tfn(1);\n\tfn(2);}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            ParseObj::If(
                Box::new(ParseObj::Bool(true)),
                Box::new(ParseObj::Block(vec![
                    ParseObj::FnCall("fn".to_string(), vec![ParseObj::Uint(1)]),
                    ParseObj::FnCall("fn".to_string(), vec![ParseObj::Uint(2)]),
                ]))
            )
        ))
    );
}

#[test]
fn test_parse_expr() {
    assert_eq!(
        expr("true".to_string()),
        ParseResult::Ok(("".to_string(), ParseObj::Bool(true)))
    );
    assert_eq!(
        expr("false".to_string()),
        ParseResult::Ok(("".to_string(), ParseObj::Bool(false)))
    );
    assert_eq!(
        expr("12".to_string()),
        ParseResult::Ok(("".to_string(), ParseObj::Uint(12)))
    );
    assert_eq!(
        expr("-12".to_string()),
        ParseResult::Ok(("".to_string(), ParseObj::Int(-12)))
    );
    assert_eq!(
        expr("12.2".to_string()),
        ParseResult::Ok(("".to_string(), ParseObj::Float(12.2)))
    );
    assert_eq!(
        expr("-12.2".to_string()),
        ParseResult::Ok(("".to_string(), ParseObj::Float(-12.2)))
    );
    assert_eq!(
        expr("name".to_string()),
        ParseResult::Ok(("".to_string(), ParseObj::Ident("name".to_string())))
    );
    assert_eq!(
        expr("\"name\"".to_string()),
        ParseResult::Ok(("".to_string(), ParseObj::Str("name".to_string())))
    );
    assert_eq!(
        expr("struct {\n\tname: string,\n\tupdated_at: date}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            ParseObj::Struct(vec![
                (
                    ParseObj::Ident("name".to_string()),
                    ParseObj::Ident("string".to_string())
                ),
                (
                    ParseObj::Ident("updated_at".to_string()),
                    ParseObj::Ident("date".to_string())
                )
            ])
        ))
    );

    assert_eq!(
        expr("struct {\n\tname: string,\n\tpayload: struct {created_at: date}}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            ParseObj::Struct(vec![
                (
                    ParseObj::Ident("name".to_string()),
                    ParseObj::Ident("string".to_string())
                ),
                (
                    ParseObj::Ident("payload".to_string()),
                    ParseObj::Struct(vec![(
                        ParseObj::Ident("created_at".to_string()),
                        ParseObj::Ident("date".to_string())
                    )])
                ),
            ])
        ))
    );
    assert_eq!(
        expr("fn_call(1,2)".to_string()),
        ParseResult::Ok((
            "".to_string(),
            ParseObj::FnCall(
                "fn_call".to_string(),
                vec![ParseObj::Uint(1), ParseObj::Uint(2),]
            )
        ))
    );
    assert_eq!(
        expr("[struct {name: string}]".to_string()),
        ParseResult::Ok((
            "".to_string(),
            ParseObj::Array(
                Box::new(None),
                Box::new(ParseObj::Struct(vec![(
                    ParseObj::Ident("name".to_string()),
                    ParseObj::Ident("string".to_string())
                )]))
            )
        ))
    );
    assert_eq!(
        expr("[struct {name: string};2]".to_string()),
        ParseResult::Ok((
            "".to_string(),
            ParseObj::Array(
                Box::new(Some(ParseObj::Uint(2))),
                Box::new(ParseObj::Struct(vec![(
                    ParseObj::Ident("name".to_string()),
                    ParseObj::Ident("string".to_string())
                )]))
            )
        ))
    );
    assert_eq!(
        expr("if cond(true) {\n\ta = 1;fn(a);\n}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            ParseObj::If(
                Box::new(ParseObj::FnCall(
                    "cond".to_string(),
                    vec![ParseObj::Bool(true)]
                )),
                Box::new(ParseObj::Block(vec![
                    ParseObj::Decl("a".to_string(), Box::new(None), Box::new(ParseObj::Uint(1))),
                    ParseObj::FnCall("fn".to_string(), vec![ParseObj::Ident("a".to_string())]),
                ]))
            )
        ))
    );
    assert_eq!(
        expr("fn() void {\n\t print(\"salam\");\n\t}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            ParseObj::Fn(
                vec![],
                Box::new(ParseObj::Ident("void".to_string())),
                Box::new(ParseObj::Block(vec![ParseObj::FnCall(
                    "print".to_string(),
                    vec![ParseObj::Str("salam".to_string())]
                )]))
            )
        ))
    );
    assert_eq!(
        expr("fn(a: struct { b: string }) void {\n\t print(\"salam\");\n\t}".to_string()),
        ParseResult::Ok((
            "".to_string(),
            ParseObj::Fn(
                vec![(
                    ParseObj::Ident("a".to_string()),
                    ParseObj::Struct(vec![(
                        ParseObj::Ident("b".to_string()),
                        ParseObj::Ident("string".to_string())
                    )]),
                )],
                Box::new(ParseObj::Ident("void".to_string())),
                Box::new(ParseObj::Block(vec![ParseObj::FnCall(
                    "print".to_string(),
                    vec![ParseObj::Str("salam".to_string())]
                )]))
            )
        ))
    );
}

#[test]
fn test_parse_module() {
    assert_eq!(
        module("main = fn() void {\n\tprintln(\"Hello World\");};".to_string()),
        ParseResult::Ok((
            "".to_string(),
            ParseObj::List(vec![ParseObj::Decl(
                "main".to_string(),
                Box::new(None),
                Box::new(ParseObj::Fn(
                    vec![],
                    Box::new(ParseObj::Ident("void".to_string())),
                    Box::new(ParseObj::Block(vec![ParseObj::FnCall(
                        "println".to_string(),
                        vec![ParseObj::Str("Hello World".to_string())]
                    )]))
                ))
            )])
        ))
    );
}

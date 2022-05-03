use nom::bytes::complete::escaped;

#[derive(Debug, PartialEq)]
enum ParseObj {
    Char(char),
    Uint(usize),
    Int(isize),
    Float(f64),
    Str(String),
    Keyword(String),
    Ident(String),
    Bool(bool),
    List(Vec<ParseObj>),
    Decl(String, Box<ParseObj>),
    FnCall(String, Vec<ParseObj>),
    Empty,
}

#[derive(Debug, PartialEq, Eq)]
enum ParseErr {
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

        return ParseResult::Ok((input[1..].to_string(), ParseObj::Char(input.chars().nth(0).unwrap())));
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

        if c== '"' {
            if remains.chars().nth(idx-1).is_some() {
               if remains.chars().nth(idx-1).unwrap() != '\\' {
                   end = idx;
               }
            }
        } 
    }
    if end != 0 {
        return Ok((remains[end+1..].to_string(), ParseObj::Str(remains[..end].to_string())));
    } else {
        return Err(ParseErr::Unknown("cannot find end of string".to_string()))
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

fn any_whitespace<'a>() -> impl Fn(String) -> ParseResult {
    let sp = parse_char(' ');
    let tab = parse_char('\t');
    let newline = parse_char('\n');
    return any_of(vec![sp, tab, newline]);
}

fn whitespace<'a>() -> impl Fn(String) -> ParseResult {
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
        "abcdefghijklmnopqrstuvwxzABCDEFGHIJKLMNOPQRSTUVWXZ_",
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
            return Ok((remains, ParseObj::Ident((name))));
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
        _ => return Err(ParseErr::Unexpected("ident".to_string(), format!("{:?}", obj), 0))
    }
    let (mut remains, _) = parse_char('(')(remains)?;
    // we know it's a function call
    let mut args: Vec<ParseObj> = Vec::new();
    loop {
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

    let (mut remains, _) = parse_char(')')(remains)?;
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
        ParseObj::Uint(num) => return Ok((input, ParseObj::Int(with_sign as isize * num as isize))),
        _ => Err(ParseErr::Unknown(format!("expected a uint found {:?}", obj)))
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

fn float(input: String) -> ParseResult {
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
    let parsers: Vec<fn(String) -> Result<(String, ParseObj), ParseErr>> =
        vec![float, uint, int, bool, string, fn_call, ident];
    return any_of(parsers)(input);
}

fn decl(mut input: String) -> ParseResult {
    // ident\s*=\s*expr;
    let (remains, obj) = ident(input)?;
    let mut identifier = "".to_string();
    match obj {
        ParseObj::Ident(i) => identifier = i,
        _ => return Err(ParseErr::Unexpected("ident".to_string(), format!("{:?}", obj), 0))
    }
    let (remains, _) = whitespace()(remains)?;
    let (remains, _) = parse_char('=')(remains)?;
    let (remains, _) = whitespace()(remains)?;
    let (remains, e) = expr(remains)?;
    return Ok((remains, ParseObj::Decl(identifier, Box::new(e))));
}
#[test]
fn test_parse_decl_bool() {
    let decl_res = decl("a = false".to_string());
    assert!(decl_res.is_ok());
    if let (_, ParseObj::Decl(name, be)) = decl_res.unwrap() {
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
    if let (_, ParseObj::Decl(name, be)) = decl_res.unwrap() {
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
    if let (_, ParseObj::Decl(name, be)) = decl_res.unwrap() {
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
    if let (_, ParseObj::Decl(name, be)) = decl_res.unwrap() {
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
fn test_parse_fn_call() {
    assert_eq!(
        fn_call("name(1,2)".to_string()),
        ParseResult::Ok(("".to_string(), ParseObj::FnCall("name".to_string(), vec![ParseObj::Uint(1), ParseObj::Uint(2)])))
    );
    assert_eq!(
        fn_call("name(1,fn(2))".to_string()),
        ParseResult::Ok(("".to_string(), ParseObj::FnCall("name".to_string(), vec![
            ParseObj::Uint(1), 
            ParseObj::FnCall("fn".to_string(), vec![ParseObj::Uint(2)]),
        ])))
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
        expr("fn_call(1,2)".to_string()),
        ParseResult::Ok(("".to_string(), ParseObj::FnCall("fn_call".to_string(), vec![
            ParseObj::Uint(1),
            ParseObj::Uint(2),
        ])))
    );
}

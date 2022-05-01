#[derive(Debug)]
struct ParseErr {
    msg: String,
    loc: usize,
}

impl ParseErr {
    pub fn new(loc: usize, msg: String) -> Self {
        return Self { loc, msg };
    }
}

impl std::fmt::Display for ParseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{} at {}", self.msg, self.loc))
    }
}
impl std::error::Error for ParseErr {}

type ParseResult = Result<(String, String), ParseErr>;

fn parse_char(c: char) -> impl Fn(String) -> ParseResult {
    return move |input: String| {
        if input.len() < 1 {
            return ParseResult::Err(ParseErr::new(0, "expected at least on char".to_string()));
        }
        if input.chars().nth(0).unwrap() == c.clone() {
            return ParseResult::Ok((c.to_string(), input[1..].to_string()));
        }
        return ParseResult::Err(ParseErr::new(
            0,
            format!("expected {} saw {}", c, input.chars().nth(0).unwrap()),
        ));
    };
}

fn any_of(parsers: Vec<impl Fn(String) -> ParseResult>) -> impl Fn(String) -> ParseResult {
    return move |input: String| {
        for parser in parsers.iter() {
            let res = parser(input.clone());
            match res {
                Ok((parsed, remaining)) => return Ok((parsed, remaining)),
                Err(err) => continue,
            }
        }
        return Err(ParseErr::new(0, "combinator error".to_string()));
    };
}
fn zero_or_one(parser: impl Fn(String) -> ParseResult) -> impl Fn(String) -> ParseResult {

}

fn zero_or_more(parser: impl Fn(String) -> ParseResult) -> impl Fn(String) -> ParseResult {
    return move |mut input: String| {
        let mut result = Vec::new();
        while let Ok((parsed, remains)) = parser(input.clone()) {
            input = remains;
            result.push(parsed);
        }
        return Ok((result.concat(), input.clone()));
    };
}

fn one_or_more(parser: impl Fn(String) -> ParseResult + Copy) -> impl Fn(String) -> ParseResult {
    return move |mut input: String| {
        let mut result = Vec::new();

        // we should first try to get one, if can't it's a parse error
        match parser(input.clone()) {
            Ok((parsed, remains)) => {
                input = remains;
                result.push(parsed);
            },
            Err(err) => {
                return Err(err);
            }
        }
        // other values are optional.
        return zero_or_more(parser)(input);
    };
}

fn any_whitespace() -> impl Fn(String) -> ParseResult {
    let sp = parse_char(' ');
    let tab = parse_char('\t');
    let newline = parse_char('\n');
    return any_of(vec![sp, tab, newline]);
}

fn whitespace() -> impl Fn(String) -> ParseResult {
    return;
}

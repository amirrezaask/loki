// #[derive(Debug, PartialEq, Eq)]
// struct ParseErr {
//     msg: String,
//     loc: usize,
// }

// impl ParseErr {
//     pub fn new(loc: usize, msg: String) -> Self {
//         return Self { loc, msg };
//     }
// }

// impl std::fmt::Display for ParseErr {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         f.write_fmt(format_args!("{} at {}", self.msg, self.loc))
//     }
// }
// impl std::error::Error for ParseErr {}

// type ParseResult = Result<(String, String), ParseErr>;

// fn any_of<'a>(parsers: Vec<impl Fn(String) -> ParseResult>) -> impl Fn(String) -> ParseResult {
//     return move |input: String| {
//         for parser in parsers.iter() {
//             let res = parser(input.clone());
//             match res {
//                 Ok((remaining, parsed)) => return Ok((remaining, parsed)),
//                 Err(err) => continue,
//             }
//         }
//         return Err(ParseErr::new(0, "combinator error".to_string()));
//     };
// }

// fn zero_or_more<'a>(parser: impl Fn(String) -> ParseResult) -> impl Fn(String) -> ParseResult {
//     return move |mut input: String| {
//         let mut result = String::new();
//         while let Ok((remains, parsed)) = parser(input.clone()) {
//             input = remains;
//             result = format!("{}{}", result, parsed);
//         }
//         return Ok((input.clone(), result));
//     };
// }

// fn one_or_more<'a>(parser: impl Fn(String) -> ParseResult) -> impl Fn(String) -> ParseResult {
//     return move |mut input: String| {
//         let mut result = Vec::new();

//         // we should first try to get one, if can't it's a parse error
//         match parser(input.clone()) {
//             Ok((remains, parsed )) => {
//                 input = remains;
//                 result.push(parsed);
//             },
//             Err(err) => {
//                 return Err(err);
//             }
//         }
//         while let Ok((remains, parsed)) = parser(input.clone()) {
//             input = remains;
//             result.push(parsed);
//         }
//         return Ok((input.clone(), result));
//     };
// }

// fn parse_char<'a>(c: char) -> impl Fn(String) -> ParseResult {
//     return move |input: String| {
//         if input.len() < 1 {
//             return ParseResult::Err(ParseErr::new(0, "expected at least on char".to_string()));
//         }
//         if input.chars().nth(0).unwrap() == c.clone() {
//             return ParseResult::Ok((&c.to_string(), &input[1..].to_string()));
//         }
//         return ParseResult::Err(ParseErr::new(
//             0,
//             format!("expected {} saw {}", c, input.chars().nth(0).unwrap()),
//         ));
//     };
// }

// fn keyword<'a>(word: String) -> impl Fn(String) -> ParseResult {
//    return move |mut input: String| {
//        let word_chars = word.chars();
//        for c in word_chars {
//            match parse_char(c)(input) {
//                Ok((remains, _)) => input = remains,
//                Err(err) => return Err(err),
//            }
//        }
//        return Ok((word.clone(), input));
//    }
// }
// fn any_whitespace<'a>() -> impl Fn(String) -> ParseResult<'a> {
//     let sp = parse_char(' ');
//     let tab = parse_char('\t');
//     let newline = parse_char('\n');
//     return any_of(vec![sp, tab, newline]);
// }

// fn whitespace<'a>() -> impl Fn(String) -> ParseResult<'a> {
//     return zero_or_more(any_whitespace());
// }

// fn any_digits<'a>() -> impl (Fn(String) -> ParseResult<'a>) {
//     return any_of(vec![
//             parse_char('0'),
//             parse_char('1'),
//             parse_char('2'),
//             parse_char('3'),
//             parse_char('4'),
//             parse_char('5'),
//             parse_char('6'),
//             parse_char('7'),
//             parse_char('8'),
//             parse_char('9'),
//         ]);
// }

// fn digits<'a>() -> impl Fn(String) -> ParseResult<'a> {
//     return move |input: String| {
//         match one_or_more(any_digits())(input) {
//             Ok(()) => {},
//             Err(err) => return Err(err),

//         }
//     };
// }

// fn bool<'a>() -> impl Fn(String) -> ParseResult<'a> {
//     let _true = keyword("true".to_string());
//     let _false = keyword("false".to_string());
//     return any_of(vec![_true, _false]);
// }

// #[test]
// fn test_parse_single_digit() {
//     assert_eq!(any_digits()("1AB".to_string()), ParseResult::Ok((Expr::Int(1), "AB".to_string())));
// }

// #[test]
// fn test_parse_digits() {
//     assert_eq!(digits()("1234AB".to_string()), ParseResult::Ok((Expr::Int(1234), "AB".to_string())));
// }

// #[test]
// fn test_parse_keyword() {
//     assert_eq!(keyword("struct".to_string())("struct name".to_string()), ParseResult::Ok((Expr::Misc("struct".to_string()), " name".to_string())));
// }

// #[test]
// fn test_parse_bool() {
//     assert_eq!(bool()("truesomeshitaftertrue".to_string()), ParseResult::Ok((Expr::Bool(true), "someshitaftertrue".to_string())));
//     assert_eq!(bool()("falsesomeshitaftertrue".to_string()), ParseResult::Ok((Expr::Bool(false), "someshitaftertrue".to_string())));
// }


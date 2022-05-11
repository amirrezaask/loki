use super::*;
/*


//////////
    expr:      term | expr '-' term | expr '+' term
    term:      factor | term '*' factor | term '/' factor
    factor:    NUMBER | '(' expr ')'

    expr  -> sum
    sum   -> prod (addop prod)*
    prod  -> value (mulop value)*
    value -> num | "(" expr ")"
    mulop -> "*" |  "/"
    addop -> "+" |  "-"
/////////
    expr :: value | expr + value | expr - value
    value :: term | value * term | value / term
    term :: number | string | bool | fn_call | ident | '('expr')'
    fn_call :: ident(expr*)


    expr -> A
    A -> B (addminus B)*
    B -> C (muldivmod C)*
    C -> number | string | bool | fn_call | ident | '(' expr ')'
    fn_call -> ident(expr*)
*/

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
    let parsers: Vec<fn(String) -> Result<(String, Node), ParseErr>> = vec![
        float,
        uint,
        int,
        _bool,
        string,
        _char,
        _if,
        fn_def,
        _struct,
        inc,
        dec,
        _return,
        fn_call,
        ident,
        array,
        inside_paren,
    ];
    return any_of(parsers)(input);
}

fn inside_paren(input: String) -> ParseResult {
    let (remains, _) = parse_char('(')(input)?;
    let (remains, e) = expr(remains)?;
    let (remains, _) = parse_char(')')(remains)?;
    Ok((remains, e))
}

fn comparisons(input: String) -> ParseResult {
    let p2 = vec![keyword("<=".to_string()), keyword(">=".to_string())];
    return any_of(p2)(input);
}

fn add_minus(input: String) -> ParseResult {
    match comparisons(input.clone()) {
        Ok((remains, p)) => Ok((remains, p)),
        Err(_) => any_of(vec![
            parse_char('+'),
            parse_char('-'),
            parse_char('<'),
            parse_char('>'),
        ])(input.clone()),
    }
}

fn mul_div_mod(input: String) -> ParseResult {
    return any_of(vec![parse_char('*'), parse_char('/'), parse_char('%')])(input);
}

// Vec<Token> -> Module
use crate::tokenizer::{Token, TokenType};
use crate::tokenizer::Errors;
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum ExprType {
    Fn,
    Int,
    Float32,
    Float64,
    String,
    Char,
    If,
}
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
enum Payload {
    Int(i64),
}
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct Expr {
    ty: ExprType,
    payload: Option<Payload>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Decl {
    pub name: String,
    pub expr: Expr,
}

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Decl>, Errors> {
    let decls = vec![];
    let mut current_decl: Option<Decl> = None;

    for (i, tok) in tokens.iter().enumerate() {
        if tok.ty == TokenType::Ident {
            if current_decl.is_none() {
                current_decl = Some(Decl {
                    name: tok.value.as_ref().unwrap().to_string(), // TODO: fix this, do not unwrap
                    expr: Expr {
                        ty: ExprType::Int,
                        payload: None,
                    }
                });
            }
        } else if tok.ty == TokenType::AssignOp {
            let next_tok = tokens.iter().nth(i+1).unwrap();
            if next_tok.ty == TokenType::Number {
                let number:i64 = next_tok.value.as_ref().unwrap().parse().unwrap();
                current_decl.unwrap().expr.payload = Some(Payload::Int(number));
            }
        }

    };

    Ok(decls)

}



#[cfg(test)]
mod tests {
    use super::Token;
    use super::TokenType;
    use super::parse;
    fn eq_vecs<T: Eq + std::fmt::Debug>(v1: Vec<T>, v2: Vec<T>) -> bool {
        if v1.len() != v2.len() {
            assert_eq!(v1.len(), v2.len());
        }
        for i in 0..v1.len() {
            assert_eq!(v1[i], v2[i]);
            if v1[i] != v2[i] {
                return false;
            }
        }
        return true;
    }

    #[test]
    fn parse_constant_assign() {

        let tokens: Vec<Token> = vec![
            Token {
                ty: TokenType::Ident,
                value: Some(String::from("x"))
            },
            Token {
                ty: TokenType::AssignOp,
                value: None,
            },
            Token {
                ty: TokenType::Number,
                value: Some(String::from("12")),
            }
        ];

        let decls = parse(tokens).unwrap();

        eq_vecs(decls, vec![]);


    }
}

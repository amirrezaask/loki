// Vec<Token> -> Module
use crate::tokenizer::{Token, TokenType};
use crate::tokenizer::Errors;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Types {
    Int,
    Float32,
    Float64,
    String,
}

// Decl is everything done in Loki, anything is a declaration either with a name assigned to it or not.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Decl {
    pub name: Option<String>,
    pub ty: Option<Types>, 
    pub expr: Expr,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Expr {
    Int(i64),
    If(If),
    Bool(bool),
    Block(Block),
    Nil,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Block(Vec<Decl>);
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct If {
    pub cond: Box<Expr>,
    pub then_block: Box<Expr>,
    pub else_block: Box<Option<Expr>>,
}

fn as_expr(tokens: Vec<Token>) -> Expr {
    println!("{:?}", tokens);
    if tokens.len() == 1 {
        let tok = tokens[0].clone();
        if tok.ty == TokenType::Number {
            let number:i64 = tok.value.as_ref().unwrap().parse().unwrap();
            return Expr::Int(number);
        } else if tok.ty == TokenType::TrueKeyword || tok.ty == TokenType::FalseKeyword {
            return Expr::Bool(tok.ty == TokenType::TrueKeyword);
        } 
        else {
            panic!("cannot turn token into expression");
        }
    }  else {
        panic!("cannot turn token into expression");
    }

}

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Decl>, Errors> {
    let mut decls = vec![];
    let mut current_decl: Option<Decl> = None;

    for mut i in 0..tokens.len() {
        let current_tok = &tokens[i];
        if current_tok.ty == TokenType::Ident {
            if current_decl.is_none() {
                current_decl = Some(Decl {
                    name: Some(current_tok.value.as_ref().unwrap().to_string()), // TODO: fix this, do not unwrap
                    ty: None,
                    expr: Expr::Nil
                });
            }
        } else if current_tok.ty == TokenType::AssignOp {
            let next_tok = tokens.iter().nth(i+1).unwrap();
            if next_tok.ty == TokenType::Number {
                current_decl.as_mut().unwrap().expr = as_expr(vec![next_tok.clone()]);
                current_decl.as_mut().unwrap().ty = Some(Types::Int);
            } else if next_tok.ty == TokenType::IfKeyword {
                // x = if true {};
                let start_of_cond = i+2; 
                let mut end_of_cond: usize = 0;

                // Finds start and end of condition
                for j in i+2..tokens.len() {
                    if tokens[j].ty == TokenType::CuBracketOpen {
                        end_of_cond = j-1;
                        break;
                    } 
                }
                
                if end_of_cond == 0 {
                    panic!("cannot find end token of if condition");
                }
                let cond_expr = as_expr(tokens[start_of_cond..end_of_cond].to_vec());

                // Finds start and end of then block code.
                let start_of_then_block = end_of_cond + 1;
                let mut end_of_then_block: usize = 0;
                let mut cu_still_open = 1;

                for j in start_of_then_block+1..tokens.len() {
                    if tokens[j].ty == TokenType::CuBracketOpen {
                        cu_still_open += 1;
                    } else if tokens[j].ty == TokenType::CuBracketClose {
                        cu_still_open -= 1;
                    }

                    if cu_still_open == 0 {
                        end_of_then_block = j;
                    }
                }

                let then_expr = as_expr(tokens[start_of_then_block..end_of_then_block].to_vec());
                let mut else_expr: Option<Expr> = None;
                // Finds start and end of else block code.
                let start_of_else_block = end_of_then_block+2;
                let mut end_of_else_block: usize = 0;
                if !(end_of_then_block >= tokens.len()) && tokens[end_of_then_block+2].ty == TokenType::ElseKeyword {
                    let mut cu_still_open = 1;
                    for j in start_of_else_block+1..tokens.len() {
                        if tokens[j].ty == TokenType::CuBracketOpen {
                            cu_still_open += 1;
                        } else if tokens[j].ty == TokenType::CuBracketClose {
                            cu_still_open -= 1;
                        }

                        if cu_still_open == 0 {
                            end_of_else_block = j;
                        }
                    }

                    else_expr = Some(as_expr(tokens[start_of_else_block..end_of_else_block].to_vec()));
                }
                current_decl.as_mut().unwrap().expr = Expr::If(If {
                    cond: Box::new(cond_expr),
                    then_block: Box::new(then_expr),
                    else_block: Box::new(else_expr.clone())
                });
                let mut end_of_if = end_of_then_block + 1;
                if else_expr.clone().is_some() {
                    end_of_if = end_of_else_block + 1;
                }
                i = end_of_if;
            };

        } 
    };

    Ok(decls)

}



#[cfg(test)]
mod tests {
    use super::Token;
    use super::Types;
    use super::TokenType;
    use super::Expr;
    use super::parse;
    use super::Decl;
    use super::If;
    use super::Block;
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
            },
            Token {
                ty: TokenType::SemiColon,
                value: None,
            }
        ];

        let decls = parse(tokens).unwrap();

        eq_vecs(decls, vec![Decl{
            name: Some("x".to_string()),
            ty: Some(Types::Int),
            expr: Expr::Int(12),
        }]);


    }

    #[test]
    fn parse_assign_if() {
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
                ty: TokenType::IfKeyword,
                value: None
            },
            Token {
                ty: TokenType::TrueKeyword,
                value: None,
            },
            Token {
                ty: TokenType::CuBracketOpen,
                value: None,
            },
            Token {
                ty: TokenType::TrueKeyword,
                value: None,
            },
            Token {
                ty: TokenType::CuBracketClose,
                value: None,
            },
            
        ];

        let decls = parse(tokens).unwrap();

        eq_vecs(decls, vec![Decl{
            name: Some("x".to_string()),
            ty: None,
            expr: Expr::If(If {
                cond: Box::new(Expr::Bool(true)), 
                then_block: Box::new(Expr::Block(Block(vec![Decl{
                    name: None,
                    ty: None,
                    expr: Expr::Bool(true)
                }]))), 
                else_block: Box::new(None),
            }),
        }]);
    }
}

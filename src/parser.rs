use std::usize;

// Vec<Token> -> Module
use crate::tokenizer::Errors;
use crate::tokenizer::{Token, TokenType};

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
    FnCall(FnCall),
    Str(String),
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FnCall {
    name: String,
    args: Vec<Expr>,
}

struct Parser {
    tokens: Vec<Token>,
    cur: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, cur: 0 }
    }

    pub fn next_expr(&mut self) -> Result<Expr, Errors> {
        // 10; [x]
        // ""; [x]
        // fncall(arg1, arg2);
        // if true {} else {};
        // struct {}
        // interface {}
        // union {}
        // enum {}
        // fn(arg1: t1): void {}
        if self.cur_token().ty == TokenType::Number {
            let num: i64 = self.cur_token().value.as_ref().unwrap().parse().unwrap();
            self.cur += 1;
            return Ok(Expr::Int(num));
        } else if self.cur_token().ty == TokenType::DoubleQuoteStart {
            let string = self.tokens[self.cur + 1]
                .value
                .as_ref()
                .unwrap()
                .to_string();
            self.cur += 2;
            return Ok(Expr::Str(string));
        } else if self.cur_token().ty == TokenType::TrueKeyword {
            self.cur += 1;
            return Ok(Expr::Bool(true));
        } else if self.cur_token().ty == TokenType::FalseKeyword {
            self.cur += 1;
            return Ok(Expr::Bool(false));
        } else if self.cur_token().ty == TokenType::Ident
            && self.tokens[self.cur + 1].ty == TokenType::ParenOpen
        {
            // function call
            //TODO: handle commas :))
            let name_ident = self.cur_token().clone();
            let mut args: Vec<Expr> = Vec::default();

            self.cur += 2; // stand on first argument
            if self.cur_token().ty == TokenType::ParenClose {
                // we dont have any args.
                return Ok(Expr::FnCall(FnCall {
                    name: name_ident.value.as_ref().unwrap().to_string(),
                    args,
                }));
            }
            // we have args
            // fn(fn(12), 12, 12)
            loop {
                if self.cur_token().ty == TokenType::ParenClose {
                    self.cur+=1;
                    break;
                } 
                if self.cur_token().ty == TokenType::Comma {
                    self.cur+=1;
                }

                if self.cur_token().ty == TokenType::ParenOpen {
                    self.cur += 1;
                }
                args.push(self.next_expr().unwrap());
            }
            return Ok(Expr::FnCall(FnCall {
                name: name_ident.value.as_ref().unwrap().to_string(),
                args,
            }));
        } else {
            return Err(Errors::ParseErr("cannot create expr".to_string()));
        }
    }

    fn cur_token(&self) -> &Token {
        return &self.tokens[self.cur];
    }

    pub fn parse(&mut self) -> Result<Vec<Decl>, Errors> {
        let mut decls = vec![];

        Ok(decls)
    }
}

#[cfg(test)]
mod tests {
    use super::Block;
    use super::Decl;
    use super::Expr;
    use super::FnCall;
    use super::If;
    use super::Parser;
    use super::Token;
    use super::TokenType;
    use super::Types;
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
    fn next_expr_bool() {
        let tokens: Vec<Token> = vec![Token {
            ty: TokenType::TrueKeyword,
            value: None,
        }];

        let mut parser = Parser::new(tokens);
        assert_eq!(Expr::Bool(true), parser.next_expr().unwrap());
    }
    #[test]
    fn next_expr_fn_with_args_nested() {
        // fn_name(fn_name2(fn_name3(12)), 12, 12)
        let tokens: Vec<Token> = vec![
            Token {
                ty: TokenType::Ident,
                value: Some(String::from("fn_name")),
            },
            Token {
                ty: TokenType::ParenOpen,
                value: None,
            },
            Token {
                ty: TokenType::Ident,
                value: Some(String::from("fn_name2")),
            },
            Token {
                ty: TokenType::ParenOpen,
                value: None,
            },
            Token {
                ty: TokenType::Ident,
                value: Some("fn_name3".to_string()),
            },
            Token {
                ty: TokenType::ParenOpen,
                value: None,
            },
            Token {
                ty: TokenType::Number,
                value: Some(String::from("12"))
            },
            Token {
                ty: TokenType::ParenClose,
                value: None,
            },
            Token {
                ty: TokenType::ParenClose,
                value: None,
            },


            Token {
                ty: TokenType::Comma,
                value: None,
            },
            Token {
                ty: TokenType::Number,
                value: Some("12".to_string()),
            },
            Token {
                ty: TokenType::Comma,
                value: None,
            },
            Token {
                ty: TokenType::Number,
                value: Some("12".to_string()),
            },
            Token {
                ty: TokenType::ParenClose,
                value: None,
            },
        ];

        let mut parser = Parser::new(tokens);
        assert_eq!(
            Expr::FnCall(FnCall {
                name: "fn_name".to_string(),
                args: vec![
                    Expr::FnCall(FnCall {
                        name: "fn_name2".to_string(),
                        args: vec![Expr::FnCall(FnCall {
                            name: "fn_name3".to_string(),
                            args: vec![Expr::Int(12)],
                        })],
                    }),
                    Expr::Int(12),
                    Expr::Int(12),
                ],
            }),
            parser.next_expr().unwrap()
        );
    }
    #[test]
    fn next_expr_fn_with_args_flat() {
        let tokens: Vec<Token> = vec![
            Token {
                ty: TokenType::Ident,
                value: Some(String::from("fn_name")),
            },
            Token {
                ty: TokenType::ParenOpen,
                value: None,
            },
            Token {
                ty: TokenType::Number,
                value: Some("12".to_string()),
            },
            Token {
                ty: TokenType::Number,
                value: Some("12".to_string()),
            },
            Token {
                ty: TokenType::ParenClose,
                value: None,
            },
        ];

        let mut parser = Parser::new(tokens);
        assert_eq!(
            Expr::FnCall(FnCall {
                name: "fn_name".to_string(),
                args: vec![Expr::Int(12), Expr::Int(12),],
            }),
            parser.next_expr().unwrap()
        );
    }
    #[test]
    fn next_expr_fn_call() {
        let tokens: Vec<Token> = vec![
            Token {
                ty: TokenType::Ident,
                value: Some(String::from("fn_name")),
            },
            Token {
                ty: TokenType::ParenOpen,
                value: None,
            },
            Token {
                ty: TokenType::ParenClose,
                value: None,
            },
        ];

        let mut parser = Parser::new(tokens);
        assert_eq!(
            Expr::FnCall(FnCall {
                name: "fn_name".to_string(),
                args: Vec::default(),
            }),
            parser.next_expr().unwrap()
        );
    }

    #[test]
    fn next_expr_string() {
        let tokens: Vec<Token> = vec![
            Token {
                ty: TokenType::DoubleQuoteStart,
                value: None,
            },
            Token {
                ty: TokenType::StringLiteral,
                value: Some(String::from("amirreza")),
            },
            Token {
                ty: TokenType::DoubleQuoteEnd,
                value: None,
            },
        ];

        let mut parser = Parser::new(tokens);
        assert_eq!(
            Expr::Str("amirreza".to_string()),
            parser.next_expr().unwrap()
        );
    }
    #[test]
    fn next_expr_number() {
        let tokens: Vec<Token> = vec![Token {
            ty: TokenType::Number,
            value: Some(String::from("12")),
        }];

        let mut parser = Parser::new(tokens);
        assert_eq!(Expr::Int(12), parser.next_expr().unwrap());
    }

    // #[test]
    // fn parse_constant_assign() {

    //     let tokens: Vec<Token> = vec![
    //         Token {
    //             ty: TokenType::Ident,
    //             value: Some(String::from("x"))
    //         },
    //         Token {
    //             ty: TokenType::AssignOp,
    //             value: None,
    //         },
    //         Token {
    //             ty: TokenType::Number,
    //             value: Some(String::from("12")),
    //         },
    //         Token {
    //             ty: TokenType::SemiColon,
    //             value: None,
    //         }
    //     ];

    //     let decls = parse(tokens).unwrap();

    //     eq_vecs(decls, vec![Decl{
    //         name: Some("x".to_string()),
    //         ty: Some(Types::Int),
    //         expr: Expr::Int(12),
    //     }]);

    // }

    // // #[test]
    // fn parse_assign_if() {
    //     let tokens: Vec<Token> = vec![
    //         Token {
    //             ty: TokenType::Ident,
    //             value: Some(String::from("x"))
    //         },
    //         Token {
    //             ty: TokenType::AssignOp,
    //             value: None,
    //         },
    //         Token {
    //             ty: TokenType::IfKeyword,
    //             value: None
    //         },
    //         Token {
    //             ty: TokenType::TrueKeyword,
    //             value: None,
    //         },
    //         Token {
    //             ty: TokenType::CuBracketOpen,
    //             value: None,
    //         },
    //         Token {
    //             ty: TokenType::TrueKeyword,
    //             value: None,
    //         },
    //         Token {
    //             ty: TokenType::CuBracketClose,
    //             value: None,
    //         },

    //     ];

    //     let decls = parse(tokens).unwrap();

    //     eq_vecs(decls, vec![Decl{
    //         name: Some("x".to_string()),
    //         ty: None,
    //         expr: Expr::If(If {
    //             cond: Box::new(Expr::Bool(true)),
    //             then_block: Box::new(Expr::Block(Block(vec![Decl{
    //                 name: None,
    //                 ty: None,
    //                 expr: Expr::Bool(true)
    //             }]))),
    //             else_block: Box::new(None),
    //         }),
    //     }]);
    // }
}

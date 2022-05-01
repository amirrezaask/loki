#![allow(dead_code)]
// Vec<Token> -> Module
use crate::tokenizer::Errors;
use crate::tokenizer::{Token, TokenType};

// Basic types of loki.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Type {
    Struct,
    Union,
    Enum,
    Int,
    Str,
}

// Decl is everything done in Loki, anything is a declaration either with a name assigned to it or not.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Decl {
    pub name: Option<String>,
    pub ty: Option<Type>,
    pub expr: Expr,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Expr {
    Int(i64),
    If(If),
    Bool(bool),
    FnCall(FnCall),
    Ref(String),
    Str(String),
    Nil,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct If {
    pub cond: Box<Expr>,
    pub then_block: Box<Expr>,
    pub else_block: Box<Option<Expr>>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FnCall {
    pub name: String,
    pub args: Vec<Expr>,
}

pub struct Parser {
    tokens: Vec<Token>,
    cur: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, cur: 0 }
    }

    pub fn parse_next_expr(&mut self) -> Result<Expr, Errors> {
        let mut expr_stack: Vec<Expr> = vec![];
        // identifier []
        // 10; [x]
        // ""; [x]
        // expr op expr []
        // fncall(arg1, arg2); [x]
        // if true {} else {}; []
        // struct {} []
        // interface {} []
        // union {} []
        // enum {} []
        // fn(arg1: t1): void {} []
        'outer: loop {
            if self.cur >= self.tokens.len() {
                break;
            }
            println!("{:?}", self.cur_token());
            if self.cur_token().ty == TokenType::Number {
                let num: i64 = self.cur_token().value.as_ref().unwrap().parse().unwrap();
                self.cur += 1;
                expr_stack.push(Expr::Int(num));
            } else if self.cur_token().ty == TokenType::Ident
                && self.tokens[self.cur + 1].ty == TokenType::SemiColon
            {
                expr_stack.push(Expr::Ref(
                    self.cur_token().value.as_ref().unwrap().to_string(),
                ));
                self.cur += 2;
            } else if self.cur_token().ty == TokenType::DoubleQuoteStart {
                self.cur += 1; // move cursor to string literal token
                let string = self.tokens[self.cur].value.as_ref().unwrap().to_string();
                self.cur += 2;
                expr_stack.push(Expr::Str(string));
            } else if self.cur_token().ty == TokenType::TrueKeyword {
                self.cur += 1;
                expr_stack.push(Expr::Bool(true));
            } else if self.cur_token().ty == TokenType::FalseKeyword {
                self.cur += 1;
                expr_stack.push(Expr::Bool(false));
            } else if self.cur_token().ty == TokenType::Ident
                && self.tokens[self.cur + 1].ty == TokenType::ParenOpen
            // Function call
            {
                // fn_name(fn_name2(fn_name3(12)), 12, 12)
                let name_ident = self.cur_token().clone();
                println!("resolving {}", name_ident.value.as_ref().unwrap());
                expr_stack.push(Expr::FnCall(FnCall {
                    name: name_ident.value.as_ref().unwrap().to_string(),
                    args: Vec::default(),
                }));

                self.cur += 2; // move cursor to first argument
                if self.cur_token().ty != TokenType::ParenClose {
                    loop {
                        if self.cur_token().ty == TokenType::ParenClose {
                            self.cur += 1; // move over the paren close token
                            break;
                        }
                        if self.cur_token().ty == TokenType::Comma {
                            self.cur += 1; // move over the comma token
                        }

                        if self.cur_token().ty == TokenType::ParenOpen {
                            self.cur += 1;
                        }
                        continue 'outer;
                    }
                }
            } else if self.cur_token().ty == TokenType::ParenClose {
                //closing a paren, probably a function call
                let mut args: Vec<Expr> = vec![];
                loop {
                    if let Some(expr) = expr_stack.pop() {
                        match expr {
                            Expr::FnCall(mut fn_call) => {
                                if fn_call.args.len() > 0 {
                                    args.push(Expr::FnCall(fn_call));
                                    continue;
                                }
                                let mut args_clone = args.clone();
                                args_clone.reverse();
                                fn_call.args = args_clone;
                                println!("{:?}", fn_call);
                                expr_stack.push(Expr::FnCall(fn_call));
                                break;
                            }
                            _ => args.push(expr),
                        }
                    }
                }

                self.cur += 1;
            } else if self.cur_token().ty == TokenType::Comma {
                self.cur += 1;
            } else {
                println!("{:?}", self.cur_token());
                return Err(Errors::ParseErr("cannot create expr".to_string()));
            }
        }
        if expr_stack.len() > 0 {
            return Ok(expr_stack[expr_stack.len() - 1].clone());
        } else {
            return Err(Errors::ParseErr("No expr constructed".to_string()));
        }
    }

    fn cur_token(&self) -> &Token {
        return &self.tokens[self.cur];
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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
    fn parse_bool() {
        let tokens: Vec<Token> = vec![Token {
            ty: TokenType::TrueKeyword,
            value: None,
        }];

        let mut parser = Parser::new(tokens);
        assert_eq!(Expr::Bool(true), parser.parse_next_expr().unwrap());
    }
    #[test]
    fn parse_fn_with_args_nested() {
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
                value: Some(String::from("12")),
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
            parser.parse_next_expr().unwrap()
        );
    }
    #[test]
    fn parse_fn_with_args_flat() {
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
            parser.parse_next_expr().unwrap()
        );
    }
    #[test]
    fn parse_fn_call() {
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
            parser.parse_next_expr().unwrap()
        );
    }

    #[test]
    fn parse_string() {
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
            parser.parse_next_expr().unwrap()
        );
    }
    #[test]
    fn parse_variable_ref() {
        let tokens: Vec<Token> = vec![
            Token {
                ty: TokenType::Ident,
                value: Some(String::from("x")),
            },
            Token {
                ty: TokenType::SemiColon,
                value: None,
            },
        ];

        let mut parser = Parser::new(tokens);
        assert_eq!(Expr::Ref("x".to_string()), parser.parse_next_expr().unwrap());
    }
    #[test]
    fn parse_number() {
        let tokens: Vec<Token> = vec![Token {
            ty: TokenType::Number,
            value: Some(String::from("12")),
        }];

        let mut parser = Parser::new(tokens);
        assert_eq!(Expr::Int(12), parser.parse_next_expr().unwrap());
    }
}

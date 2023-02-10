use std::collections::HashMap;

use crate::new_impl;
use crate::new_impl::ast::{InfixOperator, PrefixOperator};

use super::lexer::Token;

use super::{
    ast::{Expression, Node, Statement},
    lexer::{TokenType, Tokenizer},
};

pub struct Parser {
    tokenizer: Tokenizer,
}

const LOWEST: u8 = 1;
const SUM: u8 = 2;
const PRODUCT: u8 = 3;
const PREFIX: u8 = 4;
const POSTFIX: u8 = 5;
const CALL: u8 = 6;

impl Parser {
    pub fn new(tokenizer: Tokenizer) -> Self {
        Self { tokenizer }
    }

    pub fn parse(&mut self) -> Node {
        Node::Statement(Statement::Block(vec![]))
    }

    fn prefix(&mut self, t: Token) -> Node {
        match t.ty {
            TokenType::Plus | TokenType::Minus | TokenType::Asterix | TokenType::Bang => {
                let operand = self.parse_expr(self.precedence(&t.ty));
                println!("inja: {:?}", operand);
                Node::Expression(Expression::PrefixOperation {
                    op: t.ty.into(),
                    x: Box::new(operand),
                })
            }
            TokenType::Ident | TokenType::UnsignedInt => Node::Expression(Expression::Value(t)),
            _ => panic!("prefixParselet: got {:?}", t),
        }
    }

    fn infix(&mut self, t: Token, lhs: Node) -> Node {
        match t.ty {
            TokenType::Plus
            | TokenType::Minus
            | TokenType::ForwardSlash
            | TokenType::Percent
            | TokenType::Asterix => {
                println!("infix: {:?} lhs: {:?}", t, lhs);
                return Node::Expression(Expression::InfixOperation {
                    op: t.ty.clone().into(),
                    lhs: Box::new(lhs),
                    rhs: Box::new(self.parse_expr(self.precedence(&t.ty))),
                });
            }

            _ => panic!("infix: Got: {:?}", t),
        }
    }

    fn precedence(&self, t: &TokenType) -> u8 {
        match t {
            TokenType::Plus | TokenType::Minus => SUM,
            TokenType::Asterix | TokenType::ForwardSlash | TokenType::Percent => PRODUCT,
            _ => LOWEST,
        }
    }

    fn parse_expr(&mut self, precedence: u8) -> Node {
        println!("==========");
        println!("parse_expr predence: {precedence}");
        let token = self.tokenizer.next().unwrap();
        println!("token: {:?}", &token);

        let mut lhs = self.prefix(token);
        println!("lhs: {:?}", &lhs);

        let infix_op = self.tokenizer.peek();
        match infix_op {
            Some(op) => {
                println!("infix op: {:?}", op);
                loop {
                    let t = self.tokenizer.next();
                    if t.is_none() {
                        break lhs;
                    }
                    let t = t.unwrap();
                    if !precedence <= self.precedence(&t.ty) {
                        break lhs;
                    } else {
                        lhs = self.infix(op.clone(), lhs);
                    }
                }
            },
            None => lhs
        }
        
    }
}

#[test]
fn test_expr_prefix_operation() {
    let mut p = Parser::new(Tokenizer::new(String::from(""), "-1"));
    let node = p.parse_expr(LOWEST);

    assert_eq!(
        node,
        Node::Expression(Expression::PrefixOperation {
            op: PrefixOperator::Negate,
            x: Box::new(Node::Expression(Expression::Value(Token {
                ty: TokenType::UnsignedInt,
                loc: (2, 2),
                line: 0,
                col: 0
            })))
        })
    )
}

#[test]
fn test_expr_infix_operation() {
    let mut p = Parser::new(Tokenizer::new(String::from(""), "1 + 2"));
    let node = p.parse_expr(LOWEST);
    assert_eq!(
        node,
        Node::Expression(Expression::InfixOperation {
            op: InfixOperator::Sum,
            lhs: Box::new(Node::Expression(Expression::Value(Token {
                ty: TokenType::UnsignedInt,
                loc: (1, 1),
                line: 0,
                col: 0
            }))),
            rhs: Box::new(Node::Expression(Expression::Value(Token {
                ty: TokenType::UnsignedInt,
                loc: (5, 5),
                line: 0,
                col: 0
            })))
        })
    )
}
#[test]
fn test_expr_infix_operation_2() {
    let mut p = Parser::new(Tokenizer::new(String::from(""), "1 + 2 * 3"));
    let node = p.parse_expr(LOWEST);
    assert_eq!(
        node,
        Node::Expression(Expression::InfixOperation {
            op: InfixOperator::Sum,
            lhs: Box::new(Node::Expression(Expression::Value(Token {
                ty: TokenType::UnsignedInt,
                loc: (1, 1),
                line: 0,
                col: 0
            }))),
            rhs: Box::new(Node::Expression(Expression::InfixOperation { 
                op: InfixOperator::Multiply,
                lhs: Box::new(Node::Expression(Expression::Value(Token { ty: TokenType::UnsignedInt, loc: (5, 5), line: 0, col: 0 }))),
                rhs: Box::new(Node::Expression(Expression::Value(Token { ty: TokenType::UnsignedInt, loc: (9, 9), line: 0, col: 0 }))),
            }))
        })
    )
}


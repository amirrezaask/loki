use std::collections::HashMap;
use std::ops::Deref;

use serde::de::value;

use crate::syn;
use crate::syn::ast::{InfixOperator, PrefixOperator};

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
const INDEX: u8 = 7;

fn value_parser(p: &mut Parser, t: &Token) -> Node {
    Node::Expression(Expression::Value(t.clone()))
}

fn prefix_parser(p: &mut Parser, t: &Token) -> Node {
    let operand = p.parse_expr(p.precedence(&t.ty));
    Node::Expression(Expression::PrefixOperation {
        op: t.ty.clone().into(),
        x: Box::new(operand),
    })
}

fn infix_operation_parser(p: &mut Parser, lhs: Node, t: &Token) -> Node {
    return Node::Expression(Expression::InfixOperation {
        op: t.ty.clone().into(),
        lhs: Box::new(lhs),
        rhs: Box::new(p.parse_expr(p.precedence(&t.ty))),
    });
}
fn infix_call(p: &mut Parser, lhs: Node, t: &Token) -> Node {
    println!("ina");
    let mut args = vec![];
    loop {
        let arg = p.parse_expr(LOWEST);
        args.push(arg);

        let token = p.tokenizer.next();
        match token {
            Some(Token {
                ty: TokenType::RightParen,
                loc,
                line,
                col,
            }) => {
                break;
            }
            Some(Token {
                ty: TokenType::Comma,
                loc,
                line,
                col,
            }) => {
                println!("inja");
                continue;
            }
            None => break,
            _ => panic!(
                "function call should end with ) or seperated with , {:?}",
                token
            ),
        }
    }

    return Node::Expression(Expression::Call {
        callable: Box::new(lhs),
        args: args,
    });
}

fn infix_index(p: &mut Parser, lhs: Node, t: &Token) -> Node {
    Node::Expression(Expression::InfixOperation {
        op: InfixOperator::Index,
        lhs: Box::new(lhs),
        rhs: Box::new(p.parse_expr(LOWEST)),
    })
}

impl Parser {
    pub fn new(tokenizer: Tokenizer) -> Self {
        let mut s = Self { tokenizer };
        s
    }

    pub fn parse(&mut self) -> Node {
        Node::Statement(Statement::Block(vec![]))
    }

    fn prefix_parser(&mut self, t: Token) -> Node {
        match t.ty {
            TokenType::Plus | TokenType::Minus => return prefix_parser(self, &t),

            TokenType::Ident | TokenType::UnsignedInt => return value_parser(self, &t),
            _ => unreachable!(),
        }
    }

    fn infix_parser(&mut self, t: Token) -> Option<Box<dyn Fn(&mut Parser, Node, &Token) -> Node>> {
        match t.ty {
            TokenType::Plus
            | TokenType::Minus
            | TokenType::ForwardSlash
            | TokenType::Percent
            | TokenType::Asterix => {
                return Some(Box::new(infix_operation_parser));
            }

            TokenType::LeftBracket => {
                return Some(Box::new(infix_index));
            }

            TokenType::LeftParen => return Some(Box::new(infix_call)),

            _ => None,
        }
    }

    fn precedence(&self, t: &TokenType) -> u8 {
        match t {
            TokenType::Plus | TokenType::Minus => SUM,
            TokenType::Asterix | TokenType::ForwardSlash | TokenType::Percent => PRODUCT,
            TokenType::LeftParen => CALL,
            TokenType::LeftBracket => INDEX,
            _ => LOWEST,
        }
    }

    fn parse_expr(&mut self, precedence: u8) -> Node {
        let token = self.tokenizer.next().unwrap();
        println!("token: {:?}", &token);

        let mut lhs = self.prefix_parser(token);
        println!("lhs: {:?}", &lhs);

        loop {
            let token = self.tokenizer.peek();
            println!("in loop token: {:?}", token);
            match token {
                None => return lhs,
                Some(t) => {
                    if precedence < self.precedence(&t.ty) {
                        let parser = self.infix_parser(t.clone());
                        println!("infix parser:{:?} {}", t, parser.is_some());
                        if parser.is_none() {
                            return lhs;
                        }
                        self.tokenizer.next();
                        lhs = parser.unwrap()(self, lhs, &t);
                    } else {
                        break lhs;
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod PrefixTests {
    use super::*;
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
                    loc: (1, 1),
                    line: 0,
                    col: 0
                })))
            })
        )
    }
}

#[cfg(test)]
mod InfixTests {

    use super::*;
    #[test]
    fn simple_sum() {
        let mut p = Parser::new(Tokenizer::new(String::from(""), "1 + 2"));
        let node = p.parse_expr(LOWEST);
        assert_eq!(
            node,
            Node::Expression(Expression::InfixOperation {
                op: InfixOperator::Sum,
                lhs: Box::new(Node::Expression(Expression::Value(Token {
                    ty: TokenType::UnsignedInt,
                    loc: (0, 0),
                    line: 0,
                    col: 0
                }))),
                rhs: Box::new(Node::Expression(Expression::Value(Token {
                    ty: TokenType::UnsignedInt,
                    loc: (4, 4),
                    line: 0,
                    col: 0
                })))
            })
        );
    }

    #[test]
    fn call() {
        let mut p = Parser::new(Tokenizer::new(String::from(""), "a(1, 2)"));
        let node = p.parse_expr(LOWEST);

        println!("node: {:?}", node);
        assert_eq!(
            Node::Expression(Expression::Call {
                callable: Box::new(Node::Expression(Expression::Value(Token {
                    ty: TokenType::Ident,
                    loc: (0, 0),
                    line: 0,
                    col: 0
                }))),
                args: vec![
                    Node::Expression(Expression::Value(Token {
                        ty: TokenType::UnsignedInt,
                        loc: (2, 2),
                        line: 0,
                        col: 0
                    })),
                    Node::Expression(Expression::Value(Token {
                        ty: TokenType::UnsignedInt,
                        loc: (5, 5),
                        line: 0,
                        col: 0
                    }))
                ]
            }),
            node
        );
    }

    #[test]
    fn index() {
        let mut p = Parser::new(Tokenizer::new(String::from(""), "a[1]"));
        let node = p.parse_expr(LOWEST);
        println!("node: {:?}", node);

        assert_eq!(
            node,
            Node::Expression(Expression::InfixOperation {
                op: InfixOperator::Index,
                lhs: Box::new(Node::Expression(Expression::Value(Token {
                    ty: TokenType::Ident,
                    loc: (0, 0),
                    line: 0,
                    col: 0
                }))),
                rhs: Box::new(Node::Expression(Expression::Value(Token {
                    ty: TokenType::UnsignedInt,
                    loc: (2, 2),
                    line: 0,
                    col: 0
                })))
            })
        )
    }

    fn sum_and_multiply() {
        let mut p = Parser::new(Tokenizer::new(String::from(""), "1 + 2 * 3"));
        let node = p.parse_expr(LOWEST);
        assert_eq!(
            node,
            Node::Expression(Expression::InfixOperation {
                op: InfixOperator::Sum,
                lhs: Box::new(Node::Expression(Expression::Value(Token {
                    ty: TokenType::UnsignedInt,
                    loc: (0, 0),
                    line: 0,
                    col: 0
                }))),
                rhs: Box::new(Node::Expression(Expression::InfixOperation {
                    op: InfixOperator::Multiply,
                    lhs: Box::new(Node::Expression(Expression::Value(Token {
                        ty: TokenType::UnsignedInt,
                        loc: (4, 4),
                        line: 0,
                        col: 0
                    }))),
                    rhs: Box::new(Node::Expression(Expression::Value(Token {
                        ty: TokenType::UnsignedInt,
                        loc: (8, 8),
                        line: 0,
                        col: 0
                    }))),
                }))
            })
        );
    }
}

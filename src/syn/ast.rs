use super::lexer::{Token, TokenType};

#[derive(PartialEq, Debug)]

pub enum Statement {
    Block(Vec<Statement>)
}

#[derive(PartialEq, Debug)]

pub enum PrefixOperator {
    Negate,
    Not,

    Ref,
    Deref,
}

impl From<TokenType> for PrefixOperator {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::Bang => Self::Not,
            TokenType::Minus => Self::Negate,
            _ => panic!()
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum InfixOperator {
    Sum,
    Subtract,
    Multiply,
    Mod,
    Divide,

    And,
    Or,

    Index,
    Selector
}

impl From<TokenType> for InfixOperator {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::Plus => Self::Sum,
            TokenType::Minus => Self::Subtract,
            TokenType::Asterix => Self::Multiply,
            TokenType::Percent => Self::Mod,
            TokenType::ForwardSlash => Self::Divide,
            TokenType::DoubleAmpersand => Self::And,
            TokenType::DoublePipe => Self::Or,
	    TokenType::LeftBracket => Self::Index,
	    TokenType::Dot => Self::Selector,
            _ => unreachable!()
        }
    }
}

#[derive(PartialEq, Debug)]

pub enum PostfixOperator {
    Increment,
    Decrement,
}

impl From<TokenType> for PostfixOperator {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::DoublePlus => Self::Increment,
            TokenType::DoubleMinus => Self::Decrement,

            _ => unreachable!()
        }
    }
}

#[derive(PartialEq, Debug)]

pub enum Expression {
    PrefixOperation {
        op: PrefixOperator,
        x: Box<Node>,
    },

    InfixOperation {
        op: InfixOperator,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },

    PostfixOperation {
        x: Box<Node>,
        op: PostfixOperator,
    },

    Call {
        callable: Box<Node>,
        args: Vec<Node>,
    },

    Value(Token)
}

#[derive(PartialEq, Debug)]
pub enum Node {
    Statement(Statement),
    Expression(Expression)
}

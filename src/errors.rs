use crate::lexer::{TokenType, Token, State};

use super::{ir::NodeIndex, typer::Type};


pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum TypeCheckError {
    DependencyToIdentifier {
        filename: String,
        dependent_node: NodeIndex,
        identifier: String,
    },
    StructDoesNotHaveField(Type, String),
    EnumDoesNotHaveVariant(Type, String),
    InvalidNamespace(Type),
    OnlyArraysCanBeIndexed(Type),
    ArrayElementsShouldBeOfSameType(Type, Type),
    ArrayIndexShouldBeEitherUnsignedOrSignedInt(Type),
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        expected: TokenType,
    },

    UnknownExpression(TokenType),
    UnknownStatement(TokenType),
    UnknownOperation(TokenType),
    InvalidUnsignedInt(Token),
    InvalidSignedInt(Token),
    InvalidFloat(Token),
    UnknownNode(NodeIndex),
    Unknown(String)
}

#[derive(Debug)]
pub enum TokenizerError {
    InvalidChar {
        state: State
    }
}

#[derive(Debug)]
pub enum CompilationError {
    ReadFileError(String),
}

#[derive(Debug)]
pub enum Reason {
    TokenizerError(TokenizerError),
    ParseError(ParseError),
    TypeCheckError(TypeCheckError),
    CompliationUnitError(CompilationError),
}

#[derive(Debug)]
pub struct Error {
    pub filename: String,
    pub line: usize,
    pub col: usize,
    pub reason: Reason,
    
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}

impl std::error::Error for Error {}
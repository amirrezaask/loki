use crate::tokenizer::Token;
use crate::tokenizer::Tokenizer;
use crate::tokenizer::Type;
use anyhow::{anyhow, Result};

#[derive(Debug)]
pub struct Import {
    // this usize refer to src location.
    path: usize,
    _as: Option<usize>,
}

#[derive(Debug)]
pub struct Decl {
    mutable: bool,
    lhs: Box<Node>,
    rhs: Box<Node>,
}

#[derive(Debug)]
pub enum Node {
    //top level items
    Import(Import),
    Decl(Decl),

    // primitive types
    Uint(usize),
    Int(usize),
    StringLiteral(usize),
    Float(usize),
    True(usize),
    False(usize),
    Char(usize),

    Ident(usize),

    // keywords
    IntTy(usize),
    FloatTy(usize),
    UintTy(usize),
    BoolTy(usize),
    StringTy(usize),
    CharTy(usize),
    VoidTy(usize),
}

#[derive(Debug)]
pub struct AST {
    top_level: Vec<Node>,
}

#[derive(Default, Debug)]
enum State {
    #[default]
    Start,
    Block,
    Import,
    ImportWaitingForString,
    WaitingForExpr,
    SawIdentifier,
    VarDecl,
    ConstDecl,
    SawIf,
    SawFn,
}

#[derive(Debug, Default)]
struct Parser {
    tokens: Vec<Token>,
    cur: usize,
    state: State,
}

impl Parser {
    fn err_uexpected(&self, what: Type) -> anyhow::Error {
        anyhow!(
            "Expected {:?}, found {:?} at {:?}",
            what,
            self.current_token(),
            self.current_token().loc
        )
    }
    fn current_token(&self) -> &Token {
        return &self.tokens[self.cur];
    }
    fn forward_token(&mut self) {
        self.cur += 1;
    }
    pub fn new(src: &str) -> Result<Self> {
        let mut tokenizer = Tokenizer::new(src);
        let mut tokens = Vec::<Token>::new();
        loop {
            let tok = tokenizer.next()?;
            match tok.ty {
                Type::EOF => {
                    break;
                }
                _ => {
                    tokens.push(tok);
                }
            }
        }
        Ok(Self {
            tokens: tokens,
            state: State::Start,
            cur: 0,
        })
    }
    fn expect_ident(&mut self) -> Result<Node> {
        match self.current_token().ty {
            Type::Identifier => {
                return Ok(Node::Ident(self.cur));
            }
            _ => {
                return Err(self.err_uexpected(Type::Identifier));
            }
        }
    }
    fn expect_string_literal(&mut self) -> Result<Node> {
        match self.current_token().ty {
            Type::StringLiteral => {
                return Ok(Node::StringLiteral(self.cur));
            }
            _ => {
                return Err(self.err_uexpected(Type::StringLiteral));
            }
        }
    }
    fn expect_import(&mut self) -> Result<Node> {
        self.forward_token();

        let path_tok_idx =
            if let Node::StringLiteral(path_tok_idx) = self.expect_string_literal()? {
                path_tok_idx
            } else {
                unreachable!()
            };

        self.forward_token();
        match self.current_token().ty {
            Type::KeywordAs => {
                self.forward_token();
                let as_tok_idx = if let Node::Ident(as_tok_idx) = self.expect_ident()? {
                    as_tok_idx
                } else {
                    unreachable!()
                };
                self.forward_token();
                if self.current_token().ty != Type::SemiColon {
                    return Err(self.err_uexpected(Type::SemiColon));
                }
                self.forward_token();
                return Ok(Node::Import(Import {
                    path: path_tok_idx,
                    _as: Some(as_tok_idx),
                }));
            }
            Type::SemiColon => {
                println!("inja");
                self.forward_token();
                return Ok(Node::Import(Import {
                    path: path_tok_idx,
                    _as: None,
                }));
            }
            _ => Err(self.err_uexpected(Type::SemiColon)),
        }
    }
    pub fn get_ast(&mut self) -> Result<AST> {
        let mut top_level = Vec::<Node>::new();
        println!("tokens: {:?}", self.tokens);
        loop {
            if self.cur >= self.tokens.len() {
                break;
            }
            match self.state {
                State::Start => match self.current_token().ty {
                    Type::KeywordImport => {
                        let import = self.expect_import()?;
                        top_level.push(import);
                    }
                    Type::KeywordConst => {}
                    Type::KeywordVar => {}
                    _ => {
                        unreachable!();
                    }
                },
                _ => {}
            }
        }

        Ok(AST { top_level })
    }
}

#[test]
fn import_no_as() -> Result<()> {
    let mut parser = Parser::new("import \"stdio.h\";")?;
    let ast = parser.get_ast()?;
    if let Node::Import(import) = &ast.top_level[0] {
        assert_eq!(import.path, 1);
        assert_eq!(import._as, None);
    } else {
        panic!()
    }

    Ok(())
}

#[test]
fn import_with_as() -> Result<()> {
    let mut parser = Parser::new("import \"stdio.h\" as std;")?;
    let ast = parser.get_ast()?;
    if let Node::Import(import) = &ast.top_level[0] {
        assert_eq!(import.path, 1);
        assert_eq!(import._as, Some(3));
    } else {
        panic!()
    }

    Ok(())
}

use crate::tokenizer::Token;
use crate::tokenizer::Tokenizer;
use crate::tokenizer::Type;
use anyhow::{anyhow, Result};

pub type TokenIndex = usize;

#[derive(Debug, PartialEq)]
pub struct Import {
    // this usize refer to src location.
    path: TokenIndex,
    _as: Option<TokenIndex>,
}

#[derive(Debug, PartialEq)]
pub struct Decl {
    mutable: bool,
    name: Box<Node>,
    ty: Box<Option<Node>>,
    expr: Box<Node>,
}

#[derive(Debug, PartialEq)]
pub enum Node {
    //top level items
    Import(Import),
    Decl(Decl),

    // primitive types
    Uint(TokenIndex),
    Int(TokenIndex),
    StringLiteral(TokenIndex),
    Float(TokenIndex),
    True(TokenIndex),
    False(TokenIndex),
    Char(TokenIndex),

    Ident(TokenIndex),

    // keywords
    IntTy(TokenIndex),
    FloatTy(TokenIndex),
    UintTy(TokenIndex),
    BoolTy(TokenIndex),
    StringTy(TokenIndex),
    CharTy(TokenIndex),
    VoidTy(TokenIndex),

    //Expressions
    Sum(Box<Node>, Box<Node>),
    Subtract(Box<Node>, Box<Node>),
    Multiply(Box<Node>, Box<Node>),
    Div(Box<Node>, Box<Node>),
    Mod(Box<Node>, Box<Node>),
    FieldAccess(Box<Node>, Box<Node>),

    FnCall(Vec<Node>),
}

#[derive(Debug)]
pub struct AST {
    tokens: Vec<Token>,
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
    fn backward_token(&mut self) {
        self.cur -= 1;
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
            tokens,
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
    fn expect_decl(&mut self) -> Result<Node> {
        let mut ty: Option<Node> = None;
        if self.current_token().ty != Type::Identifier {
            return Err(self.err_uexpected(Type::Identifier));
        }

        let name = self.cur;

        self.forward_token();

        match self.current_token().ty {
            Type::DoubleColon => {
                self.forward_token();
                let rhs = self.expect_expr()?;
                self.forward_token();
                Ok(Node::Decl(Decl {
                    mutable: false,
                    name: Box::new(Node::Ident(name)),
                    ty: Box::new(None),
                    expr: Box::new(rhs),
                }))
            }

            Type::Colon => {
                self.forward_token();
                ty = Some(self.expect_expr()?);
                if self.current_token().ty != Type::Equal && self.current_token().ty != Type::Equal {
                    unreachable!();
                }
                let mutable = self.current_token().ty == Type::Equal;
                self.forward_token();
                let rhs = self.expect_expr()?;
                self.forward_token();
                Ok(Node::Decl(Decl {
                    mutable,
                    name: Box::new(Node::Ident(name)),
                    ty: Box::new(ty),
                    expr: Box::new(rhs),
                }))
            }

            Type::ColonEqual => {
                self.forward_token();
                let rhs = self.expect_expr()?;
                self.forward_token();
                Ok(Node::Decl(Decl {
                    mutable: true,
                    name: Box::new(Node::Ident(name)),
                    ty: Box::new(None),
                    expr: Box::new(rhs),
                }))
            }
            _ => {
                unreachable!();
            }
        }




    }
    /*
     expr -> A (add_minus A)*
     A -> B (mul_div_mod B)*
     B -> C (< <= | >= > C)* // cmp
     C -> int | unsigned_int | float | string | bool | ident(expr,*) | ident | '(' expr ')' | IDENT.IDENT | struct_def | enum_def
     */

    fn expect_fn_call(&mut self) -> Result<Node> {
        if self.current_token().ty == Type::OpenParen {
            return Err(self.err_uexpected(Type::OpenParen));
        }
        let mut args = Vec::<Node>::new();

        self.forward_token();
        loop {
            let arg = self.expect_expr()?;
            args.push(arg);
            match self.current_token().ty {
                Type::Comma => {
                    self.forward_token();
                }

                Type::CloseParen => {
                    break;
                }

                _ => {
                    return Err(self.err_uexpected(Type::Comma));
                }
            }
            self.forward_token();
        }


        Ok(Node::FnCall(args))
    }
    fn expect_expr_C(&mut self) -> Result<Node> {
        println!("self.current_token.ty: {:?}", self.current_token().ty);
        match self.current_token().ty {
            Type::UnsignedInt => {
                self.forward_token();
                Ok(Node::Uint(self.cur-1))
            }
            Type::Float => {
                self.forward_token();
                Ok(Node::Float(self.cur-1))
            }
            Type::StringLiteral => {
                self.forward_token();
                Ok(Node::StringLiteral(self.cur-1))
            }
            Type::KeywordTrue => {
                self.forward_token();
                Ok(Node::True(self.cur-1))
            }
            Type::KeywordFalse => {
                self.forward_token();
                Ok(Node::False(self.cur-1))
            }
            Type::Char => {
                self.forward_token();
                Ok(Node::Char(self.cur-1))
            }
            Type::KeywordStruct => { unreachable!(); }
            Type::KeywordEnum => { unreachable!(); }
            Type::Identifier => {
                let lhs = self.current_token();
                self.forward_token();
                match self.current_token().ty {
                    Type::OpenParen => {
                        //function call
                        self.expect_fn_call()
                    }

                    Type::SemiColon => {
                        // simple ident
                        Ok(Node::Ident(self.cur - 1))
                    }

                    Type::Dot => {
                        // field access
                        self.forward_token();
                        Ok(Node::FieldAccess(Box::new(Node::Ident(self.cur - 2)), Box::new(Node::Ident(self.cur))))
                    }

                    _ => {
                        unreachable!()
                    }
                }
            }

            _ => {
                unreachable!();
            }
        }
    }

    fn expect_expr_B(&mut self) -> Result<Node> {
        let lhs = self.expect_expr_C()?;

        match self.current_token().ty {
            Type::LeftAngle
            | Type::RightAngle
            | Type::LessEqual
            | Type::GreaterEqual
            | Type::DoubleEqual => {
                // handle !=
                let op = self.current_token();
                self.forward_token();
                let rhs = self.expect_expr_C()?;

                Ok(Node::FieldAccess(Box::new(lhs), Box::new(rhs)))
            }

            _ => Ok(lhs),
        }
    }
    fn expect_expr_A(&mut self) -> Result<Node> {
        let lhs = self.expect_expr_B()?;

        match self.current_token().ty {
            Type::Asterix | Type::Percent | Type::ForwardSlash => {
                let op = self.current_token();
                self.forward_token();
                let rhs = self.expect_expr_B()?;

                Ok(Node::Subtract(Box::new(lhs), Box::new(rhs)))
            }

            _ => Ok(lhs),
        }
    }

    // fn expect_field_access(&mut self) -> Result<Node> {}
    // fn expect_cmp(&mut self) -> Result<Node> {}
    // fn expect_vals(&mut self) -> Result<Node> {}

    fn expect_expr(&mut self) -> Result<Node> {
        let lhs = self.expect_expr_A()?;

        match self.current_token().ty {
            Type::Plus | Type::Minus => {
                let op = self.current_token();
                self.forward_token();
                let rhs = self.expect_expr_A()?;

                Ok(Node::Sum(Box::new(lhs), Box::new(rhs)))
            }

            _ => Ok(lhs),
        }
    }

    // statement is either a decl/if/for/return/switch/break/continue/goto
    fn expect_stmt(&mut self) -> Result<Node> {
        Ok(Node::True(1))
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
    pub fn get_ast(mut self) -> Result<AST> {
        let mut top_level = Vec::<Node>::new();
        println!("tokens: {:?}", self.tokens);
        loop {
            if self.cur >= self.tokens.len() {
                break;
            }
            match self.state {
                State::Start => {
                    println!("in start current token ty: {:?}", self.current_token().ty);
                    match self.current_token().ty {
                        Type::KeywordImport => {
                            let import = self.expect_import()?;
                            top_level.push(import);
                        }
                        Type::Identifier => {
                            top_level.push(self.expect_decl()?);
                        }
                        _ => {
                            unreachable!();
                        }
                    }
                },
                _ => {}
            }
        }

        Ok(AST {
            tokens: self.tokens,
            top_level,
        })
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

#[test]
fn expr_uint() -> Result<()> {
    let mut parser = Parser::new("a :: 4;")?;
    let ast = parser.get_ast()?;
    println!("here: {:?}", ast.top_level[0]);

    if let Node::Decl(decl) = &ast.top_level[0] {
        assert_eq!(decl.mutable, false);
        assert_eq!(decl.name, Box::new(Node::Ident(0)));
        assert_eq!(decl.expr, Box::new(Node::Uint(2)));
    } else {
        panic!()
    }

    Ok(())
}

#[test]
fn expr_bool_true() -> Result<()> {
    let mut parser = Parser::new("true")?;
    let node = parser.expect_expr()?;
    if let Node::True(idx) = node {
        assert_eq!(idx, 0);
    } else {
        panic!()
    }

    Ok(())
}

#[test]
fn expr_bool_false() -> Result<()> {
    let mut parser = Parser::new("false")?;
    let node = parser.expect_expr()?;
    if let Node::False(idx) = node {
        assert_eq!(idx, 0);
    } else {
        panic!()
    }

    Ok(())
}

// #[test] //TODO handle chars in tokenizer first
// fn expr_char() -> Result<()> {
//     let mut parser = Parser::new("'a'")?;
//     let node = parser.expect_expr()?;
//     if let Node::Char(idx) = node {
//         assert_eq!(idx, 0);
//     } else {
//         panic!()
//     }
//
//     Ok(())
// }

// #[test] // TODO expressions
// fn const_decl_uint() -> Result<()> {
//     let mut parser = Parser::new("i :: 8;")?;
//     let ast = parser.get_ast()?;
//     if let Node::Import(import) = &ast.top_level[0] {
//         assert_eq!(import.path, 1);
//         assert_eq!(import._as, Some(3));
//     } else {
//         panic!()
//     }

//     Ok(())
// }

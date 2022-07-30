use std::ops::Deref;

use crate::tokenizer::Token;
use crate::tokenizer::Tokenizer;
use crate::tokenizer::Type;
use anyhow::{anyhow, Result};

pub type TokenIndex = usize;

#[derive(Debug, PartialEq, Clone)]
pub struct Import {
    // this usize refer to src location.
    pub path: TokenIndex,
    pub _as: Option<TokenIndex>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Decl {
    pub mutable: bool,
    pub name: Box<Node>,
    pub ty: Box<Option<Node>>,
    pub expr: Box<Node>,
}

#[derive(Debug, PartialEq, Clone)]
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
    Int8Ty(TokenIndex),
    Int16Ty(TokenIndex),
    Int32Ty(TokenIndex),
    Int64Ty(TokenIndex),
    Int128Ty(TokenIndex),

    UintTy(TokenIndex),
    Uint8Ty(TokenIndex),
    Uint16Ty(TokenIndex),
    Uint32Ty(TokenIndex),
    Uint64Ty(TokenIndex),
    Uint128Ty(TokenIndex),

    FloatTy(TokenIndex),
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
    FnDef(Vec<(Node, Node)>, Box<Node>, Vec<Node>),

    FnCall(Box<Node>, Vec<Node>),
    If(Box<Node>, Box<Vec<Node>>, Option<Vec<Node>>),
    Return(Box<Node>),
    Struct(Vec<(Node, Node)>),
}

#[derive(Debug)]
pub struct AST {
    src: String,
    tokens: Vec<Token>,
    pub top_level: Vec<Node>,
}

impl AST {
    pub fn get_src_for_token(&self, tok_idx: usize) -> Result<&str> {
        let src_range = &self.tokens[tok_idx];
        Ok(&self.src[src_range.loc.0..=src_range.loc.1])
    }
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
pub struct Parser {
    src: String,
    tokens: Vec<Token>,
    cur: usize,
    state: State,
}
// Every parser function should parse until the last token in it's scope and then move cursor to the next token. so every parse function moves the cursor to the next.
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
    pub fn new_with_tokens(src: String, tokens: Vec<Token>) -> Result<Self> {
        Ok(Self {
            src,
            tokens,
            state: State::Start,
            cur: 0,
        })
    }
    fn new(src: &'static str) -> Result<Self> {
        let mut tokenizer = Tokenizer::new(src);
        let mut tokens = Vec::<Token>::new();
        loop {
            let tok = tokenizer.next()?;
            // println!("got token: {:?}", tok);
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
            src: src.to_string(),
            tokens,
            state: State::Start,
            cur: 0,
        })
    }

    fn expect_ident(&mut self) -> Result<Node> {
        match self.current_token().ty {
            Type::Identifier => {
                self.forward_token();
                return Ok(Node::Ident(self.cur - 1));
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
                self.expect_tok(Type::SemiColon)?;
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
                if self.current_token().ty != Type::Equal && self.current_token().ty != Type::Colon
                {
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
    C -> int | unsigned_int | float | string | bool | ident(expr,*) | ident | '(' expr ')' | field_access | struct_def | enum_def | struct_init | enum_init | fn_def | types(int, uint, void, string, bool, char, float
    */

    fn expect_fn_def(&mut self) -> Result<Node> {
        self.expect_tok(Type::KeywordFn)?;
        self.forward_token();
        self.expect_tok(Type::OpenParen)?;
        let mut args: Vec<(Node, Node)> = vec![];
        self.forward_token();
        loop {
            if self.current_token().ty == Type::CloseParen {
                self.forward_token();
                break;
            }
            let name = self.expect_ident()?;
            self.expect_tok(Type::Colon)?;
            let ty = self.expect_expr()?;
            args.push((name, ty));
        }

        let ret_ty = self.expect_expr()?;

        self.expect_tok(Type::OpenBrace)?;

        let body = self.expect_block()?;

        Ok(Node::FnDef(args, Box::new(ret_ty), body))
    }

    fn expect_fn_call(&mut self) -> Result<Node> {
        let name = Node::Ident(self.cur);
        self.forward_token();
        self.expect_tok(Type::OpenParen)?;
        let mut args = Vec::<Node>::new();

        self.forward_token();
        loop {
            if self.current_token().ty == Type::CloseParen {
                self.forward_token();
                break;
            }
            let arg = self.expect_expr()?;
            args.push(arg);
            match self.current_token().ty {
                Type::Comma => {
                    self.forward_token();
                }

                Type::CloseParen => {
                    self.forward_token();
                    break;
                }

                _ => {
                    return Err(self.err_uexpected(Type::Comma));
                }
            }
        }
        self.expect_tok(Type::SemiColon)?;
        self.forward_token();
        Ok(Node::FnCall(Box::new(name), args))
    }
    fn expect_expr_C(&mut self) -> Result<Node> {
        match self.current_token().ty {
            Type::UnsignedInt => {
                self.forward_token();
                Ok(Node::Uint(self.cur - 1))
            }
            Type::Float => {
                self.forward_token();
                Ok(Node::Float(self.cur - 1))
            }
            Type::StringLiteral => {
                self.forward_token();
                Ok(Node::StringLiteral(self.cur - 1))
            }
            Type::KeywordTrue => {
                self.forward_token();
                Ok(Node::True(self.cur - 1))
            }
            Type::KeywordFalse => {
                self.forward_token();
                Ok(Node::False(self.cur - 1))
            }
            Type::Char => {
                self.forward_token();
                Ok(Node::Char(self.cur - 1))
            }
            Type::KeywordStruct => {
                self.forward_token();
                self.expect_tok(Type::OpenBrace)?;
                self.forward_token();
                let mut fields = Vec::<(Node, Node)>::new();
                loop {
                    if self.current_token().ty == Type::CloseBrace {
                        self.forward_token();
                        break;
                    }
                    let name = self.expect_ident()?;

                    self.expect_tok(Type::Colon)?;
                    self.forward_token();
                    let ty = self.expect_expr()?;
                    fields.push((name, ty));
                    println!("self.current_token : {:?}", self.current_token());
                    match self.current_token().ty {
                        Type::Comma => {
                            self.forward_token();
                            continue;
                        }
                        Type::CloseBrace => {
                            self.forward_token();
                            break;
                        }
                        _ => return Err(self.err_uexpected(Type::Comma)),
                    }
                }

                Ok(Node::Struct(fields))
            }
            Type::KeywordEnum => {
                // TODO
                unreachable!();
            }
            Type::OpenParen => {
                self.forward_token();
                let expr = self.expect_expr()?;
                self.forward_token();
                if self.current_token().ty != Type::CloseParen {
                    return Err(self.err_uexpected(Type::CloseParen));
                }

                Ok(expr)
            }
            Type::Identifier => {
                self.forward_token();
                match self.current_token().ty {
                    Type::OpenParen => {
                        //function call
                        self.backward_token();
                        self.expect_fn_call()
                    }

                    Type::SemiColon => {
                        // simple ident
                        Ok(Node::Ident(self.cur - 1))
                    }

                    Type::Dot => {
                        // field access
                        let container = self.cur - 1;
                        self.forward_token();
                        let field = self.cur;
                        self.forward_token();
                        Ok(Node::FieldAccess(
                            Box::new(Node::Ident(container)),
                            Box::new(Node::Ident(field)),
                        ))
                    }

                    _ => Ok(Node::Ident(self.cur - 1)),
                }
            }
            Type::KeywordVoid => {
                self.forward_token();
                Ok(Node::VoidTy(self.cur - 1))
            }
            Type::KeywordInt => {
                self.forward_token();
                Ok(Node::IntTy(self.cur - 1))
            }
            Type::KeywordInt8 => {
                self.forward_token();
                Ok(Node::Int8Ty(self.cur - 1))
            }
            Type::KeywordInt16 => {
                self.forward_token();
                Ok(Node::Int16Ty(self.cur - 1))
            }
            Type::KeywordInt32 => {
                self.forward_token();
                Ok(Node::Int32Ty(self.cur - 1))
            }
            Type::KeywordInt64 => {
                self.forward_token();
                Ok(Node::Int64Ty(self.cur - 1))
            }
            Type::KeywordInt128 => {
                self.forward_token();
                Ok(Node::Int128Ty(self.cur - 1))
            }
            Type::KeywordUint => {
                self.forward_token();
                Ok(Node::Uint(self.cur - 1))
            }
            Type::KeywordUint8 => {
                self.forward_token();
                Ok(Node::Uint8Ty(self.cur - 1))
            }
            Type::KeywordUint16 => {
                self.forward_token();
                Ok(Node::Uint16Ty(self.cur - 1))
            }
            Type::KeywordUint32 => {
                self.forward_token();
                Ok(Node::Uint32Ty(self.cur - 1))
            }
            Type::KeywordUint64 => {
                self.forward_token();
                Ok(Node::Uint64Ty(self.cur - 1))
            }
            Type::KeywordUint128 => {
                self.forward_token();
                Ok(Node::Uint128Ty(self.cur - 1))
            }
            Type::KeywordFloat => {
                self.forward_token();
                Ok(Node::FloatTy(self.cur - 1))
            }
            Type::KeywordChar => {
                self.forward_token();
                Ok(Node::CharTy(self.cur - 1))
            }
            Type::KeywordBool => {
                self.forward_token();
                Ok(Node::BoolTy(self.cur - 1))
            }

            Type::KeywordFn => self.expect_fn_def(),

            _ => {
                println!(">>>{:?}", self.current_token());
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
                //TODO
                // handle !=
                let op = self.current_token();
                self.forward_token();
                let rhs = self.expect_expr_C()?;

                Ok(Node::FieldAccess(Box::new(lhs), Box::new(rhs)))
            }

            _ => Ok(lhs),
        }
    }
    fn expect_expr_mul_div_mod(&mut self) -> Result<Node> {
        let lhs = self.expect_expr_B()?;

        match self.current_token().ty {
            //TODO
            Type::Asterix | Type::Percent | Type::ForwardSlash => {
                let op = self.current_token();
                self.forward_token();
                let rhs = self.expect_expr_B()?;

                Ok(Node::Subtract(Box::new(lhs), Box::new(rhs)))
            }

            _ => Ok(lhs),
        }
    }

    fn expect_expr(&mut self) -> Result<Node> {
        let lhs = self.expect_expr_mul_div_mod()?;

        match self.current_token().ty {
            Type::Plus | Type::Minus => {
                let op = self.current_token();
                self.forward_token();
                let rhs = self.expect_expr_mul_div_mod()?;

                Ok(Node::Sum(Box::new(lhs), Box::new(rhs)))
            }

            _ => Ok(lhs),
        }
    }

    fn expect_tok(&mut self, ty: Type) -> Result<()> {
        if self.current_token().ty != ty {
            return Err(self.err_uexpected(ty));
        }

        return Ok(());
    }

    fn expect_block(&mut self) -> Result<Vec<Node>> {
        self.expect_tok(Type::OpenBrace)?;
        self.forward_token();
        let mut stmts = Vec::<Node>::new();
        loop {
            if self.current_token().ty == Type::CloseBrace {
                break;
            }
            let stmt = self.expect_stmt()?;
            stmts.push(stmt);
        }
        self.forward_token();

        Ok(stmts)
    }

    // statement is either a decl/if/for/return/switch/break/continue/goto/fn_call
    fn expect_stmt(&mut self) -> Result<Node> {
        match self.current_token().ty {
            Type::Identifier => {
                let next_tok = self.cur + 1;
                match self.tokens[next_tok].ty {
                    Type::DoubleColon | Type::Colon | Type::Equal => self.expect_decl(),
                    Type::Dot | Type::OpenParen => self.expect_fn_call(),
                    _ => Err(self.err_uexpected(Type::OpenParen)),
                }
            }

            Type::KeywordIf => {
                self.forward_token();
                let cond = self.expect_expr()?;
                self.expect_tok(Type::OpenBrace)?;
                let then = self.expect_block()?;
                match self.current_token().ty {
                    Type::KeywordElse => {
                        self.forward_token();
                        let _else = self.expect_block()?;
                        return Ok(Node::If(Box::new(cond), Box::new(then), Some(_else)));
                    }
                    _ => {
                        return Ok(Node::If(Box::new(cond), Box::new(then), None));
                    }
                }
            }

            Type::KeywordFor => {
                //TODO

                unreachable!();
            }
            Type::KeywordReturn => {
                self.forward_token();
                let expr = self.expect_expr()?;
                self.expect_tok(Type::SemiColon)?;
                self.forward_token();
                Ok(Node::Return(Box::new(expr)))
            }

            Type::KeywordGoto => {
                //TODO

                unreachable!();
            }

            Type::KeywordContinue => {
                //TODO

                unreachable!();
            }

            Type::KeywordSwitch => {
                //TODO

                unreachable!();
            }

            Type::KeywordBreak => {
                //TODO

                unreachable!();
            }

            _ => {
                println!("expecting stmt: {:?}", self.current_token());
                unreachable!();
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
                self.expect_tok(Type::SemiColon)?;
                self.forward_token();
                return Ok(Node::Import(Import {
                    path: path_tok_idx,
                    _as: Some(as_tok_idx),
                }));
            }
            Type::SemiColon => {
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
                    Type::Identifier => {
                        top_level.push(self.expect_decl()?);
                    }
                    _ => {
                        unreachable!();
                    }
                },
                _ => {}
            }
        }

        Ok(AST {
            src: self.src,
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
fn const_decl_expr_uint() -> Result<()> {
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
fn const_decl_expr_bool_true() -> Result<()> {
    let mut parser = Parser::new("a :: true;")?;
    let ast = parser.get_ast()?;
    println!("here: {:?}", ast.top_level[0]);

    if let Node::Decl(decl) = &ast.top_level[0] {
        assert_eq!(decl.mutable, false);
        assert_eq!(decl.name, Box::new(Node::Ident(0)));
        assert_eq!(decl.expr, Box::new(Node::True(2)));
    } else {
        panic!()
    }

    Ok(())
}

#[test]
fn const_decl_expr_bool_false() -> Result<()> {
    let mut parser = Parser::new("a :: false;")?;
    let ast = parser.get_ast()?;
    println!("here: {:?}", ast.top_level[0]);

    if let Node::Decl(decl) = &ast.top_level[0] {
        assert_eq!(decl.mutable, false);
        assert_eq!(decl.name, Box::new(Node::Ident(0)));
        assert_eq!(decl.expr, Box::new(Node::False(2)));
    } else {
        panic!()
    }

    Ok(())
}

#[test]
fn const_decl_expr_ident() -> Result<()> {
    let mut parser = Parser::new("a :: b;")?;
    let ast = parser.get_ast()?;

    if let Node::Decl(decl) = &ast.top_level[0] {
        assert_eq!(decl.mutable, false);
        assert_eq!(decl.name, Box::new(Node::Ident(0)));
        assert_eq!(decl.expr, Box::new(Node::Ident(2)));
    } else {
        panic!()
    }

    Ok(())
}

#[test]
fn const_decl_expr_field_access() -> Result<()> {
    let mut parser = Parser::new("a :: b.c;")?;
    let ast = parser.get_ast()?;

    if let Node::Decl(decl) = &ast.top_level[0] {
        assert_eq!(decl.mutable, false);
        assert_eq!(decl.name, Box::new(Node::Ident(0)));
        assert_eq!(
            decl.expr,
            Box::new(Node::FieldAccess(
                Box::new(Node::Ident(2)),
                Box::new(Node::Ident(4))
            ))
        );
    } else {
        panic!()
    }

    Ok(())
}

#[test]
fn const_decl_expr_char() -> Result<()> {
    let mut parser = Parser::new("a :: 'a';")?;
    let ast = parser.get_ast()?;

    if let Node::Decl(decl) = &ast.top_level[0] {
        println!("char node: {:?}", decl.expr);
        assert_eq!(decl.mutable, false);
        assert_eq!(decl.name, Box::new(Node::Ident(0)));
        assert_eq!(decl.expr, Box::new(Node::Char(2)));
    } else {
        panic!()
    }

    Ok(())
}

#[test]
fn const_decl_expr_fn_def() -> Result<()> {
    let mut parser = Parser::new("main :: fn() void {\n\tprintf(\"Hello World\");};")?;
    let ast = parser.get_ast()?;

    if let Node::Decl(decl) = &ast.top_level[0] {
        assert_eq!(decl.mutable, false);
        assert_eq!(decl.name, Box::new(Node::Ident(0)));
        assert_eq!(
            decl.expr,
            Box::new(Node::FnDef(
                Vec::new(),
                Box::new(Node::VoidTy(5)),
                vec![Node::FnCall(
                    Box::new(Node::Ident(7)),
                    vec![Node::StringLiteral(9)]
                )]
            ))
        );
    } else {
        panic!()
    }

    Ok(())
}

#[test]
fn const_decl_expr_string() -> Result<()> {
    let mut parser = Parser::new("a :: \"Amirreza\";")?;
    let ast = parser.get_ast()?;

    if let Node::Decl(decl) = &ast.top_level[0] {
        assert_eq!(decl.mutable, false);
        assert_eq!(decl.name, Box::new(Node::Ident(0)));
        assert_eq!(decl.expr, Box::new(Node::StringLiteral(2)));
    } else {
        panic!()
    }

    Ok(())
}

#[test]
fn const_decl_expr_struct() -> Result<()> {
    let mut parser = Parser::new("a :: struct { i: int, f: float };")?;
    let ast = parser.get_ast()?;

    if let Node::Decl(decl) = &ast.top_level[0] {
        assert_eq!(decl.mutable, false);
        assert_eq!(decl.name, Box::new(Node::Ident(0)));
        assert_eq!(
            decl.expr,
            Box::new(Node::Struct(vec![
                (Node::Ident(4), Node::IntTy(6)),
                (Node::Ident(8), Node::FloatTy(10))
            ]))
        );
    } else {
        panic!()
    }

    Ok(())
}

#[test]
fn const_decl_expr_float() -> Result<()> {
    let mut parser = Parser::new("a :: 2.2;")?;
    let ast = parser.get_ast()?;

    if let Node::Decl(decl) = &ast.top_level[0] {
        assert_eq!(decl.mutable, false);
        assert_eq!(decl.name, Box::new(Node::Ident(0)));
        assert_eq!(decl.expr, Box::new(Node::Float(2)));
    } else {
        panic!()
    }

    Ok(())
}

#[test]
fn const_decl_fn_with_if() -> Result<()> {
    let mut parser = Parser::new(
        "main :: fn() int {
if true {
\t\tprintf(\"Hello\");
}
\treturn 0;
};",
    )?;
    let ast = parser.get_ast()?;

    if let Node::Decl(decl) = &ast.top_level[0] {
    } else {
        panic!()
    }
    Ok(())
}

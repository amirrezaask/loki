use std::ops::Deref;

use crate::tokenizer::Token;
use crate::ast::{AST, Node, Decl, NodeData, NodeID};
use crate::tokenizer::Tokenizer;
use crate::tokenizer::Type;
use anyhow::{anyhow, Result};

#[derive(Debug, Default)]
pub struct Parser {
    src: String,
    tokens: Vec<Token>,
    cur: usize,
    node_counter: NodeID,
}
// Every parser function should parse until the last token in it's scope and then move cursor to the next token. so every parse function moves the cursor to the next.
impl Parser {
    fn err_uexpected(&self, what: Type) -> anyhow::Error {
        anyhow!(
            "Expected {:?}, found {:?} at \"{}\"",
            what,
            self.current_token().ty,
            self.src[self.current_token().loc.0..=self.current_token().loc.1].to_string(),
        )
    }
    fn new_node(&mut self, data: NodeData) -> Node {
        self.node_counter+=1;
        Node {
            id: self.node_counter,
            data,
        }
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
            cur: 0,
            node_counter: 0,
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
            cur: 0,
            node_counter: 0,
        })
    }

    fn expect_ident(&mut self) -> Result<Node> {
        match self.current_token().ty {
            Type::Ident => {
                self.forward_token();
                return Ok(self.new_node(NodeData::Ident(self.cur-1)));
            }
            _ => {
                return Err(self.err_uexpected(Type::Ident));
            }
        }
    }
    fn expect_string_literal(&mut self) -> Result<Node> {
        match self.current_token().ty {
            Type::StringLiteral => {
                return Ok(self.new_node(NodeData::StringLiteral(self.cur)));
            }
            _ => {
                return Err(self.err_uexpected(Type::StringLiteral));
            }
        }
    }

    fn naive_ty_infer(&mut self, expr: &Node) -> Option<Node> {
        match &expr.data {
            NodeData::True(_) | NodeData::False(_) => {
                return Some(self.new_node(NodeData::BoolTy(0)));
            }
            NodeData::Char(_) => {
                return Some(self.new_node(NodeData::CharTy(0)));
            }
            NodeData::Uint(_) => {
                return Some(self.new_node(NodeData::UintTy(0)));
            }
            NodeData::Int(_) => {
                return Some(self.new_node(NodeData::IntTy(0)));
            }
            NodeData::Float(_) => {
                return Some(self.new_node(NodeData::FloatTy(0)));
            }
            
            NodeData::StringLiteral(_) => {
                return Some(self.new_node(NodeData::StringTy(0)));
            }
            
            NodeData::Initialize(op_ty, _) => {
                if op_ty.is_some() {
                    
                    let type_name = if let NodeData::Ident(ident_tok_idx) = op_ty.clone().unwrap().data {
                        let loc = self.tokens[ident_tok_idx].loc;
                        self.src[loc.0..=loc.1].to_string()
                    } else {
                        unreachable!()
                    };
                    return Some(self.new_node(NodeData::TEXT(type_name)));
                } else {
                    return None;
                }
            }

            _ => {
                return None;
            }

        }

    }
    fn expect_decl(&mut self) -> Result<Node> {
        self.expect_token(Type::Ident)?;

        let name = self.cur;

        self.forward_token();

        match self.current_token().ty {
            Type::DoubleColon => {
                self.forward_token();
                let rhs = self.expect_expr()?;
                self.expect_token(Type::SemiColon)?;
                self.forward_token();
                let name = self.new_node(NodeData::Ident(name));
                let ty_infer = self.naive_ty_infer(&rhs);
                Ok(self.new_node(NodeData::Decl(Decl {
                    mutable: false,
                    name: Box::new(name),
                    ty: Box::new(ty_infer),
                    expr: Box::new(rhs),
                })))
            }

            Type::Colon => {
                self.forward_token();
                let ty = Some(self.expect_expr()?);
                if self.current_token().ty != Type::Equal && self.current_token().ty != Type::Colon
                {
                    unreachable!();
                }
                let mutable = self.current_token().ty == Type::Equal;
                self.forward_token();
                let rhs = self.expect_expr()?;
                self.forward_token();
                let name = self.new_node(NodeData::Ident(name));
                Ok(self.new_node(NodeData::Decl(Decl {
                    mutable,
                    name: Box::new(name),
                    ty: Box::new(ty),
                    expr: Box::new(rhs),
                })))
            }

            Type::ColonEqual => {
                self.forward_token();
                let rhs = self.expect_expr()?;
                self.expect_token(Type::SemiColon)?;
                self.forward_token();
                let name = self.new_node(NodeData::Ident(name));
                let ty_infer = self.naive_ty_infer(&rhs);
                Ok(self.new_node(NodeData::Decl(Decl {
                    mutable: true,
                    name: Box::new(name),
                    ty: Box::new(ty_infer),
                    expr: Box::new(rhs),
                })))

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
    C -> int | unsigned_int | float | string | bool | ident(expr,*) | ident | '(' expr ')' | deref: *expr | ref: &expr
    | field_access | struct_def | enum_def | struct_init | enum_init | fn_def | types(int, uint, void, string, bool, char, float
    */

    fn expect_fn_def(&mut self) -> Result<Node> {
        self.expect_token(Type::KeywordFn)?;
        self.forward_token();
        self.expect_token(Type::OpenParen)?;
        let mut args: Vec<(Node, Node)> = vec![];
        self.forward_token();
        loop {
            if self.current_token().ty == Type::CloseParen {
                self.forward_token();
                break;
            }
            let name = self.expect_ident()?;
            self.expect_token(Type::Colon)?;
            let ty = self.expect_expr()?;
            args.push((name, ty));
        }

        let ret_ty = self.expect_expr()?;
        self.expect_token(Type::OpenBrace)?;

        let body = self.expect_block()?;

        Ok(self.new_node(NodeData::FnDef(args, Box::new(ret_ty), body)))
    }

    fn expect_fn_call(&mut self) -> Result<Node> {
        let name = self.new_node(NodeData::Ident(self.cur));
        self.forward_token();
        self.expect_token(Type::OpenParen)?;
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
        self.expect_token(Type::SemiColon)?;
        self.forward_token();
        Ok(self.new_node(NodeData::FnCall(Box::new(name), args)))
    }
    fn expect_expr_C(&mut self) -> Result<Node> {
        match self.current_token().ty {
            Type::UnsignedInt => {
                self.forward_token();
                Ok(self.new_node(NodeData::Uint(self.cur - 1)))
            }
            Type::Float => {
                self.forward_token();
                Ok(self.new_node(NodeData::Float(self.cur - 1)))
            }
            Type::StringLiteral => {
                self.forward_token();
                Ok(self.new_node(NodeData::StringLiteral(self.cur - 1)))
            }
            Type::KeywordTrue => {
                self.forward_token();
                Ok(self.new_node(NodeData::True(self.cur - 1)))
            }
            Type::KeywordFalse => {
                self.forward_token();
                Ok(self.new_node(NodeData::False(self.cur - 1)))
            }
            Type::Char => {
                self.forward_token();
                Ok(self.new_node(NodeData::Char(self.cur - 1)))
            }
            Type::KeywordStruct => {
                self.forward_token();
                self.expect_token(Type::OpenBrace)?;
                self.forward_token();
                let mut fields = Vec::<(Node, Node)>::new();
                loop {
                    if self.current_token().ty == Type::CloseBrace {
                        self.forward_token();
                        break;
                    }
                    let name = self.expect_ident()?;

                    self.expect_token(Type::Colon)?;
                    self.forward_token();
                    let ty = self.expect_expr()?;
                    fields.push((name, ty));
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

                Ok(self.new_node(NodeData::Struct(fields)))
            }
            Type::KeywordEnum => {
                self.forward_token();
                self.expect_token(Type::OpenBrace)?;
                self.forward_token();
                let mut variants = Vec::<(Node, Option<Node>)>::new();
                let mut is_union = false;
                loop {
                    if self.current_token().ty == Type::CloseBrace {
                        self.forward_token();
                        break;
                    }

                    if self.current_token().ty == Type::Comma {
                        self.forward_token();
                        continue;
                    }

                    let name = self.expect_ident()?;
                    match self.current_token().ty {
                        Type::OpenParen => {
                            // should be a union
                            is_union = true;
                            self.forward_token();
                            let field_ty = self.expect_expr()?;

                            self.expect_token(Type::CloseParen)?;
                            self.forward_token();
                            variants.push((name, Some(field_ty)));
                        }
                        Type::Comma => {
                            variants.push((name, None));
                            self.forward_token();
                            continue;
                        }
                        Type::CloseBrace => {
                            variants.push((name, None));
                            self.forward_token();
                            break;
                        }
                        _ => {
                            self.expect_token(Type::Comma)?;
                        }
                    }
                }

                Ok(self.new_node(NodeData::Enum(is_union, variants)))
            }
            Type::OpenParen => {
                self.forward_token();
                let expr = self.expect_expr()?;
                self.expect_token(Type::CloseParen)?;
                Ok(expr)
            }
            Type::Asterix => {
                self.forward_token();
                let expr = self.expect_expr()?;
                return Ok(self.new_node(NodeData::Deref(Box::new(expr))));
            }
            Type::Ampersand => {
                self.forward_token();
                let expr = self.expect_expr()?;
                return Ok(self.new_node(NodeData::Ref(Box::new(expr))));
            }
            Type::OpenBrace => {
                self.forward_token();
                let mut fields = Vec::<(Node, Node)>::new();
                loop {
                    if self.current_token().ty == Type::CloseBrace {
                        self.forward_token();
                        break;
                    }
                    let name = self.expect_ident()?;
                    self.expect_token(Type::Equal)?;
                    self.forward_token();
                    let value = self.expect_expr()?;
                    fields.push((name, value));
                    match self.current_token().ty {
                        Type::Comma => {
                            self.forward_token();
                        }
                        Type::CloseBrace => {
                            self.forward_token();
                            break;
                        }
                        _ => return Err(self.err_uexpected(Type::Comma)),
                    }
                }

                return Ok(self.new_node(NodeData::Initialize(None, fields)));
            }
            Type::Ident => {
                self.forward_token();
                match self.current_token().ty {
                    Type::OpenParen => {
                        //function call
                        self.backward_token();
                        self.expect_fn_call()
                    }

                    Type::SemiColon => {
                        // simple ident
                        Ok(self.new_node(NodeData::Ident(self.cur - 1)))
                    }

                    Type::Dot => {
                        // field access
                        self.backward_token();
                        let mut access = Vec::<Node>::new();
                        loop {
                            let f = self.expect_ident()?;
                            access.push(f);
                            match self.current_token().ty {
                                Type::Dot => {
                                    self.forward_token();
                                }
                                _ => {
                                    break;
                                }
                            }
                        }

                        Ok(self.new_node(NodeData::FieldAccess(access)))
                    }

                    Type::OpenBrace => {
                        self.backward_token();
                        let name = self.expect_ident()?;
                        self.expect_token(Type::OpenBrace)?;
                        self.forward_token();
                        let mut fields = Vec::<(Node, Node)>::new();
                        loop {
                            if self.current_token().ty == Type::CloseBrace {
                                self.forward_token();
                                break;
                            }

                            let name = self.expect_ident()?;
                            self.expect_token(Type::Equal)?;
                            self.forward_token();
                            let value = self.expect_expr()?;
                            fields.push((name, value));
                            match self.current_token().ty {
                                Type::Comma => {
                                    self.forward_token();
                                }
                                Type::CloseBrace => {
                                    self.forward_token();
                                    break;
                                }
                                _ => return Err(self.err_uexpected(Type::Comma)),
                            }
                        }
                        return Ok(self.new_node(NodeData::Initialize(Some(Box::new(name)), fields)));
                    }

                    _ => Ok(self.new_node(NodeData::Ident(self.cur - 1))),
                }
            }
            Type::KeywordVoid => {
                self.forward_token();
                Ok(self.new_node(NodeData::VoidTy(self.cur - 1)))
            }
            Type::KeywordInt => {
                self.forward_token();
                Ok(self.new_node(NodeData::IntTy(self.cur - 1)))
            }
            Type::KeywordInt8 => {
                self.forward_token();
                Ok(self.new_node(NodeData::Int8Ty(self.cur - 1)))
            }
            Type::KeywordInt16 => {
                self.forward_token();
                Ok(self.new_node(NodeData::Int16Ty(self.cur - 1)))
            }
            Type::KeywordInt32 => {
                self.forward_token();
                Ok(self.new_node(NodeData::Int32Ty(self.cur - 1)))
            }
            Type::KeywordInt64 => {
                self.forward_token();
                Ok(self.new_node(NodeData::Int64Ty(self.cur - 1)))
            }
            Type::KeywordInt128 => {
                self.forward_token();
                Ok(self.new_node(NodeData::Int128Ty(self.cur - 1)))
            }
            Type::KeywordUint => {
                self.forward_token();
                Ok(self.new_node(NodeData::Uint(self.cur - 1)))
            }
            Type::KeywordUint8 => {
                self.forward_token();
                Ok(self.new_node(NodeData::Uint8Ty(self.cur - 1)))
            }
            Type::KeywordUint16 => {
                self.forward_token();
                Ok(self.new_node(NodeData::Uint16Ty(self.cur - 1)))
            }
            Type::KeywordUint32 => {
                self.forward_token();
                Ok(self.new_node(NodeData::Uint32Ty(self.cur - 1)))
            }
            Type::KeywordUint64 => {
                self.forward_token();
                Ok(self.new_node(NodeData::Uint64Ty(self.cur - 1)))
            }
            Type::KeywordUint128 => {
                self.forward_token();
                Ok(self.new_node(NodeData::Uint128Ty(self.cur - 1)))
            }
            Type::KeywordFloat => {
                self.forward_token();
                Ok(self.new_node(NodeData::FloatTy(self.cur - 1)))
            }
            Type::KeywordChar => {
                self.forward_token();
                Ok(self.new_node(NodeData::CharTy(self.cur - 1)))
            }
            Type::KeywordBool => {
                self.forward_token();
                Ok(self.new_node(NodeData::BoolTy(self.cur - 1)))
            }

            Type::KeywordString => {
                self.forward_token();
                Ok(self.new_node(NodeData::StringTy(self.cur - 1)))
            }

            Type::KeywordFn => self.expect_fn_def(),

            _ => {
                println!("unknown expr: {:?}", self.current_token());
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
            | Type::DoubleEqual
            | Type::NotEqual => {
                //TODO
                // handle !=
                let op = self.current_token();
                self.forward_token();
                let rhs = self.expect_expr_C()?;

                Ok(self.new_node(NodeData::Cmp(
                    self.current_token().ty.clone(),
                    Box::new(lhs),
                    Box::new(rhs),
                )))
            }

            _ => Ok(lhs),
        }
    }
    fn expect_expr_mul_div_mod(&mut self) -> Result<Node> {
        let lhs = self.expect_expr_B()?;

        match self.current_token().ty {
            Type::Asterix => {
                self.forward_token();
                let rhs = self.expect_expr_B()?;
                Ok(self.new_node(NodeData::Multiply(Box::new(lhs), Box::new(rhs))))
            }
            Type::Percent => {
                self.forward_token();
                let rhs = self.expect_expr_B()?;
                Ok(self.new_node(NodeData::Mod(Box::new(lhs), Box::new(rhs))))
            }
            Type::ForwardSlash => {
                self.forward_token();
                let rhs = self.expect_expr_B()?;
                Ok(self.new_node(NodeData::Div(Box::new(lhs), Box::new(rhs))))
            }
            _ => Ok(lhs),
        }
    }

    fn expect_expr(&mut self) -> Result<Node> {
        let lhs = self.expect_expr_mul_div_mod()?;

        match self.current_token().ty {
            Type::Minus => {
                self.forward_token();
                let rhs = self.expect_expr_mul_div_mod()?;
                Ok(self.new_node(NodeData::Subtract(Box::new(lhs), Box::new(rhs))))
            }
            Type::Plus => {
                self.forward_token();
                let rhs = self.expect_expr_mul_div_mod()?;
                Ok(self.new_node(NodeData::Sum(Box::new(lhs), Box::new(rhs))))
            }
            _ => Ok(lhs),
        }
    }

    fn expect_token(&mut self, ty: Type) -> Result<()> {
        if self.current_token().ty != ty {
            return Err(self.err_uexpected(ty));
        }

        return Ok(());
    }

    fn expect_block(&mut self) -> Result<Vec<Node>> {
        self.expect_token(Type::OpenBrace)?;
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

    fn expect_stmt(&mut self) -> Result<Node> {
        match self.current_token().ty {
            Type::Ident => {
                let next_tok = self.cur + 1;
                match self.tokens[next_tok].ty {
                    Type::DoubleColon | Type::Colon | Type::Equal | Type::ColonEqual => self.expect_decl(),
                    Type::Dot | Type::OpenParen => self.expect_fn_call(),
                    _ => Err(self.err_uexpected(Type::OpenParen)),
                }
            }

            Type::KeywordIf => {
                self.forward_token();
                self.expect_token(Type::OpenParen)?;
                self.forward_token();
                let cond = self.expect_expr()?;
                self.expect_token(Type::CloseParen)?;
                self.forward_token();
                self.expect_token(Type::OpenBrace)?;
                let then = self.expect_block()?;
                match self.current_token().ty {
                    Type::KeywordElse => {
                        self.forward_token();
                        let _else = self.expect_block()?;
                        return Ok(self.new_node(NodeData::If(Box::new(cond), Box::new(then), Some(_else))));
                    }
                    _ => {
                        return Ok(self.new_node(NodeData::If(Box::new(cond), Box::new(then), None)));
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
                self.expect_token(Type::SemiColon)?;
                self.forward_token();
                Ok(self.new_node(NodeData::Return(Box::new(expr))))
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

    pub fn get_ast(mut self) -> Result<AST> {
        let mut top_level = Vec::<Node>::new();
        loop {
            if self.cur >= self.tokens.len() {
                break;
            }
            match self.current_token().ty {
                Type::LoadDirective => {
                    self.forward_token();
                    self.expect_token(Type::StringLiteral)?;
                    let path = self.cur;
                    self.forward_token();
                    if self.current_token().ty == Type::SemiColon {
                        self.forward_token();
                    }
                    top_level.push(self.new_node(NodeData::Load(path)));
                }
                Type::HostDirective => {
                    self.forward_token();
                    self.expect_token(Type::StringLiteral)?;
                    let path = self.cur;
                    self.forward_token();
                    if self.current_token().ty == Type::SemiColon {
                        self.forward_token();
                    }
                    top_level.push(self.new_node(NodeData::Host(path)));
                }
                Type::Ident => {
                    top_level.push(self.expect_decl()?);
                }
                _ => {
                    println!("don't know what to do with: {:?}", self.current_token());
                    unreachable!();
                }
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
fn load_directive() -> Result<()> {
    let parser = Parser::new("#load \"base.loki\";")?;
    let ast = parser.get_ast()?;
    if let NodeData::Load(path) = &ast.top_level[0].data {
        assert_eq!(*path, 1);
    } else {
        panic!()
    }

    Ok(())
}

#[test]
fn host_directive() -> Result<()> {
    let parser = Parser::new("#host \"net/http\";")?;
    let ast = parser.get_ast()?;
    if let NodeData::Host(path) = &ast.top_level[0].data {
        assert_eq!(*path, 1);
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

    if let NodeData::Decl(decl) = &ast.top_level[0].data {
        assert_eq!(decl.mutable, false);
        assert_eq!(decl.name.data, NodeData::Ident(0));
        assert_eq!(decl.expr.data, NodeData::Uint(2));
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

    if let NodeData::Decl(decl) = &ast.top_level[0].data {
        assert_eq!(decl.mutable, false);
        assert_eq!(decl.name.data, NodeData::Ident(0));
        assert_eq!(decl.expr.data, NodeData::True(2));
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

    if let NodeData::Decl(decl) = &ast.top_level[0].data {
        assert_eq!(decl.mutable, false);
        assert_eq!(decl.name.data, NodeData::Ident(0));
        assert_eq!(decl.expr.data, NodeData::False(2));
    } else {
        panic!()
    }

    Ok(())
}

#[test]
fn const_decl_expr_ident() -> Result<()> {
    let mut parser = Parser::new("a :: b;")?;
    let ast = parser.get_ast()?;

    if let NodeData::Decl(decl) = &ast.top_level[0].data {
        assert_eq!(decl.mutable, false);
        assert_eq!(decl.name.data, NodeData::Ident(0));
        assert_eq!(decl.expr.data, NodeData::Ident(2));
    } else {
        panic!()
    }

    Ok(())
}

// #[test] TODO FIXME
// fn const_decl_expr_field_access() -> Result<()> {
//     let mut parser = Parser::new("a :: b.c;")?;
//     let ast = parser.get_ast()?;

//     if let NodeData::Decl(decl) = &ast.top_level[0].data {
//         assert_eq!(decl.mutable, false);
//         assert_eq!(decl.name.data, NodeData::Ident(0));
//         assert_eq!(
//             decl.expr.data,
//             NodeData::FieldAccess(vec![Node::Ident(2), Node::Ident(4)])
//         );
//     } else {
//         panic!()
//     }

//     Ok(())
// }

#[test]
fn const_decl_expr_char() -> Result<()> {
    let mut parser = Parser::new("a :: 'a';")?;
    let ast = parser.get_ast()?;

    if let NodeData::Decl(decl) = &ast.top_level[0].data {
        println!("char node: {:?}", decl.expr);
        assert_eq!(decl.mutable, false);
        assert_eq!(decl.name.data, NodeData::Ident(0));
        assert_eq!(decl.expr.data, NodeData::Char(2));
    } else {
        panic!()
    }

    Ok(())
}

// #[test] TODO FIXME
// fn const_decl_expr_fn_def() -> Result<()> {
//     let mut parser = Parser::new("main :: fn() void {\n\tprintf(\"Hello World\");};")?;
//     let ast = parser.get_ast()?;

//     if let Node::Decl(decl) = &ast.top_level[0] {
//         assert_eq!(decl.mutable, false);
//         assert_eq!(decl.name, Box::new(Node::Ident(0)));
//         assert_eq!(
//             decl.expr,
//             Box::new(Node::FnDef(
//                 Vec::new(),
//                 Box::new(Node::VoidTy(5)),
//                 vec![Node::FnCall(
//                     Box::new(Node::Ident(7)),
//                     vec![Node::StringLiteral(9)]
//                 )]
//             ))
//         );
//     } else {
//         panic!()
//     }

//     Ok(())
// }

#[test]
fn const_decl_expr_string() -> Result<()> {
    let mut parser = Parser::new("a :: \"Amirreza\";")?;
    let ast = parser.get_ast()?;

    if let NodeData::Decl(decl) = &ast.top_level[0].data {
        assert_eq!(decl.mutable, false);
        assert_eq!(decl.name.data, NodeData::Ident(0));
        assert_eq!(decl.expr.data, NodeData::StringLiteral(2));
    } else {
        panic!()
    }

    Ok(())
}

// #[test] TODO FIXME
// fn const_decl_expr_struct() -> Result<()> {
//     let parser = Parser::new("a :: struct { i: int, f: string };")?;
//     let ast = parser.get_ast()?;

//     if let NodeData::Decl(decl) = &ast.top_level[0].data {
//         assert_eq!(decl.mutable, false);
//         assert_eq!(decl.name.data, NodeData::Ident(0));
//         assert_eq!(
//             decl.expr,
//             Box::new(NodeData::Struct(vec![
//                 (Node::Ident(4), Node::IntTy(6)),
//                 (Node::Ident(8), Node::StringTy(10))
//             ]))
//         );
//     } else {
//         panic!()
//     }

//     Ok(())
// }

#[test]
fn const_decl_expr_float() -> Result<()> {
    let mut parser = Parser::new("a :: 2.2;")?;
    let ast = parser.get_ast()?;

    if let NodeData::Decl(decl) = &ast.top_level[0].data {
        assert_eq!(decl.mutable, false);
        assert_eq!(decl.name.data, NodeData::Ident(0));
        assert_eq!(decl.expr.data, NodeData::Float(2));
    } else {
        panic!()
    }

    Ok(())
}

// #[test] TODO FIXME
// fn const_decl_struct_init() -> Result<()> {
//     let mut parser = Parser::new(
//         "
// s :: S{
//     id = 1,
// };
// ",
//     )?;
//     let ast = parser.get_ast()?;

//     if let NodeData::Decl(decl) = &ast.top_level[0] {
//         assert_eq!(
//             *decl,
//             Decl {
//                 mutable: false,
//                 name: Box::new(Node::Ident(0)),
//                 ty: Box::new(None),
//                 expr: Box::new(Node::TypeInit(
//                     Some(Box::new(Node::Ident(2))),
//                     vec![(Node::Ident(4), Node::Uint(6))]
//                 ))
//             }
//         )
//     } else {
//         panic!()
//     }
//     Ok(())
// }


// #[test] TODO FIXME
// fn const_decl_struct_init_infer() -> Result<()> {
//     let mut parser = Parser::new(
//         "
// s :S: {
//     id = 1,
// };
// ",
//     )?;
//     let ast = parser.get_ast()?;

//     if let Node::Decl(decl) = &ast.top_level[0] {
//         assert_eq!(
//             *decl,
//             Decl {
//                 mutable: false,
//                 name: Box::new(Node::Ident(0)),
//                 ty: Box::new(Some(Node::Ident(2))),
//                 expr: Box::new(Node::TypeInit(None, vec![(Node::Ident(5), Node::Uint(7))]))
//             }
//         )
//     } else {
//         panic!()
//     }
//     Ok(())
// }
#[test]
fn var_decl_inside_function() -> Result<()> {
    let parser = Parser::new(
        "main :: fn() int {
a := true;
};",
    )?;
    let ast = parser.get_ast()?;

    if let NodeData::Decl(decl) = &ast.top_level[0].data {
    } else {
        panic!()
    }
    Ok(())

}


#[test]
fn const_decl_fn_with_if() -> Result<()> {
    let mut parser = Parser::new(
        "main :: fn() int {
if (true) {
\t\tprintf(\"Hello\");
}
\treturn 0;
};",
    )?;
    let ast = parser.get_ast()?;

    if let NodeData::Decl(decl) = &ast.top_level[0].data {
    } else {
        panic!()
    }
    Ok(())
}

#[test]
fn const_decl_struct_init_program() -> Result<()> {
    let mut parser = Parser::new(
        "main :: fn() int {
s :: S{
id = 1,
};
};",
    )?;
    let ast = parser.get_ast()?;

    if let NodeData::Decl(decl) = &ast.top_level[0].data {
        println!("{:?}", decl)
    } else {
        panic!()
    }
    Ok(())
}

// #[test] TODO FIXME
// fn const_decl_expr_enum() -> Result<()> {
//     let mut parser = Parser::new(
//         "e :: enum {
// a,
// b
// };",
//     )?;
//     let ast = parser.get_ast()?;

//     if let NodeData::Decl(decl) = &ast.top_level[0].data {
//         assert_eq!(decl.mutable, false);
//         assert_eq!(decl.name, Box::new(Node::Ident(0)));
//         assert_eq!(
//             decl.expr,
//             Box::new(Node::Enum(
//                 false,
//                 vec![(Node::Ident(4), None), (Node::Ident(6), None)]
//             ))
//         )
//     } else {
//         panic!()
//     }

//     Ok(())
// }

// #[test] TODO FIXME
// fn const_decl_expr_union() -> Result<()> {
//     let parser = Parser::new(
//         "e :: enum {
// a,
// b(bool)
// };",
//     )?;
//     let ast = parser.get_ast()?;

//     if let Node::Decl(decl) = &ast.top_level[0] {
//         assert_eq!(decl.mutable, false);
//         assert_eq!(decl.name, Box::new(Node::Ident(0)));
//         assert_eq!(
//             decl.expr,
//             Box::new(Node::Enum(
//                 true,
//                 vec![
//                     (Node::Ident(4), None),
//                     (Node::Ident(6), Some(Node::BoolTy(8)))
//                 ]
//             ))
//         )
//     } else {
//         panic!()
//     }

//     Ok(())
// }

// #[test] TODO FIXME
// fn expr_recursive_field_access() -> Result<()> {
//     let mut parser = Parser::new("a.b.c.d.f;")?;
//     let expr = parser.expect_expr()?;

//     assert_eq!(
//         Node::FieldAccess(vec![
//             Node::Ident(0),
//             Node::Ident(2),
//             Node::Ident(4),
//             Node::Ident(6),
//             Node::Ident(8),
//         ]),
//         expr
//     );

//     Ok(())
// }

// #[test]
// fn expr_ref() -> Result<()> {
//     let mut parser = Parser::new("&a;")?;
//     let expr = parser.expect_expr()?;

//     assert_eq!(NodeData::Ref(Box::new(Node::Ident(1))), expr.data);

//     Ok(())
// }


// #[test]
// fn expr_deref() -> Result<()> {
//     let mut parser = Parser::new("*a;")?;
//     let expr = parser.expect_expr()?;

//     assert_eq!(Node::Deref(Box::new(Node::Ident(1))), expr);

//     Ok(())
// }

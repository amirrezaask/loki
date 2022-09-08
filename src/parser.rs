#![allow(clippy::needless_return)]
use std::collections::HashMap;

use crate::ast::{
    Ast, Type, AstNode, AstNodeData, NodeID, AstOperation, AstTag, BlockType,
};
use crate::lexer::Token;
use crate::lexer::TokenType;
use crate::lexer::Tokenizer;
use crate::stack::Stack;
use crate::utils;
use anyhow::{anyhow, Result};
use rand::distributions::{Alphanumeric, DistString};
const ID_LENGTH: usize = 24;

#[derive(Debug)]
pub struct Parser {
    filename: String,
    src: String,
    tokens: Vec<Token>,
    cur: usize,
    node_counter: i64,
    pub ast: Ast,
    block_stack: Stack<NodeID>,
    block_index_stack: Stack<usize>,
}

// Every parser function should parse until the last token in it's scope and then move cursor to the next token. so every parse function moves the cursor to the next.
impl Parser {
    fn err_uexpected_token(&self, what: TokenType) -> anyhow::Error {
        anyhow::format_err!(
            "In file: {}, Expected {:?}, found {:?} at line:{}, column: {}",
            self.filename,
            what,
            self.current_token().ty,
            self.tokens[self.cur].line,
            self.tokens[self.cur].col,
        )
    }
    fn wrap_err(&self, msg: String) -> anyhow::Error {
        return anyhow::format_err!("In file: {}, {}, at line: {}, column: {}", self.filename, msg, self.current_token().line, self.current_token().col);
    }
    fn get_id(&self) -> String {
        return utils::generate_node_id();
    }
    fn new_node(&mut self, id: String, data: AstNodeData, type_annotation: Type, start_line: usize, start_col: usize) -> AstNode {
        self.node_counter += 1;
        let node = AstNode {
            id,
            data,
            type_information: type_annotation,
            tags: vec![],
            line: start_line, col: start_col,
            parent_block: self.block_stack.top().clone(),
            filename: self.filename.clone(),
            index_in_block: self.block_index_stack.top(),
        };
        self.ast.add_node(node.clone()).unwrap();
        return node;
    }
    fn current_token(&self) -> &Token {
        if self.cur >= self.tokens.len() {
            return &self.tokens[self.tokens.len() - 1];
        }
        return &self.tokens[self.cur];
    }
    fn forward_token(&mut self) {
        self.cur += 1;
    }
    fn backward_token(&mut self) {
        self.cur -= 1;
    }
    pub fn new(
        filename: String,
        src: String,
        tokens: Vec<Token>,
    ) -> Result<Self> {
        let mut ast = Ast::new(
            filename.clone(),
            src.clone(),
            tokens.clone(),
            "".to_string(),
            HashMap::new(),
        )?;
        Ok(Self {
            filename,
            src,
            tokens,
            cur: 0,
            node_counter: 0,
            ast,
            block_stack: Stack::new(),
            block_index_stack: Stack::new(),
        })
    }
    fn expect_ident(&mut self) -> Result<AstNode> {
        match self.current_token().ty {
            TokenType::Ident => {
                let name =
                    self.src[self.current_token().loc.0..=self.current_token().loc.1].to_string();
                let node = self.new_node(self.get_id(), AstNodeData::Ident(name), Type::Unknown, self.current_token().line, self.current_token().col);
                self.forward_token();
                return Ok(node);
            }
            _ => {
                return Err(self.err_uexpected_token(TokenType::Ident));
            }
        }
    }
    fn expect_string_literal(&mut self) -> Result<AstNode> {
        match self.current_token().ty {
            TokenType::StringLiteral => {
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0..=src_range.loc.1];
                return Ok(self.new_node(
                    self.get_id(), 
                    AstNodeData::StringLiteral(literal.to_string()),
                    Type::String,
                    self.current_token().line,
                    self.current_token().col,
                ));
            }
            _ => {
                return Err(self.err_uexpected_token(TokenType::StringLiteral));
            }
        }
    }

    fn expect_semicolon_and_forward(&mut self) -> Result<()> {
        self.expect_token(TokenType::SemiColon)?;
        self.forward_token();

        Ok(())
    }
    fn expect_def(&mut self) -> Result<AstNode> {
        let mut dest = self.expect_expr()?;
        match self.current_token().ty {
            TokenType::Equal => {
                self.forward_token();
                let rhs = self.expect_expr()?;
                let infered_ty = rhs.type_information.clone();
                let node = self.new_node(self.get_id(), AstNodeData::Assign{lhs: dest.id, rhs: rhs.id}, Type::NoType, self.current_token().line, self.current_token().col);
                Ok(node)
            }
            TokenType::DoubleColon => {
                self.forward_token();
                let rhs = self.expect_expr()?;
                let infered_ty = rhs.type_information.clone();
                self.ast.add_type_inference(&dest.id, infered_ty.clone());
                dest.type_information = infered_ty;
                let dest_clone = self.ast.get_node(dest.id.clone())?;
                let node = self.new_node(
                    self.get_id(), 
                    AstNodeData::Def {
                        mutable: false,
                        name: dest.id,
                        expr: rhs.id,
                    },
                    Type::NoType, self.current_token().line, self.current_token().col,
                );
                Ok(node)
            }

            TokenType::Colon => {
                self.forward_token();
                let ty = self.expect_type_expression()?;
                self.ast.add_type_inference(&dest.id, Type::new(&ty, &self.ast)?);
                if self.current_token().ty != TokenType::Equal
                    && self.current_token().ty != TokenType::Colon
                {
                    let decl_node = self.new_node(
                        self.get_id(), AstNodeData::Decl{ name: dest.id.clone(), ty: ty.id.clone()},
                        Type::new(&ty, &self.ast)?, self.current_line(), self.current_col()
                    );

                    if self.current_token().ty == TokenType::ForeignDirective {
                        self.ast.add_tag(&decl_node.id, AstTag::Foreign);
                        self.forward_token();
                    }

                    return Ok(decl_node);
                }
                let mutable = self.current_token().ty == TokenType::Equal;
                self.forward_token();
                let rhs = self.expect_expr()?;
                let infered_ty = rhs.type_information.clone();
                self.ast.add_type_inference(&dest.id, infered_ty);
                let node = self.new_node(
                    self.get_id(), AstNodeData::Def{
                        mutable,
                        name: dest.id,
                        expr: rhs.id,
                    },
                    Type::NoType, self.current_token().line, self.current_token().col
                );
                Ok(node)
            }

            TokenType::ColonEqual => {
                self.forward_token();
                let rhs = self.expect_expr()?;
                let infered_ty = rhs.type_information.clone();
                dest.type_information = infered_ty.clone();
                self.ast.add_type_inference(&dest.id, infered_ty);
                let node = self.new_node(
                    self.get_id(), AstNodeData::Def{
                        mutable: true,
                        name: dest.id,
                        expr: rhs.id,
                    },
                    Type::NoType, self.current_token().line, self.current_token().col
                );
                Ok(node)
            }
            _ => {
                return Err(self.err_uexpected_token(self.current_token().clone().ty));
            }
        }
    }
    fn current_line(&self) -> usize {
        return self.current_token().line;
    }

    fn current_col(&self) -> usize {
        return self.current_token().col;
    }
    fn expect_fn_def(&mut self) -> Result<AstNode> {
        self.expect_token(TokenType::OpenParen)?;
        let mut args: Vec<NodeID> = vec![];
        self.forward_token();
        loop {
            if self.current_token().ty == TokenType::Comma {
                self.forward_token();
            }
            if self.current_token().ty == TokenType::CloseParen {
                self.forward_token();
                break;
            }
            let name = self.expect_ident()?;
            self.expect_token(TokenType::Colon)?;
            self.forward_token();
            let ty = self.expect_type_expression()?;

            let arg = self.new_node(self.get_id(), AstNodeData::Decl{name: name.id.clone(), ty: ty.id.clone()}, Type::new(&ty, &self.ast)?, name.line, name.col);
            args.push(arg.id);
        }
        let mut ret_ty: Option<AstNode> = None;
        if self.current_token().ty != TokenType::OpenBrace {
            ret_ty = Some(self.expect_type_expression()?);
        } else {
            ret_ty = Some(self.new_node(self.get_id(), AstNodeData::VoidTy, Type::Void, self.current_line(), self.current_col()));
        }
        let ret_ty = ret_ty.unwrap();
        if self.current_token().ty != TokenType::OpenBrace {
            // fn type only
            let proto_ty = Type::make_fn_signature(&self.ast, &args, &ret_ty)?;
            let node = self.new_node(self.get_id(), AstNodeData::FnType {
                args,
                ret: ret_ty.id,
            }, proto_ty, self.current_line(), self.current_col());
            return Ok(node);
        }
        let body = self.expect_block()?;
        self.ast.add_block_ty(body.clone(), BlockType::Function);
        let proto_ty = Type::make_fn_signature(&self.ast, &args, &ret_ty)?;
        let sign = self.new_node(self.get_id(), AstNodeData::FnType { args, ret: ret_ty.id }, proto_ty.clone(),self.current_line(), self.current_col()); //@Bug: line & col are wrong
        let fn_def = self.new_node(self.get_id(), AstNodeData::FnDef{ sign: sign.id, body: body }, proto_ty, self.current_line(), self.current_col());
        Ok(fn_def)
    }

    fn expect_fn_call(&mut self) -> Result<AstNode> {
        let name = self.expect_ident()?;
        self.expect_token(TokenType::OpenParen)?;
        let mut args = Vec::<NodeID>::new();

        self.forward_token();
        loop {
            if self.current_token().ty == TokenType::CloseParen {
                self.forward_token();
                break;
            }
            let arg = self.expect_expr()?;
            args.push(arg.id);
            match self.current_token().ty {
                TokenType::Comma => {
                    self.forward_token();
                }

                TokenType::CloseParen => {
                    self.forward_token();
                    break;
                }

                _ => {
                    return Err(self.err_uexpected_token(TokenType::Comma));
                }
            }
        }
        Ok(self.new_node(self.get_id(), AstNodeData::FnCall{fn_name: name.id, args}, Type::Unknown,self.current_line(), self.current_col()))
    }
    fn expect_expr_namespace_access_or_array_index(&mut self) -> Result<AstNode> {
        let container = self.expect_expr_initialize()?;
        match self.current_token().ty {
            TokenType::Dot => {
                self.forward_token();
                let field = self.expect_expr()?;
                let f = self.ast.nodes.get_mut(&field.id).unwrap();
                f.tags.push(AstTag::IsUsedInNamespaceAccess);
                return Ok(self.new_node(self.get_id(), AstNodeData::NamespaceAccess{ namespace: container.id, field: field.id }, Type::Unknown, self.current_line(), self.current_col()));
            }
            TokenType::OpenBracket => {
                self.forward_token();
                let index = self.expect_expr()?;
                self.expect_token(TokenType::CloseBracket)?;
                self.forward_token();
                return Ok(self.new_node(self.get_id(), AstNodeData::ArrayIndex{ arr: container.id, idx: index.id }, Type::Unknown, self.current_line(), self.current_col()));
            }
            _ => {
                return Ok(container);
            }
        }
    }
    /*
    expr -> A (add_minus A)*
    A -> B (mul_div_mod B)*
    B -> initialize (< <= | >= > initialize)*
    container_field -> initialize (. IDENT)
    initialize -> exact_expr ({})*
    exact_expr -> int | unsigned_int | float | string | bool | ident | '(' expr ')' | deref: *expr | ref: &expr
        | field_access | struct_def | enum_def | fn_def | types(int, uint, void, string, bool, char, float
    */
    
    fn expect_expr_initialize(&mut self) -> Result<AstNode> {
        let ty = self.expect_expr_exact_expr()?;
        match self.current_token().ty {
            TokenType::OpenBrace => match ty.data {
                AstNodeData::IntTy(_)
                | AstNodeData::UintTy(_)
                | AstNodeData::CharTy
                | AstNodeData::BoolTy
                | AstNodeData::StringTy
                | AstNodeData::FloatTy(_)
                | AstNodeData::VoidTy => {
                    return Ok(ty);
                }

                _ => {
                    let mut is_struct_init = false;
                    self.forward_token();
                    if self.current_token().ty == TokenType::Ident {
                        self.forward_token();
                        if self.current_token().ty == TokenType::Equal {
                            is_struct_init = true;
                            self.backward_token();
                            self.backward_token();
                        } else {
                            self.backward_token();
                        }
                    }

                    if is_struct_init {
                        self.forward_token();
                        let mut fields = Vec::<(NodeID, NodeID)>::new();
                        loop {
                            if self.current_token().ty == TokenType::CloseBrace {
                                self.forward_token();
                                break;
                            }

                            let mut name = self.expect_ident()?;
                            self.expect_token(TokenType::Equal)?;
                            self.forward_token();
                            let value = self.expect_expr()?;
                            let mut name = self.ast.nodes.get_mut(&name.id.clone()).unwrap();
                            name.type_information = value.type_information.clone();
                            name.tags.push(AstTag::IsUsedInInitialize);
                            fields.push((name.id.clone(), value.id));
                            match self.current_token().ty {
                                TokenType::Comma => {
                                    self.forward_token();
                                }
                                TokenType::CloseBrace => {
                                    self.forward_token();
                                    break;
                                }
                                _ => return Err(self.err_uexpected_token(TokenType::Comma)),
                            }
                        }
                        return Ok(self.new_node(self.get_id(), 
                            AstNodeData::Initialize{ty: ty.id.clone(), fields},
                            Type::Unknown,
                        self.current_line(), self.current_col()));
                    } else {
                        let mut fields = Vec::<NodeID>::new();
                        loop {
                            if self.current_token().ty == TokenType::CloseBrace {
                                self.forward_token();
                                break;
                            }

                            let val = self.expect_expr()?;
                            fields.push(val.id);
                            match self.current_token().ty {
                                TokenType::Comma => {
                                    self.forward_token();
                                }
                                TokenType::CloseBrace => {
                                    self.forward_token();
                                    break;
                                }
                                _ => return Err(self.err_uexpected_token(TokenType::Comma)),
                            }
                        }
                        let node = self.new_node(self.get_id(), 
                            AstNodeData::InitializeArray{elements: fields.clone()},
                            Type::Array(fields.len() as u64, Box::new(Type::Unknown)), self.current_line(), self.current_col()
                        );
                        return Ok(node);
                    }
                }
            },
            _ => {
                return Ok(ty);
            }
        }
    }

    fn is_fn_def(&mut self) -> bool {
        if self.current_token().ty != TokenType::OpenParen {
            return false;
        }
        self.forward_token();
        if self.current_token().ty == TokenType::CloseParen {
            return true;
        }
        if self.current_token().ty == TokenType::Ident {
            self.forward_token();
            if self.current_token().ty == TokenType::Colon {
                return true;
            }
        }
        return false;
    }

    fn expect_type_expression(&mut self) -> Result<AstNode> {
        match self.current_token().ty {
            TokenType::KeywordStruct => {
                self.forward_token();
                self.expect_token(TokenType::OpenBrace)?;
                self.forward_token();
                let mut fields = Vec::<NodeID>::new();
                loop {
                    if self.current_token().ty == TokenType::CloseBrace {
                        self.forward_token();
                        break;
                    }
                    let mut name = self.expect_ident()?;

                    self.expect_token(TokenType::Colon)?;
                    self.forward_token();
                    let ty = self.expect_type_expression()?;
                    self.ast.add_type_inference(&name.id, Type::new(&ty, &self.ast)?);
                    let field = self.new_node(self.get_id(), AstNodeData::Decl{name: name.id, ty: ty.id.clone()}, Type::new(&ty, &self.ast)?, self.current_line(), self.current_col());
                    fields.push(field.id);
                    match self.current_token().ty {
                        TokenType::Comma => {
                            self.forward_token();
                            continue;
                        }
                        TokenType::CloseBrace => {
                            self.forward_token();
                            break;
                        }
                        _ => return Err(self.err_uexpected_token(TokenType::Comma)),
                    }
                }
                let node = self.new_node(self.get_id(), AstNodeData::StructTy(fields.clone()), Type::from_struct_fields(fields, &self.ast)?, self.current_line(), self.current_col());
                Ok(node)
            }
            TokenType::KeywordEnum => {
                self.forward_token();
                self.expect_token(TokenType::OpenBrace)?;
                self.forward_token();
                let mut variants = Vec::<NodeID>::new();
                let mut is_union = false;
                loop {
                    if self.current_token().ty == TokenType::CloseBrace {
                        self.forward_token();
                        break;
                    }

                    if self.current_token().ty == TokenType::Comma {
                        self.forward_token();
                        continue;
                    }

                    let mut name = self.expect_ident()?;
                    self.ast
                        .add_type_inference(&name.id, Type::UnsignedInt(64));
                    let ty_node = self.new_node(self.get_id(), AstNodeData::UintTy(64), Type::NoType, self.current_line(), self.current_col());
                    let variant = self.new_node(self.get_id(), AstNodeData::Decl{name: name.id, ty: ty_node.id.clone()}, Type::UnsignedInt(64), self.current_line(), self.current_col());

                    match self.current_token().ty {
                        TokenType::Comma => {
                            variants.push(variant.id);
                            self.forward_token();
                            continue;
                        }
                        TokenType::CloseBrace => {
                            variants.push(variant.id);
                            self.forward_token();
                            break;
                        }
                        _ => {
                            self.expect_token(TokenType::Comma)?;
                        }
                    }
                }
                let node = self.new_node(self.get_id(), AstNodeData::EnumTy(variants.clone()), Type::from_enum_variants(variants, &self.ast)?, self.current_line(), self.current_col());
                Ok(node)
            }
            TokenType::OpenBracket => {
                // array type
                self.forward_token();
                let len = self.expect_expr()?;
                self.expect_token(TokenType::CloseBracket)?;
                self.forward_token();
                let ty = self.expect_type_expression()?;
                let node = self.new_node(self.get_id(), 
                    AstNodeData::ArrayTy{length: len.extract_uint(), elem_ty: Type::new(&ty, &self.ast)? },
                    Type::Array(10, Box::new(Type::new(&ty, &self.ast)?)), self.current_line(), self.current_col()
                );
                return Ok(node);
            }
            TokenType::Ident => {
                self.forward_token();
                match self.current_token().ty {
                    TokenType::OpenParen => {
                        //function call
                        self.backward_token();
                        self.expect_fn_call()
                    }
                    _ => {
                        self.backward_token();
                        let name = self.expect_ident()?;
                        return Ok(name);
                    }
                }
            }
            TokenType::KeywordVoid => {
                self.forward_token();
                Ok(self.new_node(self.get_id(), AstNodeData::VoidTy, Type::Void, self.current_line(), self.current_col()))
            }
            TokenType::KeywordInt => {
                self.forward_token();
                Ok(self.new_node(self.get_id(), AstNodeData::IntTy(64), Type::SignedInt(64), self.current_line(), self.current_col()))
            }
            TokenType::KeywordInt8 => {
                self.forward_token();
                Ok(self.new_node(self.get_id(), AstNodeData::IntTy(8), Type::SignedInt(8), self.current_line(), self.current_col()))
            }
            TokenType::KeywordInt16 => {
                self.forward_token();
                Ok(self.new_node(self.get_id(), AstNodeData::IntTy(16), Type::SignedInt(16), self.current_line(), self.current_col()))
            }
            TokenType::KeywordInt32 => {
                self.forward_token();
                Ok(self.new_node(self.get_id(), AstNodeData::IntTy(32), Type::SignedInt(32), self.current_line(), self.current_col()))
            }
            TokenType::KeywordInt64 => {
                self.forward_token();
                Ok(self.new_node(self.get_id(), AstNodeData::IntTy(64), Type::SignedInt(64), self.current_line(), self.current_col()))
            }
            TokenType::KeywordInt128 => {
                self.forward_token();
                Ok(self.new_node(self.get_id(), AstNodeData::IntTy(128), Type::SignedInt(128), self.current_line(), self.current_col()))
            }
            TokenType::KeywordUint => {
                self.forward_token();
                Ok(self.new_node(self.get_id(), AstNodeData::UintTy(64), Type::UnsignedInt(64), self.current_line(), self.current_col()))
            }
            TokenType::KeywordUint8 => {
                self.forward_token();
                Ok(self.new_node(self.get_id(), AstNodeData::UintTy(8), Type::UnsignedInt(8), self.current_line(), self.current_col()))
            }
            TokenType::KeywordUint16 => {
                self.forward_token();
                Ok(self.new_node(self.get_id(), AstNodeData::UintTy(16), Type::UnsignedInt(16), self.current_line(), self.current_col()))
            }
            TokenType::KeywordUint32 => {
                self.forward_token();
                Ok(self.new_node(self.get_id(), AstNodeData::UintTy(32), Type::UnsignedInt(32), self.current_line(), self.current_col()))
            }
            TokenType::KeywordUint64 => {
                self.forward_token();
                Ok(self.new_node(self.get_id(), AstNodeData::UintTy(64), Type::UnsignedInt(64), self.current_line(), self.current_col()))
            }
            TokenType::KeywordUint128 => {
                self.forward_token();
                Ok(self.new_node(self.get_id(), AstNodeData::UintTy(128), Type::UnsignedInt(128), self.current_line(), self.current_col()))
            }
            TokenType::KeywordFloat32 => {
                self.forward_token();
                Ok(self.new_node(self.get_id(), AstNodeData::FloatTy(32), Type::Float(32), self.current_line(), self.current_col()))
            }
            TokenType::KeywordFloat64 => {
                self.forward_token();
                Ok(self.new_node(self.get_id(), AstNodeData::FloatTy(64), Type::Float(64), self.current_line(), self.current_col()))
            }
            TokenType::KeywordChar => {
                self.forward_token();
                Ok(self.new_node(self.get_id(), AstNodeData::CharTy, Type::Char, self.current_line(), self.current_col()))
            }
            TokenType::KeywordBool => {
                self.forward_token();
                Ok(self.new_node(self.get_id(), AstNodeData::BoolTy, Type::Bool, self.current_line(), self.current_col()))
            }

            TokenType::KeywordString => {
                self.forward_token();
                Ok(self.new_node(self.get_id(), AstNodeData::StringTy, Type::String, self.current_line(), self.current_col()))
            }
            TokenType::Asterix | TokenType::DoubleRightAngle => {
                self.forward_token();
                let expr = self.expect_type_expression()?;
                return Ok(self.new_node(self.get_id(), AstNodeData::PointerTo(expr.id), Type::Pointer(Box::new(expr.type_information)), self.current_line(), self.current_col()));
            }
            TokenType::OpenParen => {
                let before_check_fn_def_cur = self.cur;
                if self.is_fn_def() {
                    self.cur = before_check_fn_def_cur;
                    return Ok(self.expect_fn_def()?);
                }

                self.cur = before_check_fn_def_cur;
                self.forward_token();
                let expr = self.expect_type_expression()?;
                self.expect_token(TokenType::CloseParen)?;
                self.forward_token();
                Ok(expr)
            }
            TokenType::CVarArgsDirective => {
                self.forward_token();
                Ok(self.new_node(self.get_id(), AstNodeData::CVarArgs, Type::CVarArgs, self.current_line(), self.current_col()))
            }
            TokenType::CString => {
                self.forward_token();
                Ok(self.new_node(self.get_id(), AstNodeData::CString, Type::CString, self.current_line(), self.current_col()))
            }
            _ => {
                return Err(self.wrap_err(format!("expected a type expression found: {:?}", self.current_token())));
            }
        }
    }

    fn expect_expr_exact_expr(&mut self) -> Result<AstNode> {
        match self.current_token().ty {
            TokenType::UnsignedInt => {
                self.forward_token();
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0..=src_range.loc.1];
                let literal = literal.parse::<u64>()?;
                Ok(self.new_node(self.get_id(), AstNodeData::Unsigned(literal), Type::UnsignedInt(64), self.current_line(), self.current_col()))
            }
            TokenType::Float => {
                self.forward_token();
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0..=src_range.loc.1];
                let literal = literal.parse::<f64>()?;
                Ok(self.new_node(self.get_id(), AstNodeData::Float(literal), Type::Float(64), self.current_line(), self.current_col()))
            }
            TokenType::StringLiteral => {
                self.forward_token();
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0..=src_range.loc.1];
                Ok(self.new_node(self.get_id(), 
                    AstNodeData::StringLiteral(literal.to_string()),
                    Type::String, self.current_line(), self.current_col()
                ))
            }
            
            TokenType::KeywordTrue => {
                self.forward_token();
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0..=src_range.loc.1];
                Ok(self.new_node(self.get_id(), AstNodeData::Bool(literal == "true"), Type::Bool, self.current_line(), self.current_col()))
            }
            TokenType::KeywordFalse => {
                self.forward_token();
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0..=src_range.loc.1];
                Ok(self.new_node(self.get_id(), AstNodeData::Bool(literal == "true"), Type::Bool, self.current_line(), self.current_col()))
            }
            TokenType::Char => {
                self.forward_token();
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0 + 1..=src_range.loc.1 - 1];
                Ok(self.new_node(self.get_id(), 
                    AstNodeData::Char(literal.chars().next().unwrap()),
                    Type::Char, self.current_line(), self.current_col()
                ))
            }
            TokenType::OpenBracket => {
                // array type
                self.forward_token();
                let len = self.expect_expr()?;
                self.expect_token(TokenType::CloseBracket)?;
                self.forward_token();
                let ty = self.expect_type_expression()?;
                let node = self.new_node(self.get_id(), 
                    AstNodeData::ArrayTy{length: len.extract_uint(), elem_ty: Type::Unknown },
                    Type::Unknown, self.current_line(), self.current_col()
                );
                return Ok(node);
            }
            TokenType::OpenParen => {
                let before_check_fn_def_cur = self.cur;
                if self.is_fn_def() {
                    self.cur = before_check_fn_def_cur;
                    return Ok(self.expect_fn_def()?);
                }

                self.cur = before_check_fn_def_cur;
                self.forward_token();
                let expr = self.expect_expr()?;
                self.expect_token(TokenType::CloseParen)?;
                self.forward_token();
                Ok(expr)
            }

            TokenType::Asterix | TokenType::DoubleLeftAngle => {
                self.forward_token();
                let expr = self.expect_expr()?;
                return Ok(self.new_node(self.get_id(), AstNodeData::Deref(expr.id), expr.type_information, self.current_line(), self.current_col()));
            }
            TokenType::Ampersand | TokenType::DoubleRightAngle => {
                self.forward_token();
                let expr = self.expect_expr()?;
                return Ok(self.new_node(self.get_id(), 
                    AstNodeData::PointerTo(expr.id),
                    Type::Pointer(Box::new(expr.type_information)),
                    self.current_line(), self.current_col()
                ));
            }
            TokenType::Ident => {
                self.forward_token();
                match self.current_token().ty {
                    TokenType::OpenParen => {
                        //function call
                        self.backward_token();
                        self.expect_fn_call()
                    }
                    _ => {
                        self.backward_token();
                        let name = self.expect_ident()?;
                        return Ok(name);
                    }
                }
            }

            TokenType::Bang => {
                self.forward_token();
                let to_be_not_value = self.expect_expr()?;
                return Ok(self.new_node(self.get_id(), AstNodeData::Not(to_be_not_value.id.clone()), Type::Bool, self.current_line(), self.current_col()));
            }
            _ => {
                return Err(self.wrap_err(format!("unknown expr: {:?}", self.current_token())));
            }
        }
    }

    fn ast_op_from_token_type(&mut self, op: TokenType) -> Result<AstOperation> {
        match op {
            TokenType::GreaterEqual => Ok(AstOperation::GreaterEqual),
            TokenType::Bang => Ok(AstOperation::Not),
            TokenType::LessEqual => Ok(AstOperation::LessEqual),
            TokenType::LeftAngle => Ok(AstOperation::Less),
            TokenType::RightAngle => Ok(AstOperation::Greater),
            TokenType::DoubleEqual => Ok(AstOperation::Equal),
            TokenType::NotEqual => Ok(AstOperation::NotEqual),
            TokenType::DoublePipe => Ok(AstOperation::BinaryOr),
            TokenType::DoubleAmpersand => Ok(AstOperation::BinaryAnd),
            _ => {
                return Err(self.wrap_err(format!("expected token that can represent an operation but found: {:?}", op)));
            }
        }
    }

    fn expect_expr_binary_operations(&mut self) -> Result<AstNode> {
        let lhs = self.expect_expr_namespace_access_or_array_index()?;
        match self.current_token().ty {
            TokenType::LeftAngle
            | TokenType::RightAngle
            | TokenType::LessEqual
            | TokenType::GreaterEqual
            | TokenType::DoubleEqual
            | TokenType::DoubleAmpersand
            | TokenType::DoublePipe
            | TokenType::Bang
            | TokenType::NotEqual => {
                let op = self.ast_op_from_token_type(self.current_token().ty.clone())?;
                self.forward_token();
                let rhs = self.expect_expr_namespace_access_or_array_index()?;
                Ok(self.new_node(self.get_id(), 
                    AstNodeData::BinaryOperation{ operation: op, left: lhs.id, right:rhs.id },
                    Type::Bool, self.current_line(), self.current_col()
                ))
            }

            _ => Ok(lhs),
        }
    }
    fn expect_expr_mul_div_mod(&mut self) -> Result<AstNode> {
        let mut lhs = self.expect_expr_binary_operations()?;

        match self.current_token().ty {
            TokenType::Asterix => {
                self.forward_token();
                let rhs = self.expect_expr_binary_operations()?;
                let mut infered_ty = lhs.type_information.clone();
                if !lhs.type_information.is_unknown()
                    && !rhs.type_information.is_unknown()
                    && lhs.type_information != rhs.type_information
                {
                    return Err(self.wrap_err(format!(
                        "both sides of assignment should be same type, but lhs is: {:?} and rhs is: {:?}", 
                            lhs.type_information,
                            rhs.type_information,
                            )));
                }
                if rhs.type_information != Type::Unknown {
                    infered_ty = rhs.type_information.clone();
                }
                if lhs.type_information.is_unknown() {
                    lhs.type_information = rhs.type_information.clone();
                }
                Ok(self.new_node(self.get_id(), 
                    AstNodeData::BinaryOperation{ operation: AstOperation::Multiply, left: lhs.id, right:rhs.id },
                    Type::Bool, self.current_line(), self.current_col()
                ))
            }
            TokenType::Percent => {
                self.forward_token();
                let rhs = self.expect_expr_binary_operations()?;
                let mut infered_ty = lhs.type_information.clone();
                if !lhs.type_information.is_unknown()
                    && !rhs.type_information.is_unknown()
                    && lhs.type_information != rhs.type_information
                {
                    return Err(self.wrap_err(format!(
                        "both sides of assignment should be same type, but lhs is: {:?} and rhs is: {:?}", 
                            lhs.type_information,
                            rhs.type_information,
                            )));
                }
                if rhs.type_information != Type::Unknown {
                    infered_ty = rhs.type_information.clone();
                }
                if lhs.type_information.is_unknown() {
                    lhs.type_information = rhs.type_information.clone();
                }
                Ok(self.new_node(self.get_id(), 
                    AstNodeData::BinaryOperation{ operation: AstOperation::Modulu, left: lhs.id, right:rhs.id },
                    Type::Bool, self.current_line(), self.current_col()
                ))
            }
            TokenType::ForwardSlash => {
                self.forward_token();
                let rhs = self.expect_expr_binary_operations()?;
                let mut infered_ty = lhs.type_information.clone();
                if !lhs.type_information.is_unknown()
                    && !rhs.type_information.is_unknown()
                    && lhs.type_information != rhs.type_information
                {
                    return Err(self.wrap_err(format!(
                        "both sides of assignment should be same type, but lhs is: {:?} and rhs is: {:?}", 
                            lhs.type_information,
                            rhs.type_information,
                            )));
                }
                if rhs.type_information != Type::Unknown {
                    infered_ty = rhs.type_information.clone();
                }
                if lhs.type_information.is_unknown() {
                    lhs.type_information = rhs.type_information.clone();
                }
               Ok(self.new_node(self.get_id(), 
                    AstNodeData::BinaryOperation{ operation: AstOperation::Multiply, left: lhs.id, right:rhs.id },
                    Type::Bool, self.current_line(), self.current_col()
                ))
            }
            _ => Ok(lhs),
        }
    }

    fn expect_expr(&mut self) -> Result<AstNode> {
        match self.current_token().ty {
            TokenType::KeywordStruct |
            TokenType::KeywordEnum |
            TokenType::KeywordInt |
            TokenType::KeywordInt8 |
            TokenType::KeywordInt16 |
            TokenType::KeywordInt32 |
            TokenType::KeywordInt64 |
            TokenType::KeywordInt128 |
            TokenType::KeywordUint |
            TokenType::KeywordUint8 |
            TokenType::KeywordUint16 |
            TokenType::KeywordUint32 |
            TokenType::KeywordUint64 |
            TokenType::KeywordUint128 |
            TokenType::KeywordString |
            TokenType::KeywordFloat32 |
            TokenType::KeywordFloat64 |
            TokenType::KeywordVoid |
            TokenType::KeywordChar => {
                return self.expect_type_expression();
            }
            _ => {}
        }
        let mut lhs = self.expect_expr_mul_div_mod()?;
        match self.current_token().ty {
            TokenType::Minus => {
                self.forward_token();
                let rhs = self.expect_expr_mul_div_mod()?;
                let mut infered_ty = lhs.type_information.clone();
                if !lhs.type_information.is_unknown()
                    && !rhs.type_information.is_unknown()
                    && lhs.type_information != rhs.type_information
                {
                    return Err(self.wrap_err(format!(
                        "both sides of assignment should be same type, but lhs is: {:?} and rhs is: {:?}", 
                            lhs.type_information,
                            rhs.type_information,
                            )));
                }
                if rhs.type_information != Type::Unknown {
                    infered_ty = rhs.type_information.clone();
                }
                if lhs.type_information.is_unknown() {
                    lhs.type_information = rhs.type_information.clone();
                }
                Ok(self.new_node(self.get_id(), 
                    AstNodeData::BinaryOperation{ operation: AstOperation::Subtract, left: lhs.id, right:rhs.id },
                    Type::Bool, self.current_line(), self.current_col()
                ))
            }
            TokenType::Plus => {
                self.forward_token();
                let rhs = self.expect_expr_mul_div_mod()?;
                let mut infered_ty = lhs.type_information.clone();
                if !lhs.type_information.is_unknown()
                    && !rhs.type_information.is_unknown()
                    && lhs.type_information != rhs.type_information
                {
                    return Err(self.wrap_err(format!(
                        "both sides of assignment should be same type, but lhs is: {:?} and rhs is: {:?}", 
                            lhs.type_information,
                            rhs.type_information,
                            )));
                }
                if rhs.type_information != Type::Unknown {
                    infered_ty = rhs.type_information.clone();
                }
                if lhs.type_information.is_unknown() {
                    lhs.type_information = rhs.type_information.clone();
                }
                Ok(self.new_node(self.get_id(), 
                    AstNodeData::BinaryOperation{ operation: AstOperation::Sum, left: lhs.id, right:rhs.id },
                    Type::Bool, self.current_line(), self.current_col()
                ))
            }

            _ => Ok(lhs),
        }
    }

    fn expect_token(&mut self, ty: TokenType) -> Result<()> {
        if self.current_token().ty != ty {
            return Err(self.err_uexpected_token(ty));
        }

        return Ok(());
    }

    fn expect_block(&mut self) -> Result<NodeID> {
        if self.current_token().ty == TokenType::OpenBrace {
            self.forward_token()
        }
        let mut stmts = Vec::<NodeID>::new();
        let block_id = self.get_id();
        let is_file_root = self.block_stack.len() == 0;
        let block_node = self.new_node(block_id.clone(), AstNodeData::Block{nodes: vec![], is_file_root, ty: BlockType::Unknown}, Type::NoType, self.current_line(), self.current_col()); 
        self.block_stack.push(block_id.clone());
        self.block_index_stack.push(0);
        loop {
            if self.current_token().ty == TokenType::CloseBrace || self.cur >= self.tokens.len() {
                break;
            }
            let stmt = self.expect_stmt()?;
            self.if_semicolon_forward();
            let mut block_mut = self.ast.nodes.get_mut(&block_id.clone()).unwrap();
            if let AstNodeData::Block { ref ty, ref is_file_root, ref mut nodes } = block_mut.data {
                nodes.push(stmt.id);
            }
            let idx = self.block_index_stack.pop();
            self.block_index_stack.push(idx+1);
        }

        self.block_stack.pop();
        
        self.forward_token();
        self.block_index_stack.pop();
        Ok(block_node.id)
    }

    fn if_semicolon_forward(&mut self) {
        if self.current_token().ty == TokenType::SemiColon {
            self.forward_token();
        }
    }

    fn expect_for_c(&mut self) -> Result<AstNode> {
        let start = self.expect_def()?;
        self.expect_semicolon_and_forward()?;
        let cond = self.expect_expr()?;
        self.expect_semicolon_and_forward()?;
        let cont = self.expect_stmt()?;
        self.expect_token(TokenType::CloseParen)?;
        self.forward_token();
        let body = self.expect_block()?;
        self.ast.add_block_ty(body.clone(), BlockType::ForC);
        let for_node = self.new_node(self.get_id(), 
            AstNodeData::For{start: start.id, cond: cond.id, cont: cont.id, body: body},
            Type::Unknown, self.current_line(), self.current_col()
        );
        return Ok(for_node);
    }
    
    fn expect_for_each(&mut self) -> Result<AstNode> {
        let iterator = self.expect_ident()?;
        self.forward_token();
        let iterable = self.expect_expr()?;
        self.expect_token(TokenType::CloseParen)?;
        self.forward_token();
        self.expect_token(TokenType::OpenBrace)?;
        let body = self.expect_block()?;
        self.ast.add_block_ty(body.clone(), BlockType::ForIn);
        let node = self.new_node(self.get_id(), 
            AstNodeData::ForIn{iterator: iterator.id, iterable: iterable.id, body},
            Type::Unknown, self.current_line(), self.current_col()
        );
        return Ok(node);
    }

    fn expect_for_each_implicit_iterator(&mut self) -> Result<AstNode> {
        let iterable = self.expect_expr()?;
        self.expect_token(TokenType::CloseParen)?;
        self.forward_token();
        self.expect_token(TokenType::OpenBrace)?;
        let body = self.expect_block()?;
        let iterator = self.new_node(self.get_id(), AstNodeData::Ident("it".to_string()), Type::Unknown, self.current_line(), self.current_col());
        self.ast.add_block_ty(body.clone(), BlockType::ForIn);
        let node = self.new_node(self.get_id(), 
            AstNodeData::ForIn{iterator: iterator.id, iterable: iterable.id, body},
            Type::Unknown, self.current_line(), self.current_col()
        );
        return Ok(node);
    }

    fn expect_if(&mut self) -> Result<AstNode> {
        self.forward_token();
        self.expect_token(TokenType::OpenParen)?;
        self.forward_token();
        let cond = self.expect_expr()?;
        if !cond.type_information.is_unknown() && cond.type_information != Type::Bool {
            return Err(self.wrap_err(format!("if condition should be boolean but given condition is {:?}", cond.type_information)));
        }
        self.expect_token(TokenType::CloseParen)?;
        self.forward_token();

        self.expect_token(TokenType::OpenBrace)?;
        let then = self.expect_block()?;
        if self.current_token().ty == TokenType::KeywordElse {
            self.forward_token();
            if self.current_token().ty == TokenType::KeywordIf {
                let else_if = self.expect_if()?;
                let mut cond_thens = vec![(cond.id, then)];
                for cond_then in else_if.extract_if() {
                    cond_thens.push(cond_then.clone());
                }
                let node = self.new_node(self.get_id(), 
                    AstNodeData::If{ cases: cond_thens },
                    Type::Unknown, self.current_line(), self.current_col()
                );
                return Ok(node);
            } else if self.current_token().ty == TokenType::OpenBrace {
                let _else = self.expect_block()?;
                let default_case = self.new_node(self.get_id(), AstNodeData::Bool(true), Type::Bool, self.current_line(), self.current_col());
                let cases = vec![(cond.id, then), (default_case.id, _else)];
                let node = self.new_node(self.get_id(), 
                    AstNodeData::If { cases },
                    Type::Unknown, self.current_line(), self.current_col()
                );
                return Ok(node);
            } else {
                return Err(self.wrap_err(format!("after else keyword we expect either a code block or if keyword but found: {:?}", self.current_token())));
            }
        }
        let node = self.new_node(self.get_id(), 
            AstNodeData::If{
                cases: vec![(cond.id, then)],
            },
            Type::Unknown, self.current_line(), self.current_col()
        );
        return Ok(node);
    }
    fn expect_stmt(&mut self) -> Result<AstNode> {
        match self.current_token().ty {
            TokenType::LoadDirective => {
                self.forward_token();
                self.expect_token(TokenType::StringLiteral)?;
                let path = self.cur;
                self.forward_token();
                if self.current_token().ty == TokenType::SemiColon {
                    self.forward_token();
                }
                let src_range = &self.tokens[path];
                let literal = &self.src[src_range.loc.0..=src_range.loc.1];
                let top =
                    self.new_node(self.get_id(), AstNodeData::Load(literal.to_string()), Type::NoType, self.current_line(), self.current_col());
                Ok(top)
                }
            TokenType::CompilerFlagDirective => {
                self.forward_token();
                self.expect_token(TokenType::StringLiteral)?;
                let path = self.cur;
                self.forward_token();
                if self.current_token().ty == TokenType::SemiColon {
                    self.forward_token();
                }
                let top = self.new_node(self.get_id(), 
                    AstNodeData::CompilerFlags(path.to_string()),
                    Type::NoType, self.current_line(), self.current_col()
                );
                Ok(top)
            }
            TokenType::HostDirective => {
                self.forward_token();
                self.expect_token(TokenType::StringLiteral)?;
                let path = self.cur;
                self.forward_token();
                if self.current_token().ty == TokenType::SemiColon {
                    self.forward_token();
                }
                let src_range = &self.tokens[path];
                let literal = &self.src[src_range.loc.0..=src_range.loc.1];
                let top =
                    self.new_node(self.get_id(), AstNodeData::Host(literal.to_string()), Type::NoType, self.current_line(), self.current_col());
                Ok(top)
            }
            TokenType::Ident => {
                self.forward_token();
                match self.current_token().ty {
                    TokenType::DoubleColon
                    | TokenType::Colon
                    | TokenType::Equal
                    | TokenType::ColonEqual
                    | TokenType::OpenBracket
                    | TokenType::Dot => {
                        self.backward_token();
                        let def = self.expect_def()?;
                        return Ok(def);
                    }
                    TokenType::OpenParen => {
                        self.backward_token();
                        self.expect_fn_call()
                    }
                    TokenType::PlusEqual => {
                        self.backward_token();
                        let mut lhs = self.expect_expr()?;

                        self.forward_token();
                        let rhs = self.expect_expr()?;
                        lhs.type_information = rhs.type_information.clone();
                        let infered_ty = rhs.type_information;
                        let inner = self
                            .new_node(self.get_id(), AstNodeData::BinaryOperation {operation: AstOperation::Sum, left: lhs.id.clone(), right:rhs.id }, infered_ty, self.current_line(), self.current_col());

                        return Ok(self
                            .new_node(self.get_id(), AstNodeData::Assign{lhs: lhs.id, rhs: inner.id}, Type::NoType, self.current_line(), self.current_col()));
                    }
                    TokenType::MinusEqual => {
                        self.backward_token();
                        let mut lhs = self.expect_expr()?;
                        self.forward_token();

                        let rhs = self.expect_expr()?;
                        lhs.type_information = rhs.type_information.clone();

                        let infered_ty = rhs.type_information;
                        let inner = self
                            .new_node(self.get_id(), AstNodeData::BinaryOperation {operation: AstOperation::Subtract, left: lhs.id.clone(), right:rhs.id }, infered_ty, self.current_line(), self.current_col());
                        return Ok(self
                            .new_node(self.get_id(), AstNodeData::Assign{lhs: lhs.id, rhs: inner.id}, Type::NoType, self.current_line(), self.current_col()));
                    }
                    TokenType::ModEqual => {
                        self.backward_token();
                        let mut lhs = self.expect_expr()?;
                        self.forward_token();

                        let rhs = self.expect_expr()?;
                        lhs.type_information = rhs.type_information.clone();

                        let infered_ty = rhs.type_information;
                        let inner = self
                            .new_node(self.get_id(), AstNodeData::BinaryOperation {operation: AstOperation::Modulu, left: lhs.id.clone(), right:rhs.id }, infered_ty, self.current_line(), self.current_col());
                        return Ok(self
                            .new_node(self.get_id(), AstNodeData::Assign{lhs: lhs.id, rhs: inner.id}, Type::NoType, self.current_line(), self.current_col()));
                    }
                    TokenType::MulEqual => {
                        self.backward_token();
                        let mut lhs = self.expect_expr()?;
                        self.forward_token();

                        let rhs = self.expect_expr()?;
                        lhs.type_information = rhs.type_information.clone();

                        let infered_ty = rhs.type_information;
                        let inner = self
                            .new_node(self.get_id(), AstNodeData::BinaryOperation {operation: AstOperation::Multiply, left: lhs.id.clone(), right:rhs.id }, infered_ty, self.current_line(), self.current_col());
                        return Ok(self
                            .new_node(self.get_id(), AstNodeData::Assign{lhs: lhs.id, rhs: inner.id}, Type::NoType, self.current_line(), self.current_col()));
                    }
                    TokenType::DivEqual => {
                        self.backward_token();
                        let mut lhs = self.expect_expr()?;
                        self.forward_token();

                        let rhs = self.expect_expr()?;
                        lhs.type_information = rhs.type_information.clone();

                        let infered_ty = rhs.type_information;
                        let inner = self
                            .new_node(self.get_id(), AstNodeData::BinaryOperation {operation: AstOperation::Divide, left: lhs.id.clone(), right: rhs.id }, infered_ty, self.current_line(), self.current_col());

                        return Ok(self
                            .new_node(self.get_id(), AstNodeData::Assign{lhs: lhs.id, rhs: inner.id}, Type::NoType, self.current_line(), self.current_col()));
                    }
                    TokenType::DoublePlus => {
                        self.backward_token();
                        let mut lhs = self.expect_expr()?;
                        self.forward_token();
                        let rhs = self.new_node(self.get_id(), AstNodeData::Unsigned(1), Type::UnsignedInt(64), self.current_line(), self.current_col());
                        lhs.type_information = rhs.type_information.clone();

                        let infered_ty = rhs.type_information;
                        let inner = self
                            .new_node(self.get_id(), AstNodeData::BinaryOperation {operation: AstOperation::Sum, left: lhs.id.clone(), right:rhs.id }, infered_ty, self.current_line(), self.current_col());
                        return Ok(self
                            .new_node(self.get_id(), AstNodeData::Assign{lhs: lhs.id, rhs: inner.id}, Type::NoType, self.current_line(), self.current_col()));
                    }
                    TokenType::DoubleMinus => {
                        self.backward_token();
                        let mut lhs = self.expect_expr()?;
                        self.forward_token();

                        let rhs = self.new_node(self.get_id(), AstNodeData::Unsigned(1), Type::UnsignedInt(64), self.current_line(), self.current_col());
                        lhs.type_information = rhs.type_information.clone();

                        let infered_ty = rhs.type_information;
                        let inner = self
                            .new_node(self.get_id(), AstNodeData::BinaryOperation {operation: AstOperation::Sum, left: lhs.id.clone(), right: rhs.id }, infered_ty, self.current_line(), self.current_col());
                        return Ok(self
                            .new_node(self.get_id(), AstNodeData::Assign{lhs: lhs.id, rhs: inner.id}, Type::NoType, self.current_line(), self.current_col()));
                    }
                    _ => {
                        return Err(self.wrap_err(format!("expecting ident statement found: {:?}", self.current_token())));
                    }
                }
            }

            TokenType::Asterix | TokenType::DoubleRightAngle => {
                self.forward_token();
                let deref = self.expect_expr()?;
                let deref = self.new_node(self.get_id(), AstNodeData::Deref(deref.id), Type::Unknown, self.current_line(), self.current_col());
                self.expect_token(TokenType::Equal)?;
                self.forward_token();
                let expr = self.expect_expr()?;

                let node =
                    self.new_node(self.get_id(), AstNodeData::Assign{lhs: deref.id, rhs: expr.id}, Type::NoType, self.current_line(), self.current_col());

                return Ok(node);
            }

            TokenType::KeywordIf => {
                return self.expect_if();
            }
            TokenType::KeywordWhile => {
                self.forward_token();
                self.expect_token(TokenType::OpenParen)?;
                self.forward_token();
                let cond = self.expect_expr()?;
                if !cond.type_information.is_unknown() && cond.type_information != Type::Bool {
                    return Err(self.wrap_err(format!("while condition should be boolean but given condition is infered to be {:?}", cond.type_information)));
                }
                self.forward_token();
                self.expect_token(TokenType::OpenBrace)?;
                let body = self.expect_block()?;
                let node = self.new_node(self.get_id(), AstNodeData::While{cond: cond.id, body}, Type::Unknown, self.current_line(), self.current_col());
                return Ok(node);
            }

            TokenType::KeywordFor => {
                self.forward_token();
                self.expect_token(TokenType::OpenParen)?;
                self.forward_token();

                let starting_inside_paren = self.cur;

                if self.expect_def().is_ok() {
                    self.cur = starting_inside_paren;
                    return self.expect_for_c();
                } else {
                    self.cur = starting_inside_paren;
                }

                if self.expect_ident().is_ok() {
                    if self.expect_token(TokenType::KeywordIn).is_ok() {
                        self.cur = starting_inside_paren;
                        return self.expect_for_each();
                    } else {
                        self.cur = starting_inside_paren;
                        return self.expect_for_each_implicit_iterator();
                    }
                } else {
                    self.cur = starting_inside_paren;
                }

                if self.expect_expr().is_ok() {
                    self.cur = starting_inside_paren;

                    return self.expect_for_each_implicit_iterator();
                } else {
                    self.cur = starting_inside_paren;
                    return Err(self.wrap_err(format!("invalid for syntax.")));
                }
            }
            TokenType::KeywordReturn => {
                self.forward_token();
                let expr = self.expect_expr()?;
                self.forward_token();
                Ok(self.new_node(self.get_id(), AstNodeData::Return(expr.id), Type::NoType, self.current_line(), self.current_col()))
            }

            TokenType::KeywordGoto => {
                //TODO
                return Err(anyhow!("goto is not implemented yet"));
            }

            TokenType::KeywordContinue => {
                return Ok(self.new_node(self.get_id(), AstNodeData::Continue, Type::NoType, self.current_line(), self.current_col()));
            }

            TokenType::KeywordSwitch => {
                //TODO
                return Err(anyhow!("Switch is not implemented yet"));
            }

            TokenType::KeywordBreak => {
                return Ok(self.new_node(self.get_id(), AstNodeData::Break, Type::NoType, self.current_line(), self.current_col()));
            }

            _ => {
                return Err(self.wrap_err(format!("expecting statement found: {:?}", self.current_token())));
            }
        }
    }

    pub fn get_ast(mut self) -> Result<Ast> {
        let block_id = self.expect_block()?;      
        self.ast.top_level = block_id;
        Ok(self.ast)
    }
}

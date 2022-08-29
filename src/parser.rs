#![allow(clippy::needless_return)]
use crate::ast::{
    Ast, AstNodeType, AstNode, AstNodeData, NodeID, Scope, ScopeID, ScopeType, AstOperation, AstTag,
};
use crate::lexer::Token;
use crate::lexer::TokenType;
use crate::lexer::Tokenizer;
use crate::context::Context;
use anyhow::{anyhow, Result};
use rand::distributions::{Alphanumeric, DistString};
const ID_LENGTH: usize = 24;

#[derive(Debug)]
pub struct Parser<'a> {
    filename: String,
    src: String,
    tokens: Vec<Token>,
    cur: usize,
    node_counter: i64,
    pub context: &'a mut Context,
}

// Every parser function should parse until the last token in it's scope and then move cursor to the next token. so every parse function moves the cursor to the next.
impl<'a> Parser<'a> {
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
    fn new_node(&mut self, data: AstNodeData, type_annotation: AstNodeType, start_line: usize, start_col: usize) -> AstNode {
        let id = Alphanumeric.sample_string(&mut rand::thread_rng(), ID_LENGTH);
        self.node_counter += 1;
        let node = AstNode {
            id,
            data,
            infered_type: type_annotation,
            tags: vec![],
            scope: Default::default(),
            line: start_line, col: start_col,
            filename: self.filename.clone()
        };
        self.context.add_node(node.clone()).unwrap();
        self.context
            .add_scope_to_node(&node.id, self.context.top_of_scope_stack());
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
        compiler: &'a mut Context,
    ) -> Result<Self> {
        Ok(Self {
            filename,
            src,
            tokens,
            cur: 0,
            node_counter: 0,
            context: compiler,
        })
    }
    fn expect_ident(&mut self) -> Result<AstNode> {
        match self.current_token().ty {
            TokenType::Ident => {
                let name =
                    self.src[self.current_token().loc.0..=self.current_token().loc.1].to_string();
                let node = self.new_node(AstNodeData::Ident(name), AstNodeType::Unknown, self.current_token().line, self.current_token().col);
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
                    AstNodeData::StringLiteral(literal.to_string()),
                    AstNodeType::String,
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
        let mut dest = self.expect_lhs()?;
        match self.current_token().ty {
            TokenType::Equal => {
                self.forward_token();
                let rhs = self.expect_expr()?;
                let infered_ty = rhs.infered_type.clone();
                // self.context.add_type_inference(&dest.id, infered_ty);
                let node = self.new_node(AstNodeData::Assign{lhs: dest.id, rhs: rhs.id}, AstNodeType::NoType, self.current_token().line, self.current_token().col);
                Ok(node)
            }
            TokenType::DoubleColon => {
                self.forward_token();
                let rhs = self.expect_expr()?;
                let infered_ty = rhs.infered_type.clone();
                self.context.add_type_inference(&dest.id, infered_ty);
                let node = self.new_node(
                    AstNodeData::Def {
                        mutable: false,
                        name: dest.id,
                        expr: rhs.id,
                    },
                    AstNodeType::NoType, self.current_token().line, self.current_token().col,
                );
                Ok(node)
            }

            TokenType::Colon => {
                self.forward_token();
                let ty = self.expect_expr()?;

                self.context.add_type_inference(&dest.id, AstNodeType::new(&ty, &self.context)?);
                if self.current_token().ty != TokenType::Equal
                    && self.current_token().ty != TokenType::Colon
                {
                    let decl_node = self.new_node(
                        AstNodeData::Decl(dest.id.clone()),
                        AstNodeType::new(&ty, &self.context)?, self.current_line(), self.current_col()
                    );
                    if self.current_token().ty == TokenType::ForeignDirective {
                        self.context.add_tag(&decl_node.id, AstTag::Foreign);
                        self.forward_token();
                    }
                    return Ok(decl_node);
                }
                let mutable = self.current_token().ty == TokenType::Equal;
                self.forward_token();
                let rhs = self.expect_expr()?;
                let infered_ty = rhs.infered_type.clone();
                self.context.add_type_inference(&dest.id, infered_ty);
                let node = self.new_node(
                    AstNodeData::Def{
                        mutable,
                        name: dest.id,
                        expr: rhs.id,
                    },
                    AstNodeType::NoType, self.current_token().line, self.current_token().col
                );
                Ok(node)
            }

            TokenType::ColonEqual => {
                self.forward_token();
                let rhs = self.expect_expr()?;
                let infered_ty = rhs.infered_type.clone();
                dest.infered_type = infered_ty.clone();
                self.context.add_type_inference(&dest.id, infered_ty);
                let node = self.new_node(
                    AstNodeData::Def{
                        mutable: true,
                        name: dest.id,
                        expr: rhs.id,
                    },
                    AstNodeType::NoType, self.current_token().line, self.current_token().col
                );
                Ok(node)
            }
            _ => {
                return Err(self.err_uexpected_token(self.current_token().clone().ty));
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

    fn current_line(&self) -> usize {
        return self.current_token().line;
    }

    fn current_col(&self) -> usize {
        return self.current_token().col;
    }
    fn expect_fn_def(&mut self) -> Result<AstNode> {
        let fn_scope = self.context
            .add_scope(ScopeType::Function, self.cur as isize, -1 as isize);
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
            let ty = self.expect_expr()?;

            let arg = self.new_node(AstNodeData::Decl(name.id.clone()), AstNodeType::new(&ty, self.context)?, name.line, name.col);
            args.push(arg.id);
        }
        let mut ret_ty: Option<AstNode> = None;
        if self.current_token().ty != TokenType::OpenBrace {
            ret_ty = Some(self.expect_expr()?);
        } else {
            ret_ty = Some(self.new_node(AstNodeData::VoidTy, AstNodeType::Void, self.current_line(), self.current_col()));
        }
        let ret_ty = ret_ty.unwrap();
        
        if self.current_token().ty != TokenType::OpenBrace {
            self.context.remove_from_scope_stack();
            // fn type only
            let proto_ty = AstNodeType::make_fn_signature(self.context, &args, &ret_ty)?;
            let node = self.new_node(AstNodeData::FnType {
                args,
                ret: ret_ty.id,
            }, proto_ty, self.current_line(), self.current_col());
            return Ok(node);
        }
        let fn_scope = self.context.top_of_scope_stack();
        let body = self.expect_block(ScopeType::Function)?;
        let proto_ty = AstNodeType::make_fn_signature(self.context, &args, &ret_ty)?;
        let sign = self.new_node(AstNodeData::FnType { args, ret: ret_ty.id }, proto_ty.clone(),self.current_line(), self.current_col()); //@Bug: line & col are wrong
        let fn_def = self.new_node(AstNodeData::FnDef{ sign: sign.id, body: body }, proto_ty, self.current_line(), self.current_col());
        self.context.set_scope_owner(fn_scope, fn_def.id.clone());
        Ok(fn_def)
    }

    fn expect_fn_call(&mut self) -> Result<AstNode> {
        let name = self.expect_lhs()?;
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
        Ok(self.new_node(AstNodeData::FnCall{fn_name: name.id, args}, AstNodeType::Unknown,self.current_line(), self.current_col()))
    }
    fn expect_expr_container_field(&mut self) -> Result<AstNode> {
        let container = self.expect_expr_initialize()?;
        match self.current_token().ty {
            TokenType::Dot => {
                self.forward_token();
                let field = self.expect_expr()?;
                let f = self.context.nodes.get_mut(&field.id).unwrap();
                f.tags.push(AstTag::IsUsedInNamespaceAccess);
                return Ok(self.new_node(AstNodeData::NamespaceAccess{ namespace: container.id, field: field.id }, AstNodeType::Unknown, self.current_line(), self.current_col()));
            }
            _ => {
                return Ok(container);
            }
        }
    }
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
                            let mut name = self.context.nodes.get_mut(&name.id.clone()).unwrap();
                            name.infered_type = value.infered_type.clone();
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
                        return Ok(self.new_node(
                            AstNodeData::Initialize{ty: ty.id.clone(), fields},
                            AstNodeType::Initialize(Box::new(AstNodeType::new(&ty, self.context)?)),
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

                        return Ok(self.new_node(
                            AstNodeData::InitializeArray(Some(ty.id), fields),
                            AstNodeType::Unknown, self.current_line(), self.current_col()
                        ));
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

    fn expect_expr_exact_expr(&mut self) -> Result<AstNode> {
        match self.current_token().ty {
            TokenType::UnsignedInt => {
                self.forward_token();
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0..=src_range.loc.1];
                let literal = literal.parse::<u64>()?;
                Ok(self.new_node(AstNodeData::Unsigned(literal), AstNodeType::UnsignedInt(64), self.current_line(), self.current_col()))
            }
            TokenType::Float => {
                self.forward_token();
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0..=src_range.loc.1];
                let literal = literal.parse::<f64>()?;
                Ok(self.new_node(AstNodeData::Float(literal), AstNodeType::Float(64), self.current_line(), self.current_col()))
            }
            TokenType::StringLiteral => {
                self.forward_token();
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0..=src_range.loc.1];
                Ok(self.new_node(
                    AstNodeData::StringLiteral(literal.to_string()),
                    AstNodeType::String, self.current_line(), self.current_col()
                ))
            }
            TokenType::CVarArgsDirective => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::CVarArgs, AstNodeType::CVarArgs, self.current_line(), self.current_col()))
            }
            TokenType::CString => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::CString, AstNodeType::CString, self.current_line(), self.current_col()))
            }
            TokenType::KeywordTrue => {
                self.forward_token();
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0..=src_range.loc.1];
                Ok(self.new_node(AstNodeData::Bool(literal == "true"), AstNodeType::Bool, self.current_line(), self.current_col()))
            }
            TokenType::KeywordFalse => {
                self.forward_token();
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0..=src_range.loc.1];
                Ok(self.new_node(AstNodeData::Bool(literal == "true"), AstNodeType::Bool, self.current_line(), self.current_col()))
            }
            TokenType::Char => {
                self.forward_token();
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0 + 1..=src_range.loc.1 - 1];
                Ok(self.new_node(
                    AstNodeData::Char(literal.chars().next().unwrap()),
                    AstNodeType::Char, self.current_line(), self.current_col()
                ))
            }
            TokenType::KeywordStruct => {
                self.context.add_scope(ScopeType::Struct, self.cur as isize, -1 as isize);
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
                    let ty = self.expect_expr()?;
                    self.context.add_type_inference(&name.id, AstNodeType::new(&ty, self.context)?);
                    let field = self.new_node(AstNodeData::Decl(name.id), AstNodeType::new(&ty, self.context)?, self.current_line(), self.current_col());
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
                let current_scope_idx = self.context.top_of_scope_stack() as usize;
                let current_scope = &mut self.context.scopes[current_scope_idx];
                self.context.remove_from_scope_stack();
                let node = self.new_node(AstNodeData::Struct(fields), AstNodeType::Struct, self.current_line(), self.current_col());
                self.context.set_scope_owner(current_scope_idx as isize, node.id.clone());
                Ok(node)
            }
            TokenType::KeywordEnum => {
                self.forward_token();
                self.expect_token(TokenType::OpenBrace)?;
                self.forward_token();
                self.context.add_scope(ScopeType::Enum, self.cur as isize, -1 as isize);
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
                    self.context
                        .add_type_inference(&name.id, AstNodeType::UnsignedInt(64));
                    let variant = self.new_node(AstNodeData::Decl(name.id), AstNodeType::UnsignedInt(64), self.current_line(), self.current_col());

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
                let current_scope_idx = self.context.top_of_scope_stack() as usize;
                let current_scope = &mut self.context.scopes[current_scope_idx];
                self.context.remove_from_scope_stack();
                let node = self.new_node(AstNodeData::Enum(variants), AstNodeType::Enum, self.current_line(), self.current_col());
                self.context.set_scope_owner(current_scope_idx as isize, node.id.clone());
                Ok(node)
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

            TokenType::OpenBracket => {
                // array type
                self.forward_token();
                let len = self.expect_expr()?;
                self.expect_token(TokenType::CloseBracket)?;
                self.forward_token();
                let ty = self.expect_expr_exact_expr()?;
                return Ok(self.new_node(
                    AstNodeData::ArrayTy{length: len.extract_uint(), elem_ty: AstNodeType::new(&ty, self.context)? },
                    AstNodeType::Unknown, self.current_line(), self.current_col()
                ));
            }

            TokenType::Asterix | TokenType::DoubleLeftAngle => {
                self.forward_token();
                let expr = self.expect_expr()?;
                return Ok(self.new_node(AstNodeData::Deref(expr.id), expr.infered_type, self.current_line(), self.current_col()));
            }
            TokenType::Ampersand | TokenType::DoubleRightAngle => {
                self.forward_token();
                let expr = self.expect_expr()?;
                return Ok(self.new_node(
                    AstNodeData::PointerTo(expr.id),
                    AstNodeType::Pointer(Box::new(expr.infered_type)),
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
            TokenType::KeywordVoid => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::VoidTy, AstNodeType::Void, self.current_line(), self.current_col()))
            }
            TokenType::KeywordInt => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::IntTy(64), AstNodeType::SignedInt(64), self.current_line(), self.current_col()))
            }
            TokenType::KeywordInt8 => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::IntTy(8), AstNodeType::SignedInt(8), self.current_line(), self.current_col()))
            }
            TokenType::KeywordInt16 => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::IntTy(16), AstNodeType::SignedInt(16), self.current_line(), self.current_col()))
            }
            TokenType::KeywordInt32 => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::IntTy(32), AstNodeType::SignedInt(32), self.current_line(), self.current_col()))
            }
            TokenType::KeywordInt64 => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::IntTy(64), AstNodeType::SignedInt(64), self.current_line(), self.current_col()))
            }
            TokenType::KeywordInt128 => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::IntTy(128), AstNodeType::SignedInt(128), self.current_line(), self.current_col()))
            }
            TokenType::KeywordUint => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::UintTy(64), AstNodeType::UnsignedInt(64), self.current_line(), self.current_col()))
            }
            TokenType::KeywordUint8 => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::UintTy(8), AstNodeType::UnsignedInt(8), self.current_line(), self.current_col()))
            }
            TokenType::KeywordUint16 => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::UintTy(16), AstNodeType::UnsignedInt(16), self.current_line(), self.current_col()))
            }
            TokenType::KeywordUint32 => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::UintTy(32), AstNodeType::UnsignedInt(32), self.current_line(), self.current_col()))
            }
            TokenType::KeywordUint64 => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::UintTy(64), AstNodeType::UnsignedInt(64), self.current_line(), self.current_col()))
            }
            TokenType::KeywordUint128 => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::UintTy(128), AstNodeType::UnsignedInt(128), self.current_line(), self.current_col()))
            }
            TokenType::KeywordFloat32 => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::FloatTy(32), AstNodeType::Float(32), self.current_line(), self.current_col()))
            }
            TokenType::KeywordFloat64 => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::FloatTy(64), AstNodeType::Float(64), self.current_line(), self.current_col()))
            }
            TokenType::KeywordChar => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::CharTy, AstNodeType::Char, self.current_line(), self.current_col()))
            }
            TokenType::KeywordBool => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::BoolTy, AstNodeType::Bool, self.current_line(), self.current_col()))
            }

            TokenType::KeywordString => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::StringTy, AstNodeType::String, self.current_line(), self.current_col()))
            }

            _ => {
                println!("unknown expr: {:?}", self.current_token());
                unreachable!();
            }
        }
    }

    fn ast_op_from_token_type(&mut self, op: TokenType) -> Result<AstOperation> {
        match op {
            TokenType::GreaterEqual => Ok(AstOperation::GreaterEqual),
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
        let lhs = self.expect_expr_container_field()?;

        match self.current_token().ty {
            TokenType::LeftAngle
            | TokenType::RightAngle
            | TokenType::LessEqual
            | TokenType::GreaterEqual
            | TokenType::DoubleEqual
            | TokenType::DoubleAmpersand
            | TokenType::DoublePipe
            | TokenType::NotEqual => {
                let op = self.ast_op_from_token_type(self.current_token().ty.clone())?;
                self.forward_token();
                let rhs = self.expect_expr_container_field()?;
                Ok(self.new_node(
                    AstNodeData::BinaryOperation{ operation: op, left: lhs.id, right:rhs.id },
                    AstNodeType::Bool, self.current_line(), self.current_col()
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
                let mut infered_ty = lhs.infered_type.clone();
                if !lhs.infered_type.is_unknown()
                    && !rhs.infered_type.is_unknown()
                    && lhs.infered_type != rhs.infered_type
                {
                    return Err(self.wrap_err(format!(
                        "both sides of assignment should be same type, but lhs is: {:?} and rhs is: {:?}", 
                            lhs.infered_type,
                            rhs.infered_type,
                            )));
                }
                if rhs.infered_type != AstNodeType::Unknown {
                    infered_ty = rhs.infered_type.clone();
                }
                if lhs.infered_type.is_unknown() {
                    lhs.infered_type = rhs.infered_type.clone();
                }
                Ok(self.new_node(
                    AstNodeData::BinaryOperation{ operation: AstOperation::Multiply, left: lhs.id, right:rhs.id },
                    AstNodeType::Bool, self.current_line(), self.current_col()
                ))
            }
            TokenType::Percent => {
                self.forward_token();
                let rhs = self.expect_expr_binary_operations()?;
                let mut infered_ty = lhs.infered_type.clone();
                if !lhs.infered_type.is_unknown()
                    && !rhs.infered_type.is_unknown()
                    && lhs.infered_type != rhs.infered_type
                {
                    return Err(self.wrap_err(format!(
                        "both sides of assignment should be same type, but lhs is: {:?} and rhs is: {:?}", 
                            lhs.infered_type,
                            rhs.infered_type,
                            )));
                }
                if rhs.infered_type != AstNodeType::Unknown {
                    infered_ty = rhs.infered_type.clone();
                }
                if lhs.infered_type.is_unknown() {
                    lhs.infered_type = rhs.infered_type.clone();
                }
                Ok(self.new_node(
                    AstNodeData::BinaryOperation{ operation: AstOperation::Modulu, left: lhs.id, right:rhs.id },
                    AstNodeType::Bool, self.current_line(), self.current_col()
                ))
            }
            TokenType::ForwardSlash => {
                self.forward_token();
                let rhs = self.expect_expr_binary_operations()?;
                let mut infered_ty = lhs.infered_type.clone();
                if !lhs.infered_type.is_unknown()
                    && !rhs.infered_type.is_unknown()
                    && lhs.infered_type != rhs.infered_type
                {
                    return Err(self.wrap_err(format!(
                        "both sides of assignment should be same type, but lhs is: {:?} and rhs is: {:?}", 
                            lhs.infered_type,
                            rhs.infered_type,
                            )));
                }
                if rhs.infered_type != AstNodeType::Unknown {
                    infered_ty = rhs.infered_type.clone();
                }
                if lhs.infered_type.is_unknown() {
                    lhs.infered_type = rhs.infered_type.clone();
                }
               Ok(self.new_node(
                    AstNodeData::BinaryOperation{ operation: AstOperation::Multiply, left: lhs.id, right:rhs.id },
                    AstNodeType::Bool, self.current_line(), self.current_col()
                ))
            }
            _ => Ok(lhs),
        }
    }

    fn expect_expr(&mut self) -> Result<AstNode> {
        let mut lhs = self.expect_expr_mul_div_mod()?;

        match self.current_token().ty {
            TokenType::Minus => {
                self.forward_token();
                let rhs = self.expect_expr_mul_div_mod()?;
                let mut infered_ty = lhs.infered_type.clone();
                if !lhs.infered_type.is_unknown()
                    && !rhs.infered_type.is_unknown()
                    && lhs.infered_type != rhs.infered_type
                {
                    return Err(self.wrap_err(format!(
                        "both sides of assignment should be same type, but lhs is: {:?} and rhs is: {:?}", 
                            lhs.infered_type,
                            rhs.infered_type,
                            )));
                }
                if rhs.infered_type != AstNodeType::Unknown {
                    infered_ty = rhs.infered_type.clone();
                }
                if lhs.infered_type.is_unknown() {
                    lhs.infered_type = rhs.infered_type.clone();
                }
                Ok(self.new_node(
                    AstNodeData::BinaryOperation{ operation: AstOperation::Subtract, left: lhs.id, right:rhs.id },
                    AstNodeType::Bool, self.current_line(), self.current_col()
                ))
            }
            TokenType::Plus => {
                self.forward_token();
                let rhs = self.expect_expr_mul_div_mod()?;
                let mut infered_ty = lhs.infered_type.clone();
                if !lhs.infered_type.is_unknown()
                    && !rhs.infered_type.is_unknown()
                    && lhs.infered_type != rhs.infered_type
                {
                    return Err(self.wrap_err(format!(
                        "both sides of assignment should be same type, but lhs is: {:?} and rhs is: {:?}", 
                            lhs.infered_type,
                            rhs.infered_type,
                            )));
                }
                if rhs.infered_type != AstNodeType::Unknown {
                    infered_ty = rhs.infered_type.clone();
                }
                if lhs.infered_type.is_unknown() {
                    lhs.infered_type = rhs.infered_type.clone();
                }
                Ok(self.new_node(
                    AstNodeData::BinaryOperation{ operation: AstOperation::Sum, left: lhs.id, right:rhs.id },
                    AstNodeType::Bool, self.current_line(), self.current_col()
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

    fn expect_block(&mut self, scope_ty: ScopeType) -> Result<NodeID> {
        if self.current_token().ty == TokenType::OpenBrace {
            self.forward_token()
        }
        let mut stmts = Vec::<NodeID>::new();
        loop {
            if self.current_token().ty == TokenType::CloseBrace || self.cur >= self.tokens.len() {
                break;
            }
            let stmt = self.expect_stmt()?;
            self.if_semicolon_forward();
            stmts.push(stmt.id);
        }
        
        let current_scope_idx = self.context.top_of_scope_stack() as usize;
        let current_scope = &mut self.context.scopes[current_scope_idx];
        current_scope.end = self.cur as isize;

        self.context.remove_from_scope_stack();
        self.forward_token();
        let namespace_node = self.new_node(AstNodeData::Namespace(stmts), AstNodeType::NoType, self.current_line(), self.current_col()); 
        Ok(namespace_node.id)
    }

    fn if_semicolon_forward(&mut self) {
        if self.current_token().ty == TokenType::SemiColon {
            self.forward_token();
        }
    }

    fn expect_for_c(&mut self) -> Result<AstNode> {
        let for_scope = self.context
            .add_scope(ScopeType::For, self.cur as isize, -1 as isize);
        let start = self.expect_def()?;
        self.expect_semicolon_and_forward()?;
        let cond = self.expect_expr()?;
        self.expect_semicolon_and_forward()?;
        let cont = self.expect_stmt()?;
        self.expect_token(TokenType::CloseParen)?;
        self.forward_token();
        let body = self.expect_block(ScopeType::For)?;
        let for_node = self.new_node(
            AstNodeData::For{start: start.id, cond: cond.id, cont: cont.id, body: body},
            AstNodeType::Unknown, self.current_line(), self.current_col()
        );
        self.context.set_scope_owner(for_scope, for_node.id.clone());
        return Ok(for_node);
    }

    // expect_dest will return a node that can be used as a lhs of an assignment.
    fn expect_lhs(&mut self) -> Result<AstNode> {
        match self.current_token().ty {
            TokenType::Ident => {
                self.forward_token();
                if self.current_token().ty == TokenType::Dot {
                    self.backward_token();
                    let cf = self.expect_expr_container_field();
                    return cf;
                } else {
                    self.backward_token();
                    return self.expect_ident();
                }
            }

            _ => {
                return Err(self.wrap_err(format!(
                        "expected some kind of destination for left hand side of an assignment found: {:?}", 
                             self.current_token(),
                            )));
            }
        }
    }
    fn expect_for_each(&mut self) -> Result<AstNode> {
        let for_scope = self.context
            .add_scope(ScopeType::For, self.cur as isize, -1 as isize);
        let iterator = self.expect_ident()?;
        self.forward_token();
        let iterable = self.expect_expr()?;
        self.expect_token(TokenType::CloseParen)?;
        self.forward_token();
        self.expect_token(TokenType::OpenBrace)?;
        let body = self.expect_block(ScopeType::ForIn)?;
        let node = self.new_node(
            AstNodeData::ForIn{iterator: iterator.id, iterable: iterable.id, body},
            AstNodeType::Unknown, self.current_line(), self.current_col()
        );
        self.context.set_scope_owner(for_scope, node.id.clone());
        return Ok(node);
    }
    fn expect_for_each_implicit_iterator(&mut self) -> Result<AstNode> {
        let for_scope = self.context
            .add_scope(ScopeType::For, self.cur as isize, -1 as isize);
        let iterable = self.expect_expr()?;
        self.expect_token(TokenType::CloseParen)?;
        self.forward_token();
        self.expect_token(TokenType::OpenBrace)?;
        let body = self.expect_block(ScopeType::ForIn)?;
        self.context.push_to_scope_stack(for_scope);
        let iterator = self.new_node(AstNodeData::Ident("it".to_string()), AstNodeType::Unknown, self.current_line(), self.current_col());
        self.context.remove_from_scope_stack();
        let node = self.new_node(
            AstNodeData::ForIn{iterator: iterator.id, iterable: iterable.id, body},
            AstNodeType::Unknown, self.current_line(), self.current_col()
        );
        self.context.set_scope_owner(for_scope, node.id.clone());
        return Ok(node);
    }
    fn expect_if(&mut self) -> Result<AstNode> {
        self.forward_token();
        self.expect_token(TokenType::OpenParen)?;
        self.forward_token();
        let cond = self.expect_expr()?;
        if !cond.infered_type.is_unknown() && cond.infered_type != AstNodeType::Bool {
             return Err(self.wrap_err(format!("if condition should be boolean but given condition is {:?}", cond.infered_type)));
        }
        self.expect_token(TokenType::CloseParen)?;
        self.forward_token();

        self.expect_token(TokenType::OpenBrace)?;
        let then_scope = self.context
            .add_scope(ScopeType::If, self.cur as isize, -1 as isize);

        let then = self.expect_block(ScopeType::If)?;
        if self.current_token().ty == TokenType::KeywordElse {
            self.forward_token();
            if self.current_token().ty == TokenType::KeywordIf {
                let else_if = self.expect_if()?;
                let mut cond_thens = vec![(cond.id, then)];
                for cond_then in else_if.extract_if() {
                    cond_thens.push(cond_then.clone());
                }
                let node = self.new_node(
                    AstNodeData::If{ cases: cond_thens },
                    AstNodeType::Unknown, self.current_line(), self.current_col()
                );
                self.context.set_scope_owner(then_scope, node.id.clone());
                return Ok(node);
            } else if self.current_token().ty == TokenType::OpenBrace {
                let else_scope = self.context
                    .add_scope(ScopeType::Else, self.cur as isize, -1 as isize);
                let _else = self.expect_block(ScopeType::Else)?;
                let default_case = self.new_node(AstNodeData::Bool(true), AstNodeType::Bool, self.current_line(), self.current_col());
                let cases = vec![(cond.id, then), (default_case.id, _else)];
                let node = self.new_node(
                    AstNodeData::If { cases },
                    AstNodeType::Unknown, self.current_line(), self.current_col()
                );
                self.context.set_scope_owner(then_scope, node.id.clone());
                self.context.set_scope_owner(else_scope, node.id.clone());
                return Ok(node);
            } else {
                return Err(self.wrap_err(format!("after else keyword we expect either a code block or if keyword but found: {:?}", self.current_token())));
            }
        }
        let node = self.new_node(
            AstNodeData::If{
                cases: vec![(cond.id, then)],
            },
            AstNodeType::Unknown, self.current_line(), self.current_col()
        );
        self.context.set_scope_owner(then_scope, node.id.clone());
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
                    self.new_node(AstNodeData::Load(literal.to_string()), AstNodeType::NoType, self.current_line(), self.current_col());
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
                let top = self.new_node(
                    AstNodeData::CompilerFlags(path.to_string()),
                    AstNodeType::NoType, self.current_line(), self.current_col()
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
                    self.new_node(AstNodeData::Host(literal.to_string()), AstNodeType::NoType, self.current_line(), self.current_col());
                Ok(top)
            }
            TokenType::Ident => {
                self.forward_token();
                match self.current_token().ty {
                    TokenType::DoubleColon
                    | TokenType::Colon
                    | TokenType::Equal
                    | TokenType::ColonEqual
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
                        let mut lhs = self.expect_lhs()?;

                        self.forward_token();
                        let rhs = self.expect_expr()?;
                        lhs.infered_type = rhs.infered_type.clone();
                        let infered_ty = rhs.infered_type;
                        let inner = self
                            .new_node(AstNodeData::BinaryOperation {operation: AstOperation::Sum, left: lhs.id.clone(), right:rhs.id }, infered_ty, self.current_line(), self.current_col());

                        return Ok(self
                            .new_node(AstNodeData::Assign{lhs: lhs.id, rhs: inner.id}, AstNodeType::NoType, self.current_line(), self.current_col()));
                    }
                    TokenType::MinusEqual => {
                        self.backward_token();
                        let mut lhs = self.expect_lhs()?;
                        self.forward_token();

                        let rhs = self.expect_expr()?;
                        lhs.infered_type = rhs.infered_type.clone();

                        let infered_ty = rhs.infered_type;
                        let inner = self
                            .new_node(AstNodeData::BinaryOperation {operation: AstOperation::Subtract, left: lhs.id.clone(), right:rhs.id }, infered_ty, self.current_line(), self.current_col());
                        return Ok(self
                            .new_node(AstNodeData::Assign{lhs: lhs.id, rhs: inner.id}, AstNodeType::NoType, self.current_line(), self.current_col()));
                    }
                    TokenType::ModEqual => {
                        self.backward_token();
                        let mut lhs = self.expect_lhs()?;
                        self.forward_token();

                        let rhs = self.expect_expr()?;
                        lhs.infered_type = rhs.infered_type.clone();

                        let infered_ty = rhs.infered_type;
                        let inner = self
                            .new_node(AstNodeData::BinaryOperation {operation: AstOperation::Modulu, left: lhs.id.clone(), right:rhs.id }, infered_ty, self.current_line(), self.current_col());
                        return Ok(self
                            .new_node(AstNodeData::Assign{lhs: lhs.id, rhs: inner.id}, AstNodeType::NoType, self.current_line(), self.current_col()));
                    }
                    TokenType::MulEqual => {
                        self.backward_token();
                        let mut lhs = self.expect_lhs()?;
                        self.forward_token();

                        let rhs = self.expect_expr()?;
                        lhs.infered_type = rhs.infered_type.clone();

                        let infered_ty = rhs.infered_type;
                        let inner = self
                            .new_node(AstNodeData::BinaryOperation {operation: AstOperation::Multiply, left: lhs.id.clone(), right:rhs.id }, infered_ty, self.current_line(), self.current_col());
                        return Ok(self
                            .new_node(AstNodeData::Assign{lhs: lhs.id, rhs: inner.id}, AstNodeType::NoType, self.current_line(), self.current_col()));
                    }
                    TokenType::DivEqual => {
                        self.backward_token();
                        let mut lhs = self.expect_lhs()?;
                        self.forward_token();

                        let rhs = self.expect_expr()?;
                        lhs.infered_type = rhs.infered_type.clone();

                        let infered_ty = rhs.infered_type;
                        let inner = self
                            .new_node(AstNodeData::BinaryOperation {operation: AstOperation::Divide, left: lhs.id.clone(), right: rhs.id }, infered_ty, self.current_line(), self.current_col());

                        return Ok(self
                            .new_node(AstNodeData::Assign{lhs: lhs.id, rhs: inner.id}, AstNodeType::NoType, self.current_line(), self.current_col()));
                    }
                    TokenType::DoublePlus => {
                        self.backward_token();
                        let mut lhs = self.expect_lhs()?;
                        self.forward_token();
                        let rhs = self.new_node(AstNodeData::Unsigned(1), AstNodeType::UnsignedInt(64), self.current_line(), self.current_col());
                        lhs.infered_type = rhs.infered_type.clone();

                        let infered_ty = rhs.infered_type;
                        let inner = self
                            .new_node(AstNodeData::BinaryOperation {operation: AstOperation::Sum, left: lhs.id.clone(), right:rhs.id }, infered_ty, self.current_line(), self.current_col());
                        return Ok(self
                            .new_node(AstNodeData::Assign{lhs: lhs.id, rhs: inner.id}, AstNodeType::NoType, self.current_line(), self.current_col()));
                    }
                    TokenType::DoubleMinus => {
                        self.backward_token();
                        let mut lhs = self.expect_lhs()?;
                        self.forward_token();

                        let rhs = self.new_node(AstNodeData::Unsigned(1), AstNodeType::UnsignedInt(64), self.current_line(), self.current_col());
                        lhs.infered_type = rhs.infered_type.clone();

                        let infered_ty = rhs.infered_type;
                        let inner = self
                            .new_node(AstNodeData::BinaryOperation {operation: AstOperation::Sum, left: lhs.id.clone(), right: rhs.id }, infered_ty, self.current_line(), self.current_col());
                        return Ok(self
                            .new_node(AstNodeData::Assign{lhs: lhs.id, rhs: inner.id}, AstNodeType::NoType, self.current_line(), self.current_col()));
                    }
                    _ => {
                        return Err(self.wrap_err(format!("expecting ident statement found: {:?}", self.current_token())));
                    }
                }
            }

            TokenType::Asterix => {
                self.forward_token();
                let deref = self.expect_expr()?;
                let deref = self.new_node(AstNodeData::Deref(deref.id), AstNodeType::Unknown, self.current_line(), self.current_col());
                self.expect_token(TokenType::Equal)?;
                self.forward_token();
                let expr = self.expect_expr()?;

                let node =
                    self.new_node(AstNodeData::Assign{lhs: deref.id, rhs: expr.id}, AstNodeType::NoType, self.current_line(), self.current_col());

                return Ok(node);
            }

            TokenType::KeywordIf => {
                return self.expect_if();
            }
            TokenType::KeywordWhile => {
                self.forward_token();
                let while_scope = self.context
                    .add_scope(ScopeType::While, self.cur as isize, -1 as isize);
                self.expect_token(TokenType::OpenParen)?;
                self.forward_token();
                let cond = self.expect_expr()?;
                if !cond.infered_type.is_unknown() && cond.infered_type != AstNodeType::Bool {
                    return Err(self.wrap_err(format!("while condition should be boolean but given condition is infered to be {:?}", cond.infered_type)));
                }
                self.forward_token();
                self.expect_token(TokenType::OpenBrace)?;
                let body = self.expect_block(ScopeType::While)?;
                let node = self.new_node(AstNodeData::While{cond: cond.id, body}, AstNodeType::Unknown, self.current_line(), self.current_col());
                self.context.set_scope_owner(while_scope, node.id.clone());
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

                    unreachable!();
                }
            }
            TokenType::KeywordReturn => {
                self.forward_token();
                let expr = self.expect_expr()?;
                self.forward_token();
                Ok(self.new_node(AstNodeData::Return(expr.id), AstNodeType::NoType, self.current_line(), self.current_col()))
            }

            TokenType::KeywordGoto => {
                //TODO
                return Err(anyhow!("goto is not implemented yet"));
            }

            TokenType::KeywordContinue => {
                return Ok(self.new_node(AstNodeData::Continue, AstNodeType::NoType, self.current_line(), self.current_col()));
            }

            TokenType::KeywordSwitch => {
                //TODO
                return Err(anyhow!("Switch is not implemented yet"));
            }

            TokenType::KeywordBreak => {
                return Ok(self.new_node(AstNodeData::Break, AstNodeType::NoType, self.current_line(), self.current_col()));
            }

            _ => {
                return Err(self.wrap_err(format!("expecting statement found: {:?}", self.current_token())));
            }
        }
    }

    pub fn get_ast(mut self) -> Result<Ast> {
        let file_scope = self.context
            .add_scope(ScopeType::File(self.filename.clone()), -1, -1);
        let block_id = self.expect_block(ScopeType::File(self.filename.clone()))?;         
        self.context.set_scope_owner(file_scope, block_id.clone());        
        Ast::new(
            self.filename,
            self.src,
            self.tokens,
            block_id,
            self.context,
        )
    }
}

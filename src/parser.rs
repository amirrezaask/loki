#![allow(clippy::needless_return)]
use crate::ast::{AstDef, AstNode, AstNodeData, Ast, SymbolTable, AstContainerField, AstNodeType, AstFnSignature, AstFnDef, AstOperation, AstBinaryOperation, AstCaseBlock, AstTag};
use crate::lexer::Token;
use crate::lexer::Tokenizer;
use crate::lexer::TokenType;
use anyhow::{anyhow, Result};

#[derive(Debug, Default)]
pub struct Parser {
    filename: String,
    src: String,
    tokens: Vec<Token>,
    cur: usize,
    pub node_counter: i64,
}
// Every parser function should parse until the last token in it's scope and then move cursor to the next token. so every parse function moves the cursor to the next.
impl Parser {
    fn err_uexpected(&self, what: TokenType) -> anyhow::Error {
        anyhow!(
            "Expected {:?}, found {:?} at \"{}\"",
            what,
            self.current_token().ty,
            self.src[self.tokens[self.cur].loc.0..=self.tokens[self.cur].loc.1].to_string(),
        )
    }
    fn new_node(&mut self, data: AstNodeData, type_annotation: AstNodeType, scope: Vec<u64>) -> AstNode {
        let node = AstNode {
            id: format!("{}_{}", self.filename, self.node_counter),
            data, infered_type: type_annotation, tags: vec![]
        };
        self.node_counter += 1;
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
    pub fn new_with_tokens(filename: String ,src: String, tokens: Vec<Token>) -> Result<Self> {
        //        println!("tokens: {:?}", tokens);
        Ok(Self {
            filename,
            src,
            tokens,
            cur: 0,
            node_counter: 0,
        })
    }
    fn new(filename: String, src: &'static str) -> Result<Self> {
        let mut tokenizer = Tokenizer::new(src);
        let mut tokens = Vec::<Token>::new();

        loop {
            let tok = tokenizer.next()?;

            match tok.ty {
                TokenType::EOF => {
                    break;
                }
                _ => {
                    tokens.push(tok);
                }
            }
        }
        Ok(Self {
            filename,
            src: src.to_string(),
            tokens,
            cur: 0,
            node_counter: 0,
        })
    }

    fn expect_ident(&mut self, parent_id: Vec<u64>) -> Result<AstNode> {
        match self.current_token().ty {
            TokenType::Ident => {
                let name = self.src[self.current_token().loc.0..=self.current_token().loc.1].to_string();
                self.forward_token();
                return Ok(self.new_node(AstNodeData::Ident(name), AstNodeType::Unknown, parent_id.clone()));
            }
            _ => {
                return Err(self.err_uexpected(TokenType::Ident));
            }
        }
    }
    fn expect_string_literal(&mut self, parent_id: Vec<u64>) -> Result<AstNode> {
        match self.current_token().ty {
            TokenType::StringLiteral => {
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0..=src_range.loc.1];
                return Ok(self.new_node(AstNodeData::StringLiteral(literal.to_string()), AstNodeType::String, parent_id.clone()));
            }
            _ => {
                return Err(self.err_uexpected(TokenType::StringLiteral));
            }
        }
    }


    fn expect_semicolon_and_forward(&mut self) -> Result<()> {
        self.expect_token(TokenType::SemiColon)?;
        self.forward_token();

        Ok(())
    }
    fn expect_def(&mut self, parent_id: Vec<u64>) -> Result<AstNode> {
        let mut dest = self.expect_lhs(parent_id.clone())?;

        match self.current_token().ty {
            TokenType::Equal => {
                self.forward_token();
                let rhs = self.expect_expr(parent_id.clone())?;
                let infered_ty = rhs.infered_type.clone();
                dest.infered_type = infered_ty.clone();
                Ok(self.new_node(AstNodeData::Assign(Box::new(dest), Box::new(rhs)), infered_ty, parent_id.clone()))
            }
            TokenType::DoubleColon => {
                self.forward_token();
                let rhs = self.expect_expr(parent_id.clone())?;
                let infered_ty = rhs.infered_type.clone();
                dest.infered_type = infered_ty.clone();
                Ok(self.new_node(AstNodeData::Def(AstDef {
                    mutable: false,
                    name: dest.get_ident(),
                    expr: Box::new(rhs),
                }), infered_ty, parent_id.clone()))
            }

            TokenType::Colon => {
                self.forward_token();
                let ty = Some(self.expect_expr(parent_id.clone())?);
                if ty.is_none() {
                    panic!("need a valid type after colon");
                }
                if self.current_token().ty != TokenType::Equal && self.current_token().ty != TokenType::Colon
                {
                    let mut decl_node = self.new_node(AstNodeData::Decl(dest.get_ident()), AstNodeType::new(&ty.unwrap()), parent_id.clone());
                    if self.current_token().ty == TokenType::ForeignDirective {
                        decl_node.tags.push(AstTag::Foreign);
                        self.forward_token();
                    } 
                    return Ok(decl_node);
                    
                }
                let mutable = self.current_token().ty == TokenType::Equal;
                self.forward_token();
                let rhs = self.expect_expr(parent_id.clone())?;
                let infered_ty = rhs.infered_type.clone();
                dest.infered_type = infered_ty.clone();
                Ok(self.new_node(AstNodeData::Def(AstDef {
                    mutable,
                    name: dest.get_ident(),
                    expr: Box::new(rhs),
                }), AstNodeType::new(&ty.unwrap()), parent_id.clone()))
            }

            TokenType::ColonEqual => {
                self.forward_token();
                let rhs = self.expect_expr(parent_id.clone())?;
                let infered_ty = rhs.infered_type.clone();
                dest.infered_type = infered_ty.clone();
                Ok(self.new_node(AstNodeData::Def(AstDef {
                    mutable: true,
                    name: dest.get_ident(),
                    expr: Box::new(rhs),
                }), infered_ty, parent_id.clone()))
            }
            _ => {
                return Err(self.err_uexpected(self.current_token().clone().ty));
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

    fn expect_fn_def(&mut self, parent_id: Vec<u64>) -> Result<AstNode> {
        self.expect_token(TokenType::OpenParen)?;
        let mut args: Vec<AstNode> = vec![];
        self.forward_token();
        loop {
            if self.current_token().ty == TokenType::Comma {
                self.forward_token();
            }
            if self.current_token().ty == TokenType::CloseParen {
                self.forward_token();
                break;
            }
            let name = self.expect_ident(parent_id.clone())?;
            self.expect_token(TokenType::Colon)?;
            self.forward_token();
            let ty = self.expect_expr(parent_id.clone())?;
            args.push(self.new_node(AstNodeData::Ident(name.get_ident()), AstNodeType::new(&ty), parent_id.clone()));
        }

        let ret_ty = self.expect_expr(parent_id.clone())?;
        self.expect_token(TokenType::OpenBrace)?;

        let body = self.expect_block(parent_id.clone())?;
        let proto_ty = AstNodeType::make_fn_signature(&args, &ret_ty);
        let sign = AstFnSignature { args, ret: Box::new(ret_ty) };
        Ok(self.new_node(AstNodeData::FnDef(AstFnDef{ sign, body }), proto_ty, parent_id.clone()))
    }

    fn expect_fn_call(&mut self, parent_id: Vec<u64>) -> Result<AstNode> {
        let name = self.expect_lhs(parent_id.clone())?;
        self.expect_token(TokenType::OpenParen)?;
        let mut args = Vec::<AstNode>::new();

        self.forward_token();
        loop {
            if self.current_token().ty == TokenType::CloseParen {
                self.forward_token();
                break;
            }
            let arg = self.expect_expr(parent_id.clone())?;
            args.push(arg);
            match self.current_token().ty {
                TokenType::Comma => {
                    self.forward_token();
                }

                TokenType::CloseParen => {
                    self.forward_token();
                    break;
                }

                _ => {
                    return Err(self.err_uexpected(TokenType::Comma));
                }
            }
        }
        Ok(self.new_node(AstNodeData::FnCall(Box::new(name), args), AstNodeType::Unknown, parent_id.clone()))
    }
    fn expect_expr_container_field(&mut self, parent_id: Vec<u64>) -> Result<AstNode> {
        let container = self.expect_expr_initialize(parent_id.clone())?;
        match self.current_token().ty {
            TokenType::Dot => {
                self.forward_token();
                let field = self.expect_expr(parent_id.clone())?;
                let cf = AstContainerField {
                    container: Box::new(container),
                    field: Box::new(field),
                    container_is_enum: false,
                };
                return Ok(self.new_node(AstNodeData::ContainerField(cf), AstNodeType::Unknown, parent_id.clone()));
            }
            _ => {
                return Ok(container);
            }
        }
    }
    fn expect_expr_initialize(&mut self, parent_id: Vec<u64>) -> Result<AstNode> {
        let ty = self.expect_expr_exact_expr(parent_id.clone())?;
        match self.current_token().ty {
            TokenType::OpenBrace => match ty.data {
                AstNodeData::IntTy
                | AstNodeData::Int8Ty
                | AstNodeData::Int32Ty
                | AstNodeData::Int64Ty
                | AstNodeData::Int128Ty
                | AstNodeData::UintTy
                | AstNodeData::Uint8Ty
                | AstNodeData::Uint16Ty
                | AstNodeData::Uint32Ty
                | AstNodeData::Uint64Ty
                | AstNodeData::Uint128Ty
                | AstNodeData::CharTy
                | AstNodeData::BoolTy
                | AstNodeData::StringTy
                | AstNodeData::FloatTy
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
                        let mut fields = Vec::<(AstNode, AstNode)>::new();
                        loop {
                            if self.current_token().ty == TokenType::CloseBrace {
                                self.forward_token();
                                break;
                            }

                            let mut name = self.expect_ident(parent_id.clone())?;
                            self.expect_token(TokenType::Equal)?;
                            self.forward_token();
                            let value = self.expect_expr(parent_id.clone())?;
                            name.infered_type = value.infered_type.clone();
                            fields.push((name, value));
                            match self.current_token().ty {
                                TokenType::Comma => {
                                    self.forward_token();
                                }
                                TokenType::CloseBrace => {
                                    self.forward_token();
                                    break;
                                }
                                _ => return Err(self.err_uexpected(TokenType::Comma)),
                            }
                        }
                        return Ok(self.new_node(AstNodeData::Initialize(Box::new(ty.clone()), fields), AstNodeType::Initialize(Box::new( AstNodeType::new(&ty))), parent_id.clone()));
                    } else {
                        let mut fields = Vec::<AstNode>::new();
                        loop {
                            if self.current_token().ty == TokenType::CloseBrace {
                                self.forward_token();
                                break;
                            }

                            let val = self.expect_expr(parent_id.clone())?;
                            fields.push(val);
                            match self.current_token().ty {
                                TokenType::Comma => {
                                    self.forward_token();
                                }
                                TokenType::CloseBrace => {
                                    self.forward_token();
                                    break;
                                }
                                _ => return Err(self.err_uexpected(TokenType::Comma)),
                            }
                        }

                        return Ok(
                            self.new_node(AstNodeData::InitializeArray(Some(Box::new(ty)), fields), AstNodeType::Unknown, parent_id.clone())
                        );
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

    fn expect_expr_exact_expr(&mut self, parent_id: Vec<u64>) -> Result<AstNode> {
        match self.current_token().ty {
            TokenType::UnsignedInt => {
                self.forward_token();
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0..=src_range.loc.1];
                let literal = literal.parse::<u64>()?;
                Ok(self.new_node(AstNodeData::Uint(literal), AstNodeType::UnsignedInt(64), parent_id.clone()))
            }
            TokenType::Float => {
                self.forward_token();
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0..=src_range.loc.1];
                let literal = literal.parse::<f64>()?;
                Ok(self.new_node(AstNodeData::Float(literal), AstNodeType::Float(64), parent_id.clone()))
            }
            TokenType::StringLiteral => {
                self.forward_token();
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0..=src_range.loc.1];
                Ok(self.new_node(AstNodeData::StringLiteral(literal.to_string()), AstNodeType::String, parent_id.clone()))
            }
            TokenType::KeywordTrue => {
                self.forward_token();
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0..=src_range.loc.1];
                Ok(self.new_node(AstNodeData::Bool(literal == "true"), AstNodeType::Bool, parent_id.clone()))
            }
            TokenType::KeywordFalse => {
                self.forward_token();
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0..=src_range.loc.1];
                Ok(self.new_node(AstNodeData::Bool(literal == "true"), AstNodeType::Bool, parent_id.clone()))
            }
            TokenType::Char => {
                self.forward_token();
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0+1..=src_range.loc.1-1];
                Ok(self.new_node(AstNodeData::Char(literal.chars().nth(0).unwrap()), AstNodeType::Char, parent_id.clone()))
            }
            TokenType::KeywordStruct => {
                self.forward_token();
                self.expect_token(TokenType::OpenBrace)?;
                self.forward_token();
                let mut fields = Vec::<AstNode>::new();
                loop {
                    if self.current_token().ty == TokenType::CloseBrace {
                        self.forward_token();
                        break;
                    }
                    let mut name = self.expect_ident(parent_id.clone())?;

                    self.expect_token(TokenType::Colon)?;
                    self.forward_token();
                    let ty = self.expect_expr(parent_id.clone())?;
                    name.infered_type = AstNodeType::new(&ty);
                    fields.push(self.new_node(AstNodeData::Decl(name.get_ident()), AstNodeType::new(&ty), parent_id.clone()));
                    match self.current_token().ty {
                        TokenType::Comma => {
                            self.forward_token();
                            continue;
                        }
                        TokenType::CloseBrace => {
                            self.forward_token();
                            break;
                        }
                        _ => return Err(self.err_uexpected(TokenType::Comma)),
                    }
                }

                Ok(self.new_node(AstNodeData::Struct(fields), AstNodeType::TypeDefStruct, parent_id.clone()))
            }
            TokenType::KeywordEnum => {
                self.forward_token();
                self.expect_token(TokenType::OpenBrace)?;
                self.forward_token();
                let mut variants = Vec::<(AstNode, Option<AstNode>)>::new();
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

                    let mut variant = self.expect_ident(parent_id.clone())?;
                    variant.infered_type = AstNodeType::UnsignedInt(64);
                    match self.current_token().ty {
                        TokenType::Comma => {
                            variants.push((variant, None));
                            self.forward_token();
                            continue;
                        }
                        TokenType::CloseBrace => {
                            variants.push((variant, None));
                            self.forward_token();
                            break;
                        }
                        _ => {
                            self.expect_token(TokenType::Comma)?;
                        }
                    }
                }

                Ok(self.new_node(AstNodeData::Enum(is_union, variants), AstNodeType::TypeDefEnum, parent_id.clone()))
            }
            TokenType::OpenParen => {
                let before_check_fn_def_cur = self.cur;
                if self.is_fn_def() {
                    self.cur = before_check_fn_def_cur;
                    return Ok(self.expect_fn_def(parent_id.clone())?);
                }
                self.cur = before_check_fn_def_cur;
                let expr = self.expect_expr(parent_id.clone())?;
                self.expect_token(TokenType::CloseParen)?;
                Ok(expr)
            }

            TokenType::OpenBracket => {
                // array type
                self.forward_token();
                let len = self.expect_expr(parent_id.clone())?;
                self.expect_token(TokenType::CloseBracket)?;
                self.forward_token();
                let ty = self.expect_expr_exact_expr(parent_id.clone())?;
                return Ok(self.new_node(AstNodeData::ArrayTy(len.extract_uint(), AstNodeType::new(&ty)), AstNodeType::Unknown, parent_id.clone()));
            }

            TokenType::Asterix => {
                self.forward_token();
                let expr = self.expect_expr(parent_id.clone())?;
                return Ok(self.new_node(AstNodeData::Deref(Box::new(expr.clone())), AstNodeType::Pointer(Box::new(expr.infered_type.clone())), parent_id.clone()));
            }
            TokenType::Ampersand => {
                self.forward_token();
                let expr = self.expect_expr(parent_id.clone())?;

                return Ok(self.new_node(AstNodeData::PointerTo(Box::new(expr.clone())), AstNodeType::Deref(Box::new(expr.infered_type.clone())), parent_id.clone()));
            }
            // Type::Dot => {
            //     self.forward_token();
            //     self.expect_token(Type::OpenBrace)?;
            //     self.forward_token();
            //     let mut fields = Vec::<(Node, Node)>::new();
            //     loop {
            //         if self.current_token().ty == Type::CloseBrace {
            //             self.forward_token();
            //             break;
            //         }
            //         let name = self.expect_ident()?;
            //         self.expect_token(Type::Equal)?;
            //         self.forward_token();
            //         let value = self.expect_expr()?;
            //         fields.push((name, value));
            //         match self.current_token().ty {
            //             Type::Comma => {
            //                 self.forward_token();
            //             }
            //             Type::CloseBrace => {
            //                 self.forward_token();
            //                 break;
            //             }
            //             _ => return Err(self.err_uexpected(Type::Comma)),
            //         }
            //     }

            //     return Ok(self.new_node(NodeData::Initialize(None, fields), AstType::Unknown));
            // }
            TokenType::Ident => {
                self.forward_token();
                match self.current_token().ty {
                    TokenType::OpenParen => {
                        //function call
                        self.backward_token();
                        self.expect_fn_call(parent_id.clone())
                    }
                    _ => {
                        self.backward_token();
                        let name = self.expect_ident(parent_id.clone())?;
                        return Ok(name);
                            
                    },
                }
            }
            TokenType::KeywordVoid => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::VoidTy, AstNodeType::Void, parent_id.clone()))
            }
            TokenType::KeywordInt => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::IntTy, AstNodeType::SignedInt(64), parent_id.clone()))
            }
            TokenType::KeywordInt8 => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::Int8Ty, AstNodeType::SignedInt(8), parent_id.clone()))
            }
            TokenType::KeywordInt16 => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::Int16Ty, AstNodeType::SignedInt(16), parent_id.clone()))
            }
            TokenType::KeywordInt32 => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::Int32Ty, AstNodeType::SignedInt(32), parent_id.clone()))
            }
            TokenType::KeywordInt64 => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::Int64Ty,AstNodeType::SignedInt(64), parent_id.clone()))
            }
            TokenType::KeywordInt128 => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::Int128Ty,AstNodeType::SignedInt(128), parent_id.clone()))
            }
            TokenType::KeywordUint => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::UintTy, AstNodeType::UnsignedInt(64), parent_id.clone()))
            }
            TokenType::KeywordUint8 => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::Uint8Ty, AstNodeType::UnsignedInt(8), parent_id.clone()))
            }
            TokenType::KeywordUint16 => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::Uint16Ty, AstNodeType::UnsignedInt(16), parent_id.clone()))
            }
            TokenType::KeywordUint32 => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::Uint32Ty, AstNodeType::UnsignedInt(32), parent_id.clone()))
            }
            TokenType::KeywordUint64 => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::Uint64Ty, AstNodeType::UnsignedInt(64), parent_id.clone()))
            }
            TokenType::KeywordUint128 => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::Uint128Ty, AstNodeType::UnsignedInt(128), parent_id.clone()))
            }
            TokenType::KeywordFloat32 => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::FloatTy, AstNodeType::Float(32), parent_id.clone()))
            }
            TokenType::KeywordFloat64 => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::FloatTy, AstNodeType::Float(64), parent_id.clone()))
            }
            TokenType::KeywordChar => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::CharTy, AstNodeType::Char, parent_id.clone()))
            }
            TokenType::KeywordBool => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::BoolTy, AstNodeType::Bool, parent_id.clone()))
            }

            TokenType::KeywordString => {
                self.forward_token();
                Ok(self.new_node(AstNodeData::StringTy, AstNodeType::String, parent_id.clone()))
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
            _ => {
                panic!("expected a token that can represent an operation got {:?}", op);
            }
        }
    }

    fn expect_expr_b(&mut self, parent_id: Vec<u64>) -> Result<AstNode> {
        let lhs = self.expect_expr_container_field(parent_id.clone())?;

        match self.current_token().ty {
            TokenType::LeftAngle
                | TokenType::RightAngle
                | TokenType::LessEqual
                | TokenType::GreaterEqual
                | TokenType::DoubleEqual
                | TokenType::NotEqual => {

                    let op = self.ast_op_from_token_type(self.current_token().ty.clone())?;
                    self.forward_token();
                    let rhs = self.expect_expr_container_field(parent_id.clone())?;
                    let binary_operation = AstBinaryOperation { operation: op, left: Box::new(lhs), right: Box::new(rhs) };

                    Ok(self.new_node(AstNodeData::BinaryOperation(binary_operation), AstNodeType::Bool, parent_id.clone()))
            }

            _ => Ok(lhs),
        }
    }
    fn expect_expr_mul_div_mod(&mut self, parent_id: Vec<u64>) -> Result<AstNode> {
        let mut lhs = self.expect_expr_b(parent_id.clone())?;

        match self.current_token().ty {
            TokenType::Asterix => {
                self.forward_token();
                let rhs = self.expect_expr_b(parent_id.clone())?;
                let mut infered_ty = lhs.infered_type.clone(); 
                if !lhs.infered_type.is_unknown() && !rhs.infered_type.is_unknown() && lhs.infered_type != rhs.infered_type {
                    panic!("lhs: {:?} and rhs: {:?} should have same type", lhs, rhs);
                }
                if rhs.infered_type != AstNodeType::Unknown {
                    infered_ty = rhs.infered_type.clone();
                }
                if lhs.infered_type.is_unknown() {
                    lhs.infered_type = rhs.infered_type.clone();
                }
                let binary_operation = AstBinaryOperation { operation: AstOperation::Multiply, left: Box::new(lhs), right: Box::new(rhs) };
                Ok(self.new_node(AstNodeData::BinaryOperation(binary_operation), infered_ty.clone(), parent_id.clone()))
            }
            TokenType::Percent => {
                self.forward_token();
                let rhs = self.expect_expr_b(parent_id.clone())?;
                let mut infered_ty = lhs.infered_type.clone(); 
                if !lhs.infered_type.is_unknown() && !rhs.infered_type.is_unknown() && lhs.infered_type != rhs.infered_type {
                    panic!("lhs: {:?} and rhs: {:?} should have same type", lhs, rhs);
                }
                if rhs.infered_type != AstNodeType::Unknown {
                    infered_ty = rhs.infered_type.clone();
                }         
                if lhs.infered_type.is_unknown() {
                    lhs.infered_type = rhs.infered_type.clone();
                }    
                let binary_operation = AstBinaryOperation { operation: AstOperation::Modulu, left: Box::new(lhs), right: Box::new(rhs) };

                Ok(self.new_node(AstNodeData::BinaryOperation(binary_operation), infered_ty.clone(), parent_id.clone()))
            }
            TokenType::ForwardSlash => {
                self.forward_token();
                let rhs = self.expect_expr_b(parent_id.clone())?;
                let mut infered_ty = lhs.infered_type.clone(); 
                if !lhs.infered_type.is_unknown() && !rhs.infered_type.is_unknown() && lhs.infered_type != rhs.infered_type {
                    panic!("lhs: {:?} and rhs: {:?} should have same type", lhs, rhs);
                }
                if rhs.infered_type != AstNodeType::Unknown {
                    infered_ty = rhs.infered_type.clone();
                }
                if lhs.infered_type.is_unknown() {
                    lhs.infered_type = rhs.infered_type.clone();
                }

                let binary_operation = AstBinaryOperation { operation: AstOperation::Multiply, left: Box::new(lhs), right: Box::new(rhs) };
                Ok(self.new_node(AstNodeData::BinaryOperation(binary_operation), infered_ty.clone(), parent_id.clone()))
            }
            _ => Ok(lhs),
        }
    }

    fn expect_expr(&mut self, parent_id: Vec<u64>) -> Result<AstNode> {
        let mut lhs = self.expect_expr_mul_div_mod(parent_id.clone())?;

        match self.current_token().ty {
            TokenType::Minus => {
                self.forward_token();
                let rhs = self.expect_expr_mul_div_mod(parent_id.clone())?;
                let mut infered_ty = lhs.infered_type.clone(); 
                if !lhs.infered_type.is_unknown() && !rhs.infered_type.is_unknown() && lhs.infered_type != rhs.infered_type {
                    panic!("lhs: {:?} and rhs: {:?} should have same type", lhs, rhs);
                }
                if rhs.infered_type != AstNodeType::Unknown {
                    infered_ty = rhs.infered_type.clone();
                }
                if lhs.infered_type.is_unknown() {
                    lhs.infered_type = rhs.infered_type.clone();
                }
                let binary_operation = AstBinaryOperation { operation: AstOperation::Subtract, left: Box::new(lhs), right: Box::new(rhs) };
                Ok(self.new_node(AstNodeData::BinaryOperation(binary_operation), infered_ty, parent_id.clone()))
            }
            TokenType::Plus => {
                self.forward_token();
                let rhs = self.expect_expr_mul_div_mod(parent_id.clone())?;
                let mut infered_ty = lhs.infered_type.clone(); 
                if !lhs.infered_type.is_unknown() && !rhs.infered_type.is_unknown() && lhs.infered_type != rhs.infered_type {
                    panic!("lhs: {:?} and rhs: {:?} should have same type", lhs, rhs);
                }
                if rhs.infered_type != AstNodeType::Unknown {
                    infered_ty = rhs.infered_type.clone();
                }
                if lhs.infered_type.is_unknown() {
                    lhs.infered_type = rhs.infered_type.clone();
                }
                let binary_operation = AstBinaryOperation { operation: AstOperation::Sum, left: Box::new(lhs), right: Box::new(rhs) };
                Ok(self.new_node(AstNodeData::BinaryOperation(binary_operation), infered_ty, parent_id.clone()))
            }

            _ => Ok(lhs),
        }
    }

    fn expect_token(&mut self, ty: TokenType) -> Result<()> {
        if self.current_token().ty != ty {
            return Err(self.err_uexpected(ty));
        }

        return Ok(());
    }

    fn expect_block(&mut self, parent_id: Vec<u64>) -> Result<Vec<AstNode>> {
        self.expect_token(TokenType::OpenBrace)?;
        self.forward_token();
        let mut stmts = Vec::<AstNode>::new();
        let mut counter = 0;
        loop {
            if self.current_token().ty == TokenType::CloseBrace {
                break;
            }
            let mut new_scope = parent_id.clone();
            new_scope.push(counter);
            let stmt = self.expect_stmt(new_scope)?;
            self.if_semicolon_forward();
            stmts.push(stmt);
            counter += 1;
        }
        self.forward_token();

        Ok(stmts)
    }

    fn if_semicolon_forward(&mut self) {
        if self.current_token().ty == TokenType::SemiColon {
            self.forward_token();
        }
    }

    fn expect_for_c(&mut self, parent_id: Vec<u64>) -> Result<AstNode> {
        let start = self.expect_def(parent_id.clone())?;
        self.expect_semicolon_and_forward()?;
        let cond = self.expect_expr(parent_id.clone())?;
        self.expect_semicolon_and_forward()?;
        let cont = self.expect_stmt(parent_id.clone())?;
        self.expect_token(TokenType::CloseParen)?;
        self.forward_token();
        let body = self.expect_block(parent_id.clone())?;

        return Ok(self.new_node(AstNodeData::For(
            Box::new(start),
            Box::new(cond),
            Box::new(cont),
            body,
        ), AstNodeType::Unknown, parent_id.clone()));
    }
    
    // expect_dest will return a node that can be used as a lhs of an assignment.
    fn expect_lhs(&mut self, parent_id: Vec<u64>) -> Result<AstNode> {
        match self.current_token().ty {
            TokenType::Ident => {
                self.forward_token();
                if self.current_token().ty == TokenType::Dot {
                    self.backward_token();
                    let cf = self.expect_expr_container_field(parent_id.clone());
                    return cf;
                } else {
                    self.backward_token();
                    return self.expect_ident(parent_id.clone());
                }
            }

            _ => {
                panic!("token {:?} cannot be a destination node.", self.current_token());
            }
        }
    }
    fn expect_for_each(&mut self, parent_id: Vec<u64>) -> Result<AstNode> {
        let iterator = self.expect_ident(parent_id.clone())?;
        self.forward_token();
        let iterable = self.expect_expr(parent_id.clone())?;
        self.expect_token(TokenType::CloseParen)?;
        self.forward_token();
        self.expect_token(TokenType::OpenBrace)?;
        let body = self.expect_block(parent_id.clone())?;
        return Ok(self.new_node(AstNodeData::ForIn(
            Some(Box::new(iterator)),
            Box::new(iterable),
            body,
        ), AstNodeType::Unknown, parent_id.clone()));
    }
    fn expect_for_each_implicit_iterator(&mut self, parent_id: Vec<u64>) -> Result<AstNode> {
        let iterable = self.expect_expr(parent_id.clone())?;
        self.expect_token(TokenType::CloseParen)?;
        self.forward_token();
        self.expect_token(TokenType::OpenBrace)?;
        let body = self.expect_block(parent_id.clone())?;
        let iterator = self.new_node(AstNodeData::Ident("it".to_string()), AstNodeType::Unknown, parent_id.clone());
        return Ok(self.new_node(AstNodeData::ForIn(
            Some(Box::new(iterator)),
            Box::new(iterable),
            body,
        ), AstNodeType::Unknown, parent_id.clone())); //TODO: we should handle any expression here now we just handle ident
    }
    fn expect_if(&mut self, parent_id: Vec<u64>) -> Result<AstNode> {
        self.forward_token();
        self.expect_token(TokenType::OpenParen)?;
        self.forward_token();
        let cond = self.expect_expr(parent_id.clone())?;
        if !cond.infered_type.is_unknown() && cond.infered_type != AstNodeType::Bool {
            panic!("if condition should evaluate to boolean but: {:?}", cond);
        }
        self.expect_token(TokenType::CloseParen)?;
        self.forward_token();
        self.expect_token(TokenType::OpenBrace)?;
        let then = self.expect_block(parent_id.clone())?;
        if self.current_token().ty == TokenType::KeywordElse {
            self.forward_token();
            if self.current_token().ty == TokenType::KeywordIf {
                let else_if = self.expect_if(parent_id.clone())?;
                let mut cond_thens = vec![(cond, then)];
                for cond_then in &else_if.extract_if().cases {
                    cond_thens.push(cond_then.clone());
                }
                return Ok(self.new_node(AstNodeData::If(AstCaseBlock {
                    cases: cond_thens,
                }), AstNodeType::Unknown, parent_id))
            } else if self.current_token().ty == TokenType::OpenBrace {
                let _else = self.expect_block(parent_id.clone())?;
                let cases = vec![(cond, then), (self.new_node(AstNodeData::Bool(true), AstNodeType::Bool, parent_id.clone()), _else)];
                return Ok(self.new_node(AstNodeData::If(AstCaseBlock {
                    cases,
                }), AstNodeType::Unknown, parent_id))
           
            } else {
                panic!("after else keyword either if or {{ is accepted: {:?}", self.current_token());
            }
        }
        return Ok(self.new_node(AstNodeData::If(AstCaseBlock {
            cases: vec![(cond, then)],
        }), AstNodeType::Unknown, parent_id))
    }
    fn expect_stmt(&mut self, parent_id: Vec<u64>) -> Result<AstNode> {
        match self.current_token().ty {
            TokenType::Ident => {
                let before_lhs_cur = self.cur;
                let mut lhs = self.expect_lhs(parent_id.clone())?;
                match self.current_token().ty {
                    TokenType::DoubleColon | TokenType::Colon | TokenType::Equal | TokenType::ColonEqual => {
                        self.cur = before_lhs_cur;
                        let def = self.expect_def(parent_id.clone())?;
                        return Ok(def);
                    }
                    TokenType::OpenParen => {
                        self.cur = before_lhs_cur;
                        self.expect_fn_call(parent_id.clone())
                    },
                    TokenType::PlusEqual => {
                        self.forward_token();
                        let rhs = self.expect_expr(parent_id.clone())?;
                        lhs.infered_type = rhs.infered_type.clone();

                        let binary_operation = AstBinaryOperation { operation: AstOperation::Sum, left: Box::new(lhs.clone()), right: Box::new(rhs.clone()) };
                        let infered_ty = rhs.infered_type;
                        let inner =
                            self.new_node(AstNodeData::BinaryOperation(binary_operation), infered_ty, parent_id.clone());
                        return Ok(self.new_node(AstNodeData::Assign(Box::new(lhs), Box::new(inner)), AstNodeType::NoType, parent_id.clone()));
                    }
                    TokenType::MinusEqual => {
                        self.forward_token();

                        let rhs = self.expect_expr(parent_id.clone())?;
                        lhs.infered_type = rhs.infered_type.clone();

                        let binary_operation = AstBinaryOperation { operation: AstOperation::Subtract, left: Box::new(lhs.clone()), right: Box::new(rhs.clone()) };
                        let infered_ty = rhs.infered_type;
                        let inner =
                            self.new_node(AstNodeData::BinaryOperation(binary_operation), infered_ty, parent_id.clone());
                        return Ok(self.new_node(AstNodeData::Assign(Box::new(lhs), Box::new(inner)), AstNodeType::NoType, parent_id.clone()));
                    }
                    TokenType::ModEqual => {
                        self.forward_token();

                        let rhs = self.expect_expr(parent_id.clone())?;
                        lhs.infered_type = rhs.infered_type.clone();

                        let binary_operation = AstBinaryOperation { operation: AstOperation::Modulu, left: Box::new(lhs.clone()), right: Box::new(rhs.clone()) };
                        let infered_ty = rhs.infered_type;
                        let inner =
                            self.new_node(AstNodeData::BinaryOperation(binary_operation), infered_ty, parent_id.clone());
                        return Ok(self.new_node(AstNodeData::Assign(Box::new(lhs), Box::new(inner)), AstNodeType::NoType, parent_id.clone()));
                    }
                    TokenType::MulEqual => {
                        self.forward_token();

                        let rhs = self.expect_expr(parent_id.clone())?;
                        lhs.infered_type = rhs.infered_type.clone();

                        let binary_operation = AstBinaryOperation { operation: AstOperation::Multiply, left: Box::new(lhs.clone()), right: Box::new(rhs.clone()) };
                        let infered_ty = rhs.infered_type;
                        let inner =
                            self.new_node(AstNodeData::BinaryOperation(binary_operation), infered_ty, parent_id.clone());
                        return Ok(self.new_node(AstNodeData::Assign(Box::new(lhs), Box::new(inner)), AstNodeType::NoType, parent_id.clone()));
                    }
                    TokenType::DivEqual => {
                        self.forward_token();
                        
                        let rhs = self.expect_expr(parent_id.clone())?;
                        lhs.infered_type = rhs.infered_type.clone();

                        let binary_operation = AstBinaryOperation { operation: AstOperation::Divide, left: Box::new(lhs.clone()), right: Box::new(rhs.clone()) };
                        let infered_ty = rhs.infered_type;
                        let inner =
                            self.new_node(AstNodeData::BinaryOperation(binary_operation), infered_ty, parent_id.clone());

                        return Ok(self.new_node(AstNodeData::Assign(Box::new(lhs), Box::new(inner)), AstNodeType::NoType, parent_id.clone()));
                    }
                    TokenType::DoublePlus => {
                        self.forward_token();
                        let rhs = self.new_node(AstNodeData::Uint(1), AstNodeType::UnsignedInt(64), parent_id.clone());
                        lhs.infered_type = rhs.infered_type.clone();

                        let binary_operation = AstBinaryOperation { operation: AstOperation::Sum, left: Box::new(lhs.clone()), right: Box::new(rhs.clone()) };
                        let infered_ty = rhs.infered_type;
                        let inner =
                            self.new_node(AstNodeData::BinaryOperation(binary_operation), infered_ty, parent_id.clone());
                        return Ok(self.new_node(AstNodeData::Assign(Box::new(lhs), Box::new(inner)), AstNodeType::NoType, parent_id.clone()));
                    }
                    TokenType::DoubleMinus => {
                        self.forward_token();
                        
                        let rhs = self.new_node(AstNodeData::Uint(1), AstNodeType::UnsignedInt(64), parent_id.clone());
                        lhs.infered_type = rhs.infered_type.clone();

                        let binary_operation = AstBinaryOperation { operation: AstOperation::Subtract, left: Box::new(lhs.clone()), right: Box::new(rhs.clone()) };
                        let infered_ty = rhs.infered_type;
                        let inner =
                            self.new_node(AstNodeData::BinaryOperation(binary_operation), infered_ty, parent_id.clone());                        
                            return Ok(self.new_node(AstNodeData::Assign(Box::new(lhs), Box::new(inner)), AstNodeType::NoType, parent_id.clone()));
                    }
                    _ => {
                        panic!("expecting stmt got {:?}", self.current_token());
                    },
                }
            }

            TokenType::KeywordIf => {
                return self.expect_if(parent_id.clone());
            }
            TokenType::KeywordWhile => {
                self.forward_token();
                self.expect_token(TokenType::OpenParen)?;
                self.forward_token();
                let cond = self.expect_expr(parent_id.clone())?;
                if !cond.infered_type.is_unknown() && cond.infered_type != AstNodeType::Bool {
                    panic!("while condition should evaluate to boolean but: {:?}", cond);
                }
                self.forward_token();
                self.expect_token(TokenType::OpenBrace)?;
                let body = self.expect_block(parent_id.clone())?;

                return Ok(self.new_node(AstNodeData::While(Box::new(cond), body), AstNodeType::Unknown, parent_id.clone()));
            }

            TokenType::KeywordFor => {
                self.forward_token();
                self.expect_token(TokenType::OpenParen)?;
                self.forward_token();

                let starting_inside_paren = self.cur;
                let before_node_counter = self.node_counter;

                if self.expect_def(parent_id.clone()).is_ok() {
                    self.node_counter = before_node_counter;
                    self.cur = starting_inside_paren;
                    return self.expect_for_c(parent_id.clone());
                } else {
                    self.cur = starting_inside_paren;
                    self.node_counter = before_node_counter;
                }

                if self.expect_ident(parent_id.clone()).is_ok() {
                    if self.expect_token(TokenType::KeywordIn).is_ok() {
                        self.cur = starting_inside_paren;
                        self.node_counter = before_node_counter;
                        return self.expect_for_each(parent_id.clone());
                    } else {
                        self.cur = starting_inside_paren;
                        self.node_counter = before_node_counter;
                        return self.expect_for_each_implicit_iterator(parent_id.clone());
                    }
                } else {
                    self.cur = starting_inside_paren;
                    self.node_counter = before_node_counter;
                }

                if self.expect_expr(parent_id.clone()).is_ok() {
                    self.cur = starting_inside_paren;
                    self.node_counter = before_node_counter;

                    return self.expect_for_each_implicit_iterator(parent_id.clone());
                } else {
                    self.cur = starting_inside_paren;
                    self.node_counter = before_node_counter;

                    unreachable!();
                }
            }
            TokenType::KeywordReturn => {
                self.forward_token();
                let expr = self.expect_expr(parent_id.clone())?;
                self.forward_token();
                Ok(self.new_node(AstNodeData::Return(Box::new(expr)), AstNodeType::Unknown, parent_id.clone()))
            }

            TokenType::KeywordGoto => {
                //TODO

                unreachable!();
            }

            TokenType::KeywordContinue => {
                return Ok(self.new_node(AstNodeData::Break, AstNodeType::Unknown, parent_id.clone()));
            }

            TokenType::KeywordSwitch => {
                //TODO

                unreachable!();
            }

            TokenType::KeywordBreak => {
                return Ok(self.new_node(AstNodeData::Break, AstNodeType::Unknown, parent_id.clone()));
            }

            _ => {
                println!("expecting stmt: {:?}", self.current_token());
                unreachable!();
            }
        }
    }

    pub fn get_ast(mut self, st: &mut SymbolTable) -> Result<Ast> {
        let mut top_level = Vec::<AstNode>::new();
        let root_id: Vec<u64> = vec![];
        loop {
            if self.cur >= self.tokens.len() {
                break;
            }
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
                    top_level.push(self.new_node(AstNodeData::Load(literal.to_string()), AstNodeType::Unknown, root_id.clone()));
                }
                TokenType::CompilerFlagDirective => {
                    self.forward_token();
                    self.expect_token(TokenType::StringLiteral)?;
                    let path = self.cur;
                    self.forward_token();
                    if self.current_token().ty == TokenType::SemiColon {
                        self.forward_token();
                    }
                    top_level.push(self.new_node(AstNodeData::CompilerFlags(path.to_string()), AstNodeType::Unknown, root_id.clone()));
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
                    top_level.push(self.new_node(AstNodeData::Host(literal.to_string()), AstNodeType::Unknown, root_id.clone()));
                }
                TokenType::Ident => {
                    let decl = self.expect_def(root_id.clone())?;
                    if self.current_token().ty == TokenType::SemiColon {
                        self.forward_token();
                    }
                    top_level.push(decl);
                }
                _ => {
                    println!("expecting a top level item found: {:?}", self.current_token());
                    unreachable!();
                }
            }
        }
        Ast::new(self.filename, self.src, self.tokens, top_level, st)
    }
}

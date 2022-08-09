use crate::ast::{Def, Node, NodeData, NodeID, Ast, SymbolTable, ContainerField, Annotation, TypeAnnotation};
use crate::tokenizer::Token;
use crate::tokenizer::Tokenizer;
use crate::tokenizer::Type;
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
    fn err_uexpected(&self, what: Type) -> anyhow::Error {
        anyhow!(
            "Expected {:?}, found {:?} at \"{}\"",
            what,
            self.current_token().ty,
            self.src[self.tokens[self.cur].loc.0..=self.tokens[self.cur].loc.1].to_string(),
        )
    }
    fn new_node(&mut self, data: NodeData, annotations: Vec<Annotation>) -> Node {
        let node = Node {
            id: format!("{}_{}", self.filename, self.node_counter),
            data, annotations
        };
        self.node_counter += 1;
        return node;
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
                Type::EOF => {
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

    fn expect_ident(&mut self) -> Result<Node> {
        match self.current_token().ty {
            Type::Ident => {
                let name = self.src[self.current_token().loc.0..=self.current_token().loc.1].to_string();
                self.forward_token();
                return Ok(self.new_node(NodeData::Ident(name), vec![]));
            }
            _ => {
                return Err(self.err_uexpected(Type::Ident));
            }
        }
    }
    fn expect_string_literal(&mut self) -> Result<Node> {
        match self.current_token().ty {
            Type::StringLiteral => {
                return Ok(self.new_node(NodeData::StringLiteral(self.cur), vec![Annotation::Type(TypeAnnotation::String)]));
            }
            _ => {
                return Err(self.err_uexpected(Type::StringLiteral));
            }
        }
    }

    fn naive_ty_infer(&mut self, expr: &Node) -> Option<Node> {
        match &expr.data {
            NodeData::True(_) | NodeData::False(_) => {
                return Some(self.new_node(NodeData::BoolTy(0), vec![Annotation::Type(TypeAnnotation::Bool)]));
            }
            NodeData::Char(_) => {
                return Some(self.new_node(NodeData::CharTy(0), vec![Annotation::Type(TypeAnnotation::Char)]));
            }
            NodeData::Uint(_) => {
                return Some(self.new_node(NodeData::UintTy(0), vec![Annotation::Type(TypeAnnotation::Uint)]));
            }
            NodeData::Int(_) => {
                return Some(self.new_node(NodeData::IntTy(0), vec![Annotation::Type(TypeAnnotation::Int)]));
            }
            NodeData::Float(_) => {
                return Some(self.new_node(NodeData::FloatTy(0), vec![Annotation::Type(TypeAnnotation::Float64)]));
            }

            NodeData::StringLiteral(_) => {
                return Some(self.new_node(NodeData::StringTy(0), vec![Annotation::Type(TypeAnnotation::String)]));
            }
            NodeData::InitializeArray(op_ty, _) => {
                if op_ty.is_some() {
                    if let NodeData::ArrayTy(len, ty) = op_ty.clone().unwrap().data {
                        return Some(self.new_node(op_ty.clone().unwrap().data, vec![]));
                    } else {
                        return None;
                    }
                } else {
                    return None;
                }
            }
            NodeData::Initialize(op_ty, _) => {
                if op_ty.is_some() {
                    let type_name =
                        if let NodeData::Ident(ident) = op_ty.clone().unwrap().data {
                            ident
                        } else {
                            unreachable!()
                        };
                    return Some(self.new_node(NodeData::TEXT(type_name), vec![]));
                } else {
                    return None;
                }
            }

            _ => {
                return None;
            }
        }
    }
    fn expect_semicolon_and_forward(&mut self) -> Result<()> {
        self.expect_token(Type::SemiColon)?;
        self.forward_token();

        Ok(())
    }
    fn expect_def(&mut self) -> Result<Node> {
        let dest = self.expect_lhs()?;

        match self.current_token().ty {
            Type::Equal => {
                self.forward_token();
                let rhs = self.expect_expr()?;
                Ok(self.new_node(NodeData::Assign(Box::new(dest), Box::new(rhs)), vec![]))
            }
            Type::DoubleColon => {
                self.forward_token();
                let rhs = self.expect_expr()?;
                let ty_infer = self.naive_ty_infer(&rhs);
                Ok(self.new_node(NodeData::Def(Def {
                    mutable: false,
                    name: Box::new(dest),
                    ty: Box::new(ty_infer),
                    expr: Box::new(rhs),
                }), vec![]))
            }

            Type::Colon => {
                self.forward_token();
                let ty = Some(self.expect_expr()?);
                if ty.is_none() {
                    panic!("need a valid type after colon");
                }
                if self.current_token().ty != Type::Equal && self.current_token().ty != Type::Colon
                {
                    return Ok(self.new_node(NodeData::Decl(Box::new(dest), Box::new(ty.unwrap())), vec![]));
                }
                let mutable = self.current_token().ty == Type::Equal;
                self.forward_token();
                let rhs = self.expect_expr()?;
                Ok(self.new_node(NodeData::Def(Def {
                    mutable,
                    name: Box::new(dest),
                    ty: Box::new(ty),
                    expr: Box::new(rhs),
                }), vec![]))
            }

            Type::ColonEqual => {
                self.forward_token();
                let rhs = self.expect_expr()?;
                let ty_infer = self.naive_ty_infer(&rhs);
                Ok(self.new_node(NodeData::Def(Def {
                    mutable: true,
                    name: Box::new(dest),
                    ty: Box::new(ty_infer),
                    expr: Box::new(rhs),
                }), vec![]))
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

    fn expect_fn_def(&mut self) -> Result<Node> {
        self.expect_token(Type::KeywordFn)?;
        self.forward_token();
        self.expect_token(Type::OpenParen)?;
        let mut args: Vec<Node> = vec![];
        self.forward_token();
        loop {
            if self.current_token().ty == Type::Comma {
                self.forward_token();
            }
            if self.current_token().ty == Type::CloseParen {
                self.forward_token();
                break;
            }
            let name = self.expect_ident()?;
            self.expect_token(Type::Colon)?;
            self.forward_token();
            let ty = self.expect_expr()?;
            args.push(self.new_node(NodeData::Decl(Box::new(name), Box::new(ty)), vec![]));
        }

        let ret_ty = self.expect_expr()?;
        self.expect_token(Type::OpenBrace)?;

        let body = self.expect_block()?;

        let proto = self.new_node(NodeData::FnPrototype(args, Box::new(ret_ty)), vec![]);
        Ok(self.new_node(NodeData::FnDef(Box::new(proto), body), vec![]))
    }

    fn expect_fn_call(&mut self) -> Result<Node> {
        let name = self.expect_lhs()?;
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
        Ok(self.new_node(NodeData::FnCall(Box::new(name), args), vec![]))
    }
    fn expect_expr_container_field(&mut self) -> Result<Node> {
        let container = self.expect_expr_initialize()?;
        match self.current_token().ty {
            Type::Dot => {
                self.forward_token();
                let field = self.expect_expr()?;
                let cf = ContainerField {
                    container: Box::new(container),
                    field: Box::new(field),
                    container_is_enum: false,
                };
                return Ok(self.new_node(NodeData::ContainerField(cf), vec![]));
            }
            _ => {
                return Ok(container);
            }
        }
    }
    fn expect_expr_initialize(&mut self) -> Result<Node> {
        let ty = self.expect_expr_exact_expr()?;
        match self.current_token().ty {
            Type::OpenBrace => match ty.data {
                NodeData::IntTy(_)
                | NodeData::Int8Ty(_)
                | NodeData::Int32Ty(_)
                | NodeData::Int64Ty(_)
                | NodeData::Int128Ty(_)
                | NodeData::UintTy(_)
                | NodeData::Uint8Ty(_)
                | NodeData::Uint16Ty(_)
                | NodeData::Uint32Ty(_)
                | NodeData::Uint64Ty(_)
                | NodeData::Uint128Ty(_)
                | NodeData::CharTy(_)
                | NodeData::BoolTy(_)
                | NodeData::StringTy(_)
                | NodeData::FloatTy(_)
                | NodeData::VoidTy(_) => {
                    return Ok(ty);
                }

                _ => {
                    let mut is_struct_init = false;
                    self.forward_token();
                    if self.current_token().ty == Type::Ident {
                        self.forward_token();
                        if self.current_token().ty == Type::Equal {
                            is_struct_init = true;
                            self.backward_token();
                            self.backward_token();
                        } else {
                            self.backward_token();
                        }
                    }

                    if is_struct_init {
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
                        return Ok(self.new_node(NodeData::Initialize(Some(Box::new(ty)), fields), vec![]));
                    } else {
                        let mut fields = Vec::<Node>::new();
                        loop {
                            if self.current_token().ty == Type::CloseBrace {
                                self.forward_token();
                                break;
                            }

                            let val = self.expect_expr()?;
                            fields.push(val);
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

                        return Ok(
                            self.new_node(NodeData::InitializeArray(Some(Box::new(ty)), fields), vec![])
                        );
                    }
                }
            },
            _ => {
                return Ok(ty);
            }
        }
    }

    fn expect_expr_exact_expr(&mut self) -> Result<Node> {
        match self.current_token().ty {
            Type::UnsignedInt => {
                self.forward_token();
                Ok(self.new_node(NodeData::Uint(self.cur - 1), vec![]))
            }
            Type::Float => {
                self.forward_token();
                Ok(self.new_node(NodeData::Float(self.cur - 1), vec![]))
            }
            Type::StringLiteral => {
                self.forward_token();
                Ok(self.new_node(NodeData::StringLiteral(self.cur - 1), vec![]))
            }
            Type::KeywordTrue => {
                self.forward_token();
                Ok(self.new_node(NodeData::True(self.cur - 1), vec![]))
            }
            Type::KeywordFalse => {
                self.forward_token();
                Ok(self.new_node(NodeData::False(self.cur - 1), vec![]))
            }
            Type::Char => {
                self.forward_token();
                Ok(self.new_node(NodeData::Char(self.cur - 1), vec![]))
            }
            Type::KeywordStruct => {
                self.forward_token();
                self.expect_token(Type::OpenBrace)?;
                self.forward_token();
                let mut fields = Vec::<Node>::new();
                loop {
                    if self.current_token().ty == Type::CloseBrace {
                        self.forward_token();
                        break;
                    }
                    let name = self.expect_ident()?;

                    self.expect_token(Type::Colon)?;
                    self.forward_token();
                    let ty = self.expect_expr()?;
                    fields.push(self.new_node(NodeData::Decl(Box::new(name), Box::new(ty)), vec![]));
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

                Ok(self.new_node(NodeData::Struct(fields), vec![]))
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

                Ok(self.new_node(NodeData::Enum(is_union, variants), vec![]))
            }
            Type::OpenParen => {
                self.forward_token();
                let expr = self.expect_expr()?;
                self.expect_token(Type::CloseParen)?;
                Ok(expr)
            }

            Type::OpenBracket => {
                // array type
                self.forward_token();
                let len = self.expect_expr()?;
                self.expect_token(Type::CloseBracket)?;
                self.forward_token();
                let ty = self.expect_expr_exact_expr()?;
                return Ok(self.new_node(NodeData::ArrayTy(Box::new(len), Box::new(ty)), vec![]));
            }

            Type::Asterix => {
                self.forward_token();
                let expr = self.expect_expr()?;
                return Ok(self.new_node(NodeData::Deref(Box::new(expr)), vec![]));
            }
            Type::Ampersand => {
                self.forward_token();
                let expr = self.expect_expr()?;
                return Ok(self.new_node(NodeData::Ref(Box::new(expr)), vec![]));
            }
            Type::Dot => {
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

                return Ok(self.new_node(NodeData::Initialize(None, fields), vec![]));
            }
            Type::Ident => {
                self.forward_token();
                match self.current_token().ty {
                    Type::OpenParen => {
                        //function call
                        self.backward_token();
                        self.expect_fn_call()
                    }
                    _ => {
                        self.backward_token();
                        let name = self.expect_ident()?;
                        return Ok(name);
                            
                    },
                }
            }
            Type::KeywordVoid => {
                self.forward_token();
                Ok(self.new_node(NodeData::VoidTy(self.cur - 1), vec![]))
            }
            Type::KeywordInt => {
                self.forward_token();
                Ok(self.new_node(NodeData::IntTy(self.cur - 1), vec![]))
            }
            Type::KeywordInt8 => {
                self.forward_token();
                Ok(self.new_node(NodeData::Int8Ty(self.cur - 1), vec![]))
            }
            Type::KeywordInt16 => {
                self.forward_token();
                Ok(self.new_node(NodeData::Int16Ty(self.cur - 1), vec![]))
            }
            Type::KeywordInt32 => {
                self.forward_token();
                Ok(self.new_node(NodeData::Int32Ty(self.cur - 1), vec![]))
            }
            Type::KeywordInt64 => {
                self.forward_token();
                Ok(self.new_node(NodeData::Int64Ty(self.cur - 1), vec![]))
            }
            Type::KeywordInt128 => {
                self.forward_token();
                Ok(self.new_node(NodeData::Int128Ty(self.cur - 1), vec![]))
            }
            Type::KeywordUint => {
                self.forward_token();
                Ok(self.new_node(NodeData::UintTy(self.cur - 1), vec![]))
            }
            Type::KeywordUint8 => {
                self.forward_token();
                Ok(self.new_node(NodeData::Uint8Ty(self.cur - 1), vec![]))
            }
            Type::KeywordUint16 => {
                self.forward_token();
                Ok(self.new_node(NodeData::Uint16Ty(self.cur - 1), vec![]))
            }
            Type::KeywordUint32 => {
                self.forward_token();
                Ok(self.new_node(NodeData::Uint32Ty(self.cur - 1), vec![]))
            }
            Type::KeywordUint64 => {
                self.forward_token();
                Ok(self.new_node(NodeData::Uint64Ty(self.cur - 1), vec![]))
            }
            Type::KeywordUint128 => {
                self.forward_token();
                Ok(self.new_node(NodeData::Uint128Ty(self.cur - 1), vec![]))
            }
            Type::KeywordFloat => {
                self.forward_token();
                Ok(self.new_node(NodeData::FloatTy(self.cur - 1), vec![]))
            }
            Type::KeywordChar => {
                self.forward_token();
                Ok(self.new_node(NodeData::CharTy(self.cur - 1), vec![]))
            }
            Type::KeywordBool => {
                self.forward_token();
                Ok(self.new_node(NodeData::BoolTy(self.cur - 1), vec![]))
            }

            Type::KeywordString => {
                self.forward_token();
                Ok(self.new_node(NodeData::StringTy(self.cur - 1), vec![]))
            }

            Type::KeywordFn => self.expect_fn_def(),

            _ => {
                println!("unknown expr: {:?}", self.current_token());
                unreachable!();
            }
        }
    }

    fn expect_expr_b(&mut self) -> Result<Node> {
        let lhs = self.expect_expr_container_field()?;

        match self.current_token().ty {
            Type::LeftAngle
                | Type::RightAngle
                | Type::LessEqual
                | Type::GreaterEqual
                | Type::DoubleEqual
                | Type::NotEqual => {

                    let op = self.current_token().ty.clone();
                    self.forward_token();
                    let rhs = self.expect_expr_container_field()?;
                    Ok(self.new_node(NodeData::Cmp(op, Box::new(lhs), Box::new(rhs)), vec![]))
            }

            _ => Ok(lhs),
        }
    }
    fn expect_expr_mul_div_mod(&mut self) -> Result<Node> {
        let lhs = self.expect_expr_b()?;

        match self.current_token().ty {
            Type::Asterix => {
                self.forward_token();
                let rhs = self.expect_expr_b()?;
                Ok(self.new_node(NodeData::Multiply(Box::new(lhs), Box::new(rhs)), vec![]))
            }
            Type::Percent => {
                self.forward_token();
                let rhs = self.expect_expr_b()?;
                Ok(self.new_node(NodeData::Mod(Box::new(lhs), Box::new(rhs)), vec![]))
            }
            Type::ForwardSlash => {
                self.forward_token();
                let rhs = self.expect_expr_b()?;
                Ok(self.new_node(NodeData::Div(Box::new(lhs), Box::new(rhs)), vec![]))
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
                Ok(self.new_node(NodeData::Subtract(Box::new(lhs), Box::new(rhs)), vec![]))
            }
            Type::Plus => {
                self.forward_token();
                let rhs = self.expect_expr_mul_div_mod()?;
                Ok(self.new_node(NodeData::Sum(Box::new(lhs), Box::new(rhs)), vec![]))
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
            self.if_semicolon_forward();
            stmts.push(stmt);
        }
        self.forward_token();

        Ok(stmts)
    }

    fn if_semicolon_forward(&mut self) {
        if self.current_token().ty == Type::SemiColon {
            self.forward_token();
        }
    }

    fn expect_for_c(&mut self) -> Result<Node> {
        let start = self.expect_def()?;
        self.expect_semicolon_and_forward()?;
        let cond = self.expect_expr()?;
        self.expect_semicolon_and_forward()?;
        let cont = self.expect_stmt()?;
        self.expect_token(Type::CloseParen)?;
        self.forward_token();
        let body = self.expect_block()?;

        return Ok(self.new_node(NodeData::For(
            Box::new(start),
            Box::new(cond),
            Box::new(cont),
            body,
        ), vec![]));
    }
    
    // expect_dest will return a node that can be used as a lhs of an assignment.
    fn expect_lhs(&mut self) -> Result<Node> {
        match self.current_token().ty {
            Type::Ident => {
                self.forward_token();
                if self.current_token().ty == Type::Dot {
                    self.backward_token();
                    let cf = self.expect_expr_container_field();
                    return cf;
                } else {
                    self.backward_token();
                    return self.expect_ident();
                }
            }

            _ => {
                panic!("token {:?} cannot be a destination node.", self.current_token());
            }
        }
    }
    fn expect_for_each(&mut self) -> Result<Node> {
        let iterator = self.expect_ident()?;
        self.forward_token();
        let iterable = self.expect_expr()?;
        self.expect_token(Type::CloseParen)?;
        self.forward_token();
        self.expect_token(Type::OpenBrace)?;
        let body = self.expect_block()?;
        return Ok(self.new_node(NodeData::ForIn(
            Some(Box::new(iterator)),
            Box::new(iterable),
            body,
        ), vec![]));
    }
    fn expect_for_each_implicit_iterator(&mut self) -> Result<Node> {
        let iterable = self.expect_expr()?;
        self.expect_token(Type::CloseParen)?;
        self.forward_token();
        self.expect_token(Type::OpenBrace)?;
        let body = self.expect_block()?;
        let iterator = self.new_node(NodeData::TEXT("it".to_string()), vec![]);
        return Ok(self.new_node(NodeData::ForIn(
            Some(Box::new(iterator)),
            Box::new(iterable),
            body,
        ), vec![])); //TODO: we should handle any expression here now we just handle ident
    }
    fn expect_stmt(&mut self) -> Result<Node> {
        match self.current_token().ty {
            Type::Ident => {
                let before_lhs_cur = self.cur;
                let lhs = self.expect_lhs()?;
                match self.current_token().ty {
                    Type::DoubleColon | Type::Colon | Type::Equal | Type::ColonEqual => {
                        self.cur = before_lhs_cur;
                        let def = self.expect_def()?;
                        self.expect_semicolon_and_forward()?;
                        return Ok(def);
                    }
                    Type::OpenParen => {
                        self.cur = before_lhs_cur;
                        self.expect_fn_call()
                    },
                    Type::PlusEqual => {
                        self.forward_token();
                        let rhs = self.expect_expr()?;
                        let inner =
                            self.new_node(NodeData::Sum(Box::new(lhs.clone()), Box::new(rhs)), vec![]);
                        return Ok(self.new_node(NodeData::Assign(Box::new(lhs), Box::new(inner)), vec![]));
                    }
                    Type::MinusEqual => {
                        self.forward_token();

                        let rhs = self.expect_expr()?;
                        let inner =
                            self.new_node(NodeData::Subtract(Box::new(lhs.clone()), Box::new(rhs)), vec![]);
                        return Ok(self.new_node(NodeData::Assign(Box::new(lhs), Box::new(inner)), vec![]));
                    }
                    Type::ModEqual => {
                        self.forward_token();

                        let rhs = self.expect_expr()?;
                        let inner =
                            self.new_node(NodeData::Mod(Box::new(lhs.clone()), Box::new(rhs)), vec![]);
                        return Ok(self.new_node(NodeData::Assign(Box::new(lhs), Box::new(inner)), vec![]));
                    }
                    Type::MulEqual => {
                        self.forward_token();

                        let rhs = self.expect_expr()?;
                        let inner =
                            self.new_node(NodeData::Multiply(Box::new(lhs.clone()), Box::new(rhs)), vec![]);
                        return Ok(self.new_node(NodeData::Assign(Box::new(lhs), Box::new(inner)), vec![]));
                    }
                    Type::DivEqual => {
                        self.forward_token();
                        
                        let rhs = self.expect_expr()?;
                        let inner =
                            self.new_node(NodeData::Div(Box::new(lhs.clone()), Box::new(rhs)), vec![]);
                        return Ok(self.new_node(NodeData::Assign(Box::new(lhs), Box::new(inner)), vec![]));
                    }
                    Type::DoublePlus => {
                        self.forward_token();
                        let rhs = self.new_node(NodeData::TEXT("1".to_string()), vec![]);
                        let inner =
                            self.new_node(NodeData::Sum(Box::new(lhs.clone()), Box::new(rhs)), vec![]);
                        return Ok(self.new_node(NodeData::Assign(Box::new(lhs), Box::new(inner)), vec![]));
                    }
                    Type::DoubleMinus => {
                        self.forward_token();
                        
                        let rhs = self.new_node(NodeData::TEXT("1".to_string()), vec![]);
                        let inner =
                            self.new_node(NodeData::Sum(Box::new(lhs.clone()), Box::new(rhs)), vec![]);
                        return Ok(self.new_node(NodeData::Assign(Box::new(lhs), Box::new(inner)), vec![]));
                    }
                    _ => {
                        panic!("expecting stmt got {:?}", self.current_token());
                    },
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
                        return Ok(self.new_node(NodeData::If(
                            Box::new(cond),
                            Box::new(then),
                            Some(_else),
                        ), vec![]));
                    }
                    _ => {
                        return Ok(self.new_node(NodeData::If(
                            Box::new(cond),
                            Box::new(then),
                            None,
                        ), vec![]));
                    }
                }
            }
            Type::KeywordWhile => {
                self.forward_token();
                self.expect_token(Type::OpenParen)?;
                self.forward_token();
                let cond = self.expect_expr()?;
                self.forward_token();
                self.expect_token(Type::OpenBrace)?;
                let body = self.expect_block()?;

                return Ok(self.new_node(NodeData::While(Box::new(cond), body), vec![]));
            }

            Type::KeywordFor => {
                self.forward_token();
                self.expect_token(Type::OpenParen)?;
                self.forward_token();

                let starting_inside_paren = self.cur;
                let before_node_counter = self.node_counter;

                if self.expect_def().is_ok() {
                    self.node_counter = before_node_counter;
                    self.cur = starting_inside_paren;
                    return self.expect_for_c();
                } else {
                    self.cur = starting_inside_paren;
                    self.node_counter = before_node_counter;
                }

                if self.expect_ident().is_ok() {
                    if self.expect_token(Type::KeywordIn).is_ok() {
                        self.cur = starting_inside_paren;
                        self.node_counter = before_node_counter;
                        return self.expect_for_each();
                    } else {
                        self.cur = starting_inside_paren;
                        self.node_counter = before_node_counter;
                        return self.expect_for_each_implicit_iterator();
                    }
                } else {
                    self.cur = starting_inside_paren;
                    self.node_counter = before_node_counter;
                }

                if self.expect_expr().is_ok() {
                    self.cur = starting_inside_paren;
                    self.node_counter = before_node_counter;

                    return self.expect_for_each_implicit_iterator();
                } else {
                    self.cur = starting_inside_paren;
                    self.node_counter = before_node_counter;

                    unreachable!();
                }
            }
            Type::KeywordReturn => {
                self.forward_token();
                let expr = self.expect_expr()?;
                self.expect_token(Type::SemiColon)?;
                self.forward_token();
                Ok(self.new_node(NodeData::Return(Box::new(expr)), vec![]))
            }

            Type::KeywordGoto => {
                //TODO

                unreachable!();
            }

            Type::KeywordContinue => {
                return Ok(self.new_node(NodeData::Break, vec![]));
            }

            Type::KeywordSwitch => {
                //TODO

                unreachable!();
            }

            Type::KeywordBreak => {
                return Ok(self.new_node(NodeData::Break, vec![]));
            }

            _ => {
                println!("expecting stmt: {:?}", self.current_token());
                unreachable!();
            }
        }
    }

    pub fn get_ast(mut self, st: &mut SymbolTable) -> Result<Ast> {
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
                    top_level.push(self.new_node(NodeData::Load(path), vec![]));
                }
                Type::C_CompilerFlagDirective => {
                    self.forward_token();
                    self.expect_token(Type::StringLiteral)?;
                    let path = self.cur;
                    self.forward_token();
                    if self.current_token().ty == Type::SemiColon {
                        self.forward_token();
                    }
                    top_level.push(self.new_node(NodeData::C_CompilerFlag(path), vec![]));
                }
                Type::HostDirective => {
                    self.forward_token();
                    self.expect_token(Type::StringLiteral)?;
                    let path = self.cur;
                    self.forward_token();
                    if self.current_token().ty == Type::SemiColon {
                        self.forward_token();
                    }
                    top_level.push(self.new_node(NodeData::Host(path), vec![]));
                }
                Type::Ident => {
                    let decl = self.expect_def()?;
                    self.expect_semicolon_and_forward()?;
                    top_level.push(decl);
                }
                _ => {
                    println!("don't know what to do with: {:?}", self.current_token());
                    unreachable!();
                }
            }
        }
        Ok(Ast::new(self.filename, self.src, self.tokens, top_level, st)?)
    }
}

#![allow(clippy::needless_return)]
use std::collections::HashMap;

use crate::ir::{AstTag, BinaryOperation, Node, NodeData, NodeIndex, IR};
use crate::lexer::{Token, TokenType};
use crate::stack::Stack;
use crate::utils;
use rand::distributions::{Alphanumeric, DistString};
use rand::Rng;

use crate::errors::{CompilerError, ParseError, Reason, Result};
use crate::ir::{Expression, Statement, TypeDefinition, UnaryOperation};
const ID_LENGTH: usize = 24;

#[derive(Debug)]
pub struct Parser {
    filename: String,
    src: String,
    tokens: Vec<Token>,
    cur: usize,
    node_counter: i64,
    ir: IR,
    owner_stack: Stack<NodeIndex>,
    scope_stack: Stack<NodeIndex>,
}

// Every parser function should parse until the last token in it's scope and then move cursor to the next token. so every parse function moves the cursor to the next.
impl Parser {
    fn err_uexpected_token(&self, what: TokenType) -> CompilerError {
        CompilerError {
            filename: self.filename.clone(),
            file_source: self.src.clone(),
            line: self.current_line(),
            col: self.current_col(),
            reason: Reason::ParseError(ParseError::UnexpectedToken { expected: what }),
        }
    }

    fn report_error(&self, parse_error: ParseError) -> CompilerError {
        return CompilerError {
            file_source: self.src.clone(),
            filename: self.filename.clone(),
            line: self.current_line(),
            col: self.current_col(),
            reason: Reason::ParseError(parse_error),
        };
    }
    fn new_index(&self) -> NodeIndex {
        let mut rng = rand::thread_rng();
        let id: u64 = rng.gen();
        return id;
    }
    fn new_node(
        &mut self,
        id: NodeIndex,
        data: NodeData,
        start_line: usize,
        start_col: usize,
    ) -> Node {
        self.node_counter += 1;
        let node = Node {
            id,
            data,
            tags: vec![],
            line: start_line,
            col: start_col,
            parent_block: self.scope_stack.top().clone(),
            filename: self.filename.clone(),
            type_information: None,
        };
        self.ir.registered_indexes.push(id);
        self.ir.nodes.insert(id, node.clone());
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
    pub fn new(filename: String, src: String, tokens: Vec<Token>) -> Result<Self> {
        let mut hir = IR {
            file_source: src.clone(),
            filename: filename.clone(),
            tokens: tokens.clone(),
            registered_indexes: vec![],
            root: 0,
            nodes: HashMap::new(),
            scoped_symbols: HashMap::new(),
            exported_symbols: HashMap::new(),
            dependencies: vec![],
            type_checked: false,
            continue_jstack: Stack::new(),
        };
        Ok(Self {
            filename,
            src,
            tokens,
            cur: 0,
            node_counter: 0,
            ir: hir,
            scope_stack: Stack::new(),
            owner_stack: Stack::new(),
        })
    }
    fn expect_ident(&mut self) -> Result<Node> {
        match self.current_token().ty {
            TokenType::Ident => {
                let name =
                    self.src[self.current_token().loc.0..=self.current_token().loc.1].to_string();
                let node = self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::Identifier(name)),
                    self.current_token().line,
                    self.current_token().col,
                );
                self.forward_token();
                return Ok(node);
            }
            _ => {
                return Err(self.report_error(ParseError::UnknownExpression(
                    self.current_token().ty.clone(),
                )));
            }
        }
    }
    fn expect_string_literal(&mut self) -> Result<Node> {
        match self.current_token().ty {
            TokenType::StringLiteral => {
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0..=src_range.loc.1];
                return Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::StringLiteral(literal.to_string())),
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
    fn expect_definition_declaration_assignment(&mut self) -> Result<Node> {
        let mut dest = self.expect_expr()?;
        match self.current_token().ty {
            TokenType::Equal => {
                self.forward_token();
                let rhs = self.expect_expr()?;
                let node = self.new_node(
                    self.new_index(),
                    NodeData::Statement(Statement::Assign {
                        lhs: dest.id,
                        rhs: rhs.id,
                    }),
                    self.current_token().line,
                    self.current_token().col,
                );
                Ok(node)
            }
            TokenType::PlusEqual => {
                self.forward_token();
                let rhs = self.expect_expr()?;
                let inner = self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::BinaryOperation {
                        operation: BinaryOperation::Sum,
                        left: dest.id.clone(),
                        right: rhs.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                );
                return Ok(self.new_node(
                    self.new_index(),
                    NodeData::Statement(Statement::Assign {
                        lhs: dest.id,
                        rhs: inner.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                ));
            }
            TokenType::MinusEqual => {
                self.forward_token();
                let rhs = self.expect_expr()?;
                let inner = self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::BinaryOperation {
                        operation: BinaryOperation::Subtract,
                        left: dest.id.clone(),
                        right: rhs.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                );
                return Ok(self.new_node(
                    self.new_index(),
                    NodeData::Statement(Statement::Assign {
                        lhs: dest.id,
                        rhs: inner.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                ));
            }
            TokenType::ModEqual => {
                self.forward_token();

                let rhs = self.expect_expr()?;
                let inner = self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::BinaryOperation {
                        operation: BinaryOperation::Modulu,
                        left: dest.id.clone(),
                        right: rhs.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                );
                return Ok(self.new_node(
                    self.new_index(),
                    NodeData::Statement(Statement::Assign {
                        lhs: dest.id,
                        rhs: inner.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                ));
            }
            TokenType::MulEqual => {
                self.forward_token();

                let rhs = self.expect_expr()?;
                let inner = self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::BinaryOperation {
                        operation: BinaryOperation::Multiply,
                        left: dest.id.clone(),
                        right: rhs.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                );
                return Ok(self.new_node(
                    self.new_index(),
                    NodeData::Statement(Statement::Assign {
                        lhs: dest.id,
                        rhs: inner.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                ));
            }
            TokenType::DivEqual => {
                self.forward_token();
                let rhs = self.expect_expr()?;
                let inner = self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::BinaryOperation {
                        operation: BinaryOperation::Divide,
                        left: dest.id.clone(),
                        right: rhs.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                );

                return Ok(self.new_node(
                    self.new_index(),
                    NodeData::Statement(Statement::Assign {
                        lhs: dest.id,
                        rhs: inner.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                ));
            }
            TokenType::DoublePlus => {
                self.forward_token();
                let rhs = self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::Unsigned(1)),
                    self.current_line(),
                    self.current_col(),
                );
                let inner = self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::BinaryOperation {
                        operation: BinaryOperation::Sum,
                        left: dest.id.clone(),
                        right: rhs.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                );
                return Ok(self.new_node(
                    self.new_index(),
                    NodeData::Statement(Statement::Assign {
                        lhs: dest.id,
                        rhs: inner.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                ));
            }
            TokenType::DoubleMinus => {
                self.forward_token();

                let rhs = self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::Unsigned(1)),
                    self.current_line(),
                    self.current_col(),
                );
                let inner = self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::BinaryOperation {
                        operation: BinaryOperation::Subtract,
                        left: dest.id.clone(),
                        right: rhs.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                );
                return Ok(self.new_node(
                    self.new_index(),
                    NodeData::Statement(Statement::Assign {
                        lhs: dest.id,
                        rhs: inner.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                ));
            }
            TokenType::DoubleColon => {
                self.forward_token();
                let rhs = self.expect_expr()?;
                let dest_clone = match self.ir.get_node(dest.id.clone()) {
                    Ok(d) => d,
                    Err(_) => {
                        return Err(self.report_error(ParseError::UnknownNode(dest.id.clone())))
                    }
                };
                let node = self.new_node(
                    self.new_index(),
                    NodeData::Statement(Statement::Def {
                        mutable: false,
                        name: dest.id,
                        ty: None,
                        expr: rhs.id,
                    }),
                    self.current_token().line,
                    self.current_token().col,
                );
                Ok(node)
            }

            TokenType::Colon => {
                self.forward_token();
                let ty = self.expect_type_expression()?;
                if self.current_token().ty != TokenType::Equal
                    && self.current_token().ty != TokenType::Colon
                {
                    let decl_node = self.new_node(
                        self.new_index(),
                        NodeData::Statement(Statement::Decl {
                            name: dest.id.clone(),
                            ty: ty.id.clone(),
                        }),
                        self.current_line(),
                        self.current_col(),
                    );

                    if self.current_token().ty == TokenType::ForeignDirective {
                        self.ir.add_tag(decl_node.id, AstTag::Foreign);
                        self.forward_token();
                    }

                    return Ok(decl_node);
                }
                let mutable = self.current_token().ty == TokenType::Equal;
                self.forward_token();
                let rhs = self.expect_expr()?;
                let node = self.new_node(
                    self.new_index(),
                    NodeData::Statement(Statement::Def {
                        mutable,
                        name: dest.id,
                        ty: Some(ty.id),
                        expr: rhs.id,
                    }),
                    self.current_token().line,
                    self.current_token().col,
                );
                Ok(node)
            }

            TokenType::ColonEqual => {
                self.forward_token();
                let rhs = self.expect_expr()?;
                let node = self.new_node(
                    self.new_index(),
                    NodeData::Statement(Statement::Def {
                        mutable: true,
                        name: dest.id,
                        ty: None,
                        expr: rhs.id,
                    }),
                    self.current_token().line,
                    self.current_token().col,
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
    fn expect_function_definition(&mut self) -> Result<Node> {
        self.expect_token(TokenType::OpenParen)?;
        let mut args: Vec<NodeIndex> = vec![];
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

            let arg = self.new_node(
                self.new_index(),
                NodeData::Statement(Statement::Decl {
                    name: name.id.clone(),
                    ty: ty.id.clone(),
                }),
                name.line,
                name.col,
            );
            args.push(arg.id);
        }
        let mut ret_ty: Option<Node> = None;
        if self.current_token().ty != TokenType::OpenBrace {
            ret_ty = Some(self.expect_type_expression()?);
        } else {
            ret_ty = Some(self.new_node(
                self.new_index(),
                NodeData::TypeDefinition(TypeDefinition::Void),
                self.current_line(),
                self.current_col(),
            ));
        }
        let ret_ty = ret_ty.unwrap();
        if self.current_token().ty != TokenType::OpenBrace {
            // fn type only
            let node = self.new_node(
                self.new_index(),
                NodeData::TypeDefinition(TypeDefinition::Function {
                    args,
                    ret: ret_ty.id,
                }),
                self.current_line(),
                self.current_col(),
            );
            return Ok(node);
        }
        let body = self.expect_block()?;
        let fn_def = self.new_node(
            self.new_index(),
            NodeData::Expression(Expression::Function {
                args,
                ret_ty: ret_ty.id,
                body: body,
            }),
            self.current_line(),
            self.current_col(),
        );
        Ok(fn_def)
    }

    fn expect_fn_call(&mut self) -> Result<Node> {
        let name = self.expect_ident()?;
        self.expect_token(TokenType::OpenParen)?;
        let mut args = Vec::<NodeIndex>::new();

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
        Ok(self.new_node(
            self.new_index(),
            NodeData::Expression(Expression::FunctionCall {
                fn_name: name.id,
                args,
            }),
            self.current_line(),
            self.current_col(),
        ))
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

    fn expect_token(&mut self, ty: TokenType) -> Result<()> {
        if self.current_token().ty != ty {
            return Err(self.err_uexpected_token(ty));
        }

        return Ok(());
    }

    fn expect_block(&mut self) -> Result<NodeIndex> {
        if self.current_token().ty == TokenType::OpenBrace {
            self.forward_token()
        }
        let mut stmts = Vec::<NodeIndex>::new();
        let mut block_id = self.new_index();
        let is_file_root = self.scope_stack.len() == 0;
        let block_node = self.new_node(
            block_id.clone(),
            NodeData::Statement(Statement::Scope {
                owner: 0,
                stmts: vec![],
                is_file_root,
            }),
            self.current_line(),
            self.current_col(),
        );
        self.scope_stack.push(block_id.clone());
        loop {
            if (self.current_token().ty == TokenType::CloseBrace || self.cur >= self.tokens.len()) {
                // if is_file_root { // BUG
                //     return Err(self.report_error(ParseError::UnknownStatement(self.current_token().ty.clone())));
                // }
                break;
            }
            let stmt = self.expect_statement()?;
            self.if_semicolon_forward();
            let mut block_mut = self.ir.nodes.get_mut(&block_id.clone()).unwrap();
            if let NodeData::Statement(Statement::Scope {
                ref owner,
                ref mut stmts,
                ref is_file_root,
            }) = &mut block_mut.data
            {
                stmts.push(stmt.id);
            }
        }
        self.scope_stack.pop();

        self.forward_token();
        Ok(block_node.id)
    }

    fn if_semicolon_forward(&mut self) {
        if self.current_token().ty == TokenType::SemiColon {
            self.forward_token();
        }
    }

    fn expect_for_c(&mut self) -> Result<Node> {
        println!("parsing for c...");
        let start = self.expect_definition_declaration_assignment()?;
        self.expect_semicolon_and_forward()?;
        let cond = self.expect_expr()?;
        self.expect_semicolon_and_forward()?;
        let cont = self.expect_statement()?;
        self.expect_token(TokenType::CloseParen)?;
        self.forward_token();
        let body = self.expect_block()?;
        let for_node = self.new_node(
            self.new_index(),
            NodeData::Statement(Statement::For {
                start: start.id,
                cond: cond.id,
                cont: cont.id,
                body: body,
            }),
            self.current_line(),
            self.current_col(),
        );
        return Ok(for_node);
    }

    fn expect_for_each(&mut self) -> Result<Node> {
        let iterator = self.expect_ident()?;
        self.forward_token();
        let iterable = self.expect_expr()?;
        self.expect_token(TokenType::CloseParen)?;
        self.forward_token();
        self.expect_token(TokenType::OpenBrace)?;
        let body = self.expect_block()?;
        let node = self.new_node(
            self.new_index(),
            NodeData::Statement(Statement::ForIn {
                iterator: iterator.id,
                iterable: iterable.id,
                body,
            }),
            self.current_line(),
            self.current_col(),
        );
        return Ok(node);
    }

    fn expect_for_each_implicit_iterator(&mut self) -> Result<Node> {
        let iterable = self.expect_expr()?;
        self.expect_token(TokenType::CloseParen)?;
        self.forward_token();
        self.expect_token(TokenType::OpenBrace)?;
        let body = self.expect_block()?;
        let iterator = self.new_node(
            self.new_index(),
            NodeData::Expression(Expression::Identifier("it".to_string())),
            self.current_line(),
            self.current_col(),
        );
        let node = self.new_node(
            self.new_index(),
            NodeData::Statement(Statement::ForIn {
                iterator: iterator.id,
                iterable: iterable.id,
                body,
            }),
            self.current_line(),
            self.current_col(),
        );
        return Ok(node);
    }

    fn expect_if(&mut self) -> Result<Node> {
        self.forward_token();
        self.expect_token(TokenType::OpenParen)?;
        self.forward_token();
        let cond = self.expect_expr()?;
        self.expect_token(TokenType::CloseParen)?;
        self.forward_token();

        self.expect_token(TokenType::OpenBrace)?;
        let then = self.expect_block()?;
        if self.current_token().ty == TokenType::KeywordElse {
            self.forward_token();
            if self.current_token().ty == TokenType::KeywordIf {
                let else_if = self.expect_if()?;
                let mut cond_thens = vec![(cond.id, then)];
                if let NodeData::Statement(Statement::If { cases }) = else_if.data {
                    for cond_then in cases {
                        cond_thens.push(cond_then.clone());
                    }
                }
                let node = self.new_node(
                    self.new_index(),
                    NodeData::Statement(Statement::If { cases: cond_thens }),
                    self.current_line(),
                    self.current_col(),
                );
                self.ir.nodes.remove(&else_if.id);
                return Ok(node);
            } else if self.current_token().ty == TokenType::OpenBrace {
                let _else = self.expect_block()?;
                let default_case = self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::Bool(true)),
                    self.current_line(),
                    self.current_col(),
                );
                let cases = vec![(cond.id, then), (default_case.id, _else)];
                let node = self.new_node(
                    self.new_index(),
                    NodeData::Statement(Statement::If { cases }),
                    self.current_line(),
                    self.current_col(),
                );
                return Ok(node);
            } else {
                return Err(self.report_error(ParseError::Unknown(format!("after else keyword we expect either a code block or if keyword but found: {:?}", self.current_token()))));
            }
        }
        let node = self.new_node(
            self.new_index(),
            NodeData::Statement(Statement::If {
                cases: vec![(cond.id, then)],
            }),
            self.current_line(),
            self.current_col(),
        );
        return Ok(node);
    }
    fn expect_statement(&mut self) -> Result<Node> {
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
                let top = self.new_node(
                    self.new_index(),
                    NodeData::Statement(Statement::Load(literal.to_string())),
                    self.current_line(),
                    self.current_col(),
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
                let top = self.new_node(
                    self.new_index(),
                    NodeData::Statement(Statement::Host(literal.to_string())),
                    self.current_line(),
                    self.current_col(),
                );
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
                    | TokenType::Dot
                    | TokenType::PlusEqual
                    | TokenType::MinusEqual
                    | TokenType::ModEqual
                    | TokenType::MulEqual
                    | TokenType::DivEqual
                    | TokenType::DoublePlus
                    | TokenType::DoubleMinus => {
                        self.backward_token();
                        let def = self.expect_definition_declaration_assignment()?;
                        return Ok(def);
                    }
                    TokenType::OpenParen => {
                        self.backward_token();
                        self.expect_fn_call()
                    }
                    _ => {
                        return Err(self.report_error(ParseError::Unknown(format!(
                            "expecting ident statement found: {:?}",
                            self.current_token()
                        ))));
                    }
                }
            }

            TokenType::Asterix | TokenType::DoubleRightAngle => {
                self.forward_token();
                let deref = self.expect_expr()?;
                let deref = self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::Deref(deref.id)),
                    self.current_line(),
                    self.current_col(),
                );
                self.expect_token(TokenType::Equal)?;
                self.forward_token();
                let expr = self.expect_expr()?;

                let node = self.new_node(
                    self.new_index(),
                    NodeData::Statement(Statement::Assign {
                        lhs: deref.id,
                        rhs: expr.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                );

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
                self.forward_token();
                self.expect_token(TokenType::OpenBrace)?;
                let body = self.expect_block()?;
                let node = self.new_node(
                    self.new_index(),
                    NodeData::Statement(Statement::While {
                        cond: cond.id,
                        body,
                    }),
                    self.current_line(),
                    self.current_col(),
                );
                return Ok(node);
            }

            TokenType::KeywordFor => {
                self.forward_token();
                self.expect_token(TokenType::OpenParen)?;
                self.forward_token();

                let starting_inside_paren = self.cur;
                match self.current_token().ty {
                    TokenType::Ident => {
                        // either for_c or foreach
                        self.forward_token();
                        match self.current_token().ty {
                            TokenType::Equal | TokenType::ColonEqual | TokenType::DoubleColon => {
                                // for _c
                                self.cur = starting_inside_paren;
                                return self.expect_for_c();
                            }
                            TokenType::Colon => {
                                self.cur = starting_inside_paren;
                                return self.expect_for_each();
                            }
                            TokenType::CloseParen => {
                                self.cur = starting_inside_paren;
                                return self.expect_for_each_implicit_iterator();
                            }
                            _ => {
                                return Err(self.report_error(ParseError::Unknown(format!(
                                    "invalid for syntax"
                                ))));
                            }
                        }
                    }
                    _ => {
                        self.cur = starting_inside_paren;
                        return self.expect_for_each_implicit_iterator();
                    }
                }
            }
            TokenType::KeywordReturn => {
                self.forward_token();
                let expr = self.expect_expr()?;
                self.forward_token();
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::Statement(Statement::Return(expr.id)),
                    self.current_line(),
                    self.current_col(),
                ))
            }

            TokenType::KeywordGoto => {
                self.forward_token();
                let label = self.expect_ident()?;
                return Ok(self.new_node(self.new_index(), NodeData::Statement(Statement::Goto(label.id)), self.current_line(), self.current_col()));
            }

            TokenType::KeywordContinue => {
                self.forward_token();
                return Ok(self.new_node(
                    self.new_index(),
                    NodeData::Statement(Statement::Continue),
                    self.current_line(),
                    self.current_col(),
                ));
            }

            TokenType::KeywordSwitch => {
                //TODO
                unreachable!();
            }

            TokenType::KeywordBreak => {
                self.forward_token();
                return Ok(self.new_node(
                    self.new_index(),
                    NodeData::Statement(Statement::Break),
                    self.current_line(),
                    self.current_col(),
                ));
            }

            _ => {
                return Err(self.report_error(ParseError::UnknownStatement(
                    self.current_token().ty.clone(),
                )));
            }
        }
    }

    fn expect_type_expression(&mut self) -> Result<Node> {
        match self.current_token().ty {
            TokenType::KeywordStruct => {
                self.forward_token();
                self.expect_token(TokenType::OpenBrace)?;
                self.forward_token();
                let mut fields = Vec::<NodeIndex>::new();
                loop {
                    if self.current_token().ty == TokenType::CloseBrace {
                        self.forward_token();
                        break;
                    }
                    let mut name = self.expect_ident()?;

                    self.expect_token(TokenType::Colon)?;
                    self.forward_token();
                    let ty = self.expect_type_expression()?;
                    let field = self.new_node(
                        self.new_index(),
                        NodeData::Statement(Statement::Decl {
                            name: name.id,
                            ty: ty.id.clone(),
                        }),
                        self.current_line(),
                        self.current_col(),
                    );
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
                let node = self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::Struct(fields.clone())),
                    self.current_line(),
                    self.current_col(),
                );
                Ok(node)
            }
            TokenType::KeywordEnum => {
                self.forward_token();
                self.expect_token(TokenType::OpenBrace)?;
                self.forward_token();
                let mut variants = Vec::<NodeIndex>::new();
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
                    let ty_node = self.new_node(
                        self.new_index(),
                        NodeData::TypeDefinition(TypeDefinition::Uint(64)),
                        self.current_line(),
                        self.current_col(),
                    );
                    let variant = self.new_node(
                        self.new_index(),
                        NodeData::Statement(Statement::Decl {
                            name: name.id,
                            ty: ty_node.id.clone(),
                        }),
                        self.current_line(),
                        self.current_col(),
                    );

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
                let node = self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::Enum(variants.clone())),
                    self.current_line(),
                    self.current_col(),
                );
                Ok(node)
            }
            TokenType::OpenBracket => {
                // array type
                self.forward_token();
                let len = self.expect_expr()?;
                self.expect_token(TokenType::CloseBracket)?;
                self.forward_token();
                let ty = self.expect_type_expression()?;
                let node = self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::Array {
                        length: len.id,
                        elem_ty: ty.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                );
                return Ok(node);
            }
            TokenType::Ident => {
                self.forward_token();
                match self.current_token().ty {
                    _ => {
                        self.backward_token();
                        let name = self.expect_ident()?;
                        return Ok(name);
                    }
                }
            }
            TokenType::KeywordVoid => {
                self.forward_token();
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::Void),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::KeywordInt => {
                self.forward_token();
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::Int(64)),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::KeywordInt8 => {
                self.forward_token();
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::Int(8)),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::KeywordInt16 => {
                self.forward_token();
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::Int(16)),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::KeywordInt32 => {
                self.forward_token();
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::Int(32)),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::KeywordInt64 => {
                self.forward_token();
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::Int(64)),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::KeywordInt128 => {
                self.forward_token();
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::Int(128)),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::KeywordUint => {
                self.forward_token();
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::Uint(64)),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::KeywordUint8 => {
                self.forward_token();
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::Uint(8)),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::KeywordUint16 => {
                self.forward_token();
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::Uint(16)),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::KeywordUint32 => {
                self.forward_token();
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::Uint(32)),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::KeywordUint64 => {
                self.forward_token();
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::Uint(64)),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::KeywordUint128 => {
                self.forward_token();
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::Uint(128)),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::KeywordFloat32 => {
                self.forward_token();
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::Uint(32)),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::KeywordFloat64 => {
                self.forward_token();
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::Uint(64)),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::KeywordChar => {
                self.forward_token();
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::Char),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::KeywordBool => {
                self.forward_token();
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::Bool),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::UintPtrDirective => {
                self.forward_token();
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::UintPtr),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::IntPtrDirective => {
                self.forward_token();
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::IntPtr),
                    self.current_line(),
                    self.current_col(),
                ))
            }

            TokenType::KeywordString => {
                self.forward_token();
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::String),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::Asterix | TokenType::DoubleRightAngle => {
                self.forward_token();
                let expr = self.expect_type_expression()?;
                return Ok(self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::Pointer(expr.id)),
                    self.current_line(),
                    self.current_col(),
                ));
            }
            TokenType::OpenParen => {
                let before_check_fn_def_cur = self.cur;
                if self.is_fn_def() {
                    self.cur = before_check_fn_def_cur;
                    return Ok(self.expect_function_definition()?);
                }

                self.cur = before_check_fn_def_cur;
                self.forward_token();
                let expr = self.expect_type_expression()?;
                self.expect_token(TokenType::CloseParen)?;
                self.forward_token();
                Ok(expr)
            }
            TokenType::OpenBracket => {
                self.forward_token();
                let len = self.expect_expr()?;
                self.expect_token(TokenType::CloseBracket)?;
                self.forward_token();
                let ty = self.expect_type_expression()?;
                let node = self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::Array {
                        length: len.id,
                        elem_ty: ty.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                );
                return Ok(node);
            }
            TokenType::CVarArgsDirective => {
                self.forward_token();
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::CVarArgs),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::CString => {
                self.forward_token();
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::CString),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            _ => {
                return Err(self.report_error(ParseError::UnknownExpression(
                    self.current_token().ty.clone(),
                )));
            }
        }
    }

    pub fn get_ast(mut self) -> Result<IR> {
        let block_id = self.expect_block()?;
        self.ir.root = block_id;
        Ok(self.ir)
    }
}

// this impl block handles all expression and operator precedence.
impl Parser {
    fn expect_expr(&mut self) -> Result<Node> {
        match self.current_token().ty {
            TokenType::KeywordStruct
            | TokenType::KeywordEnum
            | TokenType::KeywordInt
            | TokenType::KeywordInt8
            | TokenType::KeywordInt16
            | TokenType::KeywordInt32
            | TokenType::KeywordInt64
            | TokenType::KeywordInt128
            | TokenType::KeywordUint
            | TokenType::KeywordUint8
            | TokenType::KeywordUint16
            | TokenType::KeywordUint32
            | TokenType::KeywordUint64
            | TokenType::KeywordUint128
            | TokenType::KeywordString
            | TokenType::KeywordFloat32
            | TokenType::KeywordFloat64
            | TokenType::KeywordVoid
            | TokenType::KeywordChar => {
                return self.expect_type_expression();
            }
            _ => {}
        }
        return self.expect_or();
    }
    fn expect_or(&mut self) -> Result<Node> {
        let lhs = self.expect_and()?;
        match self.current_token().ty {
            TokenType::DoublePipe => {
                self.forward_token();
                let rhs = self.expect_and()?;

                return Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::BinaryOperation {
                        operation: BinaryOperation::BinaryOr,
                        left: lhs.id,
                        right: rhs.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                ));
            }
            _ => {
                return Ok(lhs);
            }
        }
    }
    fn expect_and(&mut self) -> Result<Node> {
        let lhs = self.expect_bitwise_or()?;
        match self.current_token().ty {
            TokenType::DoubleAmpersand => {
                self.forward_token();
                let rhs = self.expect_bitwise_or()?;

                return Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::BinaryOperation {
                        operation: BinaryOperation::BinaryAnd,
                        left: lhs.id,
                        right: rhs.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                ));
            }
            _ => {
                return Ok(lhs);
            }
        }
    }
    fn expect_bitwise_or(&mut self) -> Result<Node> {
        let lhs = self.expect_bitwise_xor()?;
        match self.current_token().ty {
            TokenType::Pipe => {
                self.forward_token();
                let rhs = self.expect_bitwise_xor()?;

                return Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::BinaryOperation {
                        operation: BinaryOperation::BitwiseOr,
                        left: lhs.id,
                        right: rhs.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                ));
            }
            _ => {
                return Ok(lhs);
            }
        }
    }
    fn expect_bitwise_xor(&mut self) -> Result<Node> {
        let lhs = self.expect_bitwise_and()?;
        match self.current_token().ty {
            TokenType::Hat => {
                self.forward_token();
                let rhs = self.expect_bitwise_and()?;

                return Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::BinaryOperation {
                        operation: BinaryOperation::BitwiseXor,
                        left: lhs.id,
                        right: rhs.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                ));
            }
            _ => {
                return Ok(lhs);
            }
        }
    }
    fn expect_bitwise_and(&mut self) -> Result<Node> {
        let lhs = self.expect_equality_check()?;
        match self.current_token().ty {
            TokenType::Ampersand => {
                self.forward_token();
                let rhs = self.expect_equality_check()?;

                return Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::BinaryOperation {
                        operation: BinaryOperation::BitwiseAnd,
                        left: lhs.id,
                        right: rhs.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                ));
            }
            _ => {
                return Ok(lhs);
            }
        }
    }
    fn expect_equality_check(&mut self) -> Result<Node> {
        let lhs = self.expect_comparisons()?;
        match self.current_token().ty {
            TokenType::DoubleEqual => {
                self.forward_token();
                let rhs = self.expect_comparisons()?;

                return Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::BinaryOperation {
                        operation: BinaryOperation::Equal,
                        left: lhs.id,
                        right: rhs.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                ));
            }
            TokenType::NotEqual => {
                self.forward_token();
                let rhs = self.expect_comparisons()?;

                return Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::BinaryOperation {
                        operation: BinaryOperation::NotEqual,
                        left: lhs.id,
                        right: rhs.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                ));
            }
            _ => {
                return Ok(lhs);
            }
        }
    }
    fn ast_op_from_token_type(&mut self, op: TokenType) -> Result<BinaryOperation> {
        match op {
            TokenType::GreaterEqual => Ok(BinaryOperation::GreaterEqual),
            TokenType::LessEqual => Ok(BinaryOperation::LessEqual),
            TokenType::LeftAngle => Ok(BinaryOperation::Less),
            TokenType::RightAngle => Ok(BinaryOperation::Greater),
            TokenType::DoubleEqual => Ok(BinaryOperation::Equal),
            TokenType::NotEqual => Ok(BinaryOperation::NotEqual),
            TokenType::DoublePipe => Ok(BinaryOperation::BinaryOr),
            TokenType::DoubleAmpersand => Ok(BinaryOperation::BinaryAnd),
            _ => {
                return Err(self.report_error(ParseError::UnknownOperation(op)));
            }
        }
    }
    fn expect_comparisons(&mut self) -> Result<Node> {
        let lhs = self.expect_bitwise_shift()?;
        match self.current_token().ty {
            TokenType::GreaterEqual
            | TokenType::RightAngle
            | TokenType::LeftAngle
            | TokenType::LessEqual => {
                let op = self.ast_op_from_token_type(self.current_token().ty.clone())?;
                self.forward_token();
                let rhs = self.expect_bitwise_shift()?;
                return Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::BinaryOperation {
                        operation: op,
                        left: lhs.id,
                        right: rhs.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                ));
            }
            _ => {
                return Ok(lhs);
            }
        }
    }
    fn expect_bitwise_shift(&mut self) -> Result<Node> {
        return self.expect_sum_subtract();
    }
    fn expect_sum_subtract(&mut self) -> Result<Node> {
        let mut lhs = self.expect_multiply_division_modulo()?;
        match self.current_token().ty {
            TokenType::Minus => {
                self.forward_token();
                let rhs = self.expect_multiply_division_modulo()?;
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::BinaryOperation {
                        operation: BinaryOperation::Subtract,
                        left: lhs.id,
                        right: rhs.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::Plus => {
                self.forward_token();
                let rhs = self.expect_multiply_division_modulo()?;
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::BinaryOperation {
                        operation: BinaryOperation::Sum,
                        left: lhs.id,
                        right: rhs.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                ))
            }

            _ => Ok(lhs),
        }
    }
    fn expect_multiply_division_modulo(&mut self) -> Result<Node> {
        let mut lhs = self.expect_size_deref_ref_not()?;

        match self.current_token().ty {
            TokenType::Asterix => {
                self.forward_token();
                let rhs = self.expect_size_deref_ref_not()?;
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::BinaryOperation {
                        operation: BinaryOperation::Multiply,
                        left: lhs.id,
                        right: rhs.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::Percent => {
                self.forward_token();
                let rhs = self.expect_size_deref_ref_not()?;
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::BinaryOperation {
                        operation: BinaryOperation::Modulu,
                        left: lhs.id,
                        right: rhs.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::ForwardSlash => {
                self.forward_token();
                let rhs = self.expect_size_deref_ref_not()?;
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::BinaryOperation {
                        operation: BinaryOperation::Divide,
                        left: lhs.id,
                        right: rhs.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            _ => Ok(lhs),
        }
    }
    fn expect_size_deref_ref_not(&mut self) -> Result<Node> {
        match self.current_token().ty {
            TokenType::Bang => {
                self.forward_token();
                let to_be_not_value = self.expect_expr()?;
                return Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::UnaryOperation {
                        operator: UnaryOperation::Not,
                        expr: to_be_not_value.id.clone(),
                    }),
                    self.current_line(),
                    self.current_col(),
                ));
            }

            TokenType::Asterix | TokenType::DoubleLeftAngle => {
                self.forward_token();
                let expr = self.expect_expr()?;
                return Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::Deref(expr.id)),
                    self.current_line(),
                    self.current_col(),
                ));
            }
            TokenType::Ampersand | TokenType::DoubleRightAngle => {
                self.forward_token();
                let expr = self.expect_expr()?;
                return Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::PointerOf(expr.id)),
                    self.current_line(),
                    self.current_col(),
                ));
            }
            _ => {
                return self.expect_initialize_namespace_access_array_index_function_call();
            }
        }
    }
    fn expect_initialize_namespace_access_array_index_function_call(&mut self) -> Result<Node> {
        let value = self.expect_values()?;
        match self.current_token().ty {
            TokenType::OpenParen => {
                match value.data {
                    NodeData::Expression(Expression::Identifier(function_name)) => {
                        match function_name.as_str() {
                            "cast" => {
                                self.ir.nodes.remove(&value.id.clone());
                                self.forward_token();
                                let cast_expr = self.expect_expr()?;
                                self.expect_token(TokenType::Comma);
                                self.forward_token();
                                let cast_type = self.expect_type_expression()?;
                                self.expect_token(TokenType::CloseParen)?;
                                self.forward_token();
                                return Ok(self.new_node(
                                    self.new_index(),
                                    NodeData::Expression(Expression::Cast(
                                        cast_expr.id,
                                        cast_type.id,
                                    )),
                                    self.current_line(),
                                    self.current_col(),
                                ));
                            }
                            "sizeof" => {
                                self.ir.nodes.remove(&value.id.clone());
                                self.forward_token();
                                let sizeof_expr = self.expect_type_expression()?;
                                self.expect_token(TokenType::CloseParen)?;
                                self.forward_token();
                                return Ok(self.new_node(
                                    self.new_index(),
                                    NodeData::Expression(Expression::SizeOf(sizeof_expr.id)),
                                    self.current_line(),
                                    self.current_col(),
                                ));
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
                //function call
                self.ir.nodes.remove(&value.id.clone());
                self.backward_token();
                self.expect_fn_call()
            }
            TokenType::OpenBrace => {
                self.forward_token();
                let mut fields = Vec::<(NodeIndex, NodeIndex)>::new();
                loop {
                    if self.current_token().ty == TokenType::CloseBrace {
                        self.forward_token();
                        break;
                    }

                    let mut name = self.expect_ident()?;
                    self.expect_token(TokenType::Equal)?;
                    self.forward_token();
                    let value = self.expect_expr()?;
                    let mut name = self.ir.nodes.get_mut(&name.id.clone()).unwrap();
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
                    self.new_index(),
                    NodeData::Expression(Expression::Initialize {
                        ty: value.id.clone(),
                        fields,
                    }),
                    self.current_line(),
                    self.current_col(),
                ));
            }
            TokenType::OpenBracket => {
                self.forward_token();
                let index = self.expect_expr()?;
                self.expect_token(TokenType::CloseBracket)?;
                self.forward_token();
                return Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::ArrayIndex {
                        arr: value.id,
                        idx: index.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                ));
            }
            TokenType::Dot => {
                self.forward_token();
                let field = self.expect_ident()?;
                let f = self.ir.nodes.get_mut(&field.id).unwrap();
                return Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::NamespaceAccess {
                        namespace: value.id,
                        field: field.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                ));
            }

            _ => {
                return Ok(value);
            }
        }
    }
    fn expect_values(&mut self) -> Result<Node> {
        match self.current_token().ty {
            TokenType::UnsignedInt => {
                self.forward_token();
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0..=src_range.loc.1];
                let literal = match literal.parse::<u64>() {
                    Ok(n) => n,
                    Err(_) => {
                        return Err(self.report_error(ParseError::InvalidUnsignedInt(
                            self.current_token().clone(),
                        )))
                    }
                };
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::Unsigned(literal)),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::Float => {
                self.forward_token();
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0..=src_range.loc.1];
                let literal = match literal.parse::<f64>() {
                    Ok(n) => n,
                    Err(_) => {
                        return Err(self
                            .report_error(ParseError::InvalidFloat(self.current_token().clone())))
                    }
                };
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::Float(literal)),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::StringLiteral => {
                self.forward_token();
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0..=src_range.loc.1];
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::StringLiteral(literal.to_string())),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::KeywordTrue => {
                self.forward_token();
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0..=src_range.loc.1];
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::Bool(literal == "true")),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::KeywordFalse => {
                self.forward_token();
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0..=src_range.loc.1];
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::Bool(literal == "true")),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::Char => {
                self.forward_token();
                let src_range = &self.tokens[self.cur - 1];
                let literal = &self.src[src_range.loc.0 + 1..=src_range.loc.1 - 1];
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::Char(literal.chars().next().unwrap())),
                    self.current_line(),
                    self.current_col(),
                ))
            }
            TokenType::Ident => {
                let name = self.expect_ident()?;
                return Ok(name);
            }
            TokenType::OpenParen => {
                let before_check_fn_def_cur = self.cur;
                if self.is_fn_def() {
                    self.cur = before_check_fn_def_cur;
                    return Ok(self.expect_function_definition()?);
                }

                self.cur = before_check_fn_def_cur;
                self.forward_token();
                let expr = self.expect_expr()?;
                self.expect_token(TokenType::CloseParen)?;
                self.forward_token();
                Ok(self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::Paren(expr.id.clone())),
                    expr.line,
                    expr.col,
                ))
            }
            TokenType::OpenBracket => {
                self.forward_token();
                let len = self.expect_expr()?;
                self.expect_token(TokenType::CloseBracket)?;
                self.forward_token();
                let ty = self.expect_type_expression()?;
                self.expect_token(TokenType::OpenBrace)?;
                self.forward_token();
                let array_type = self.new_node(
                    self.new_index(),
                    NodeData::TypeDefinition(TypeDefinition::Array {
                        length: len.id,
                        elem_ty: ty.id,
                    }),
                    self.current_line(),
                    self.current_col(),
                );
                let mut fields = Vec::<NodeIndex>::new();
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
                let node = self.new_node(
                    self.new_index(),
                    NodeData::Expression(Expression::InitializeArray {
                        ty: array_type.id,
                        elements: fields.clone(),
                    }),
                    self.current_line(),
                    self.current_col(),
                );
                return Ok(node);
            }
            _ => {
                return Err(self.report_error(ParseError::UnknownExpression(
                    self.current_token().ty.clone(),
                )));
            }
        }
    }
}

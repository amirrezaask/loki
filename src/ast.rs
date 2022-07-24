use crate::tokenizer::Token;
use crate::tokenizer::Tokenizer;
use crate::tokenizer::Type;
use anyhow::Result;

#[derive(Debug)]
pub enum Node {}

#[derive(Debug)]
pub struct AST {
    top_level: Vec<Node>,
}

impl AST {
    pub fn new(src: &str) -> Result<Self> {
        let mut top_level = Vec::<Node>::new();
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
        println!("tokens: {:?}", tokens);

        Ok(Self { top_level })
    }
}

#[test]
fn ast_get_tokens() {
    let ast = AST::new("const i = 0;");
    assert!(ast.is_err());
    let ast = ast.unwrap();
}

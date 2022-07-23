use crate::tokenizer::Tokenizer;

pub enum Node {}

pub struct AST {
    top_level: Vec<Node>,
}

impl AST {
    pub fn new(src: &str) -> Self {
        let mut top_level = Vec::<Node>::new();
        let tokenizer = Tokenizer::new(src);
        while () {}
        Self { top_level }
    }
}

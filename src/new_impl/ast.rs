pub enum Statement {
    Block(Vec<Statement>)
}

pub enum PrefixOperator {
    Negate,

    Ref,
    Deref,
}

pub enum InfixOperator {
    Sum,
    Subtract,
    Multiply,
    Mod,
    Divide,

    And,
    Or,
}

pub enum PostfixOperator {
    Index,
    Call{ 
        args: Vec<Expression>
    },
}

pub enum Expression {
    PrefixOperation {
        op: PrefixOperator,
        x: Box<Expression>,
    },

    InfixOperation {
        op: InfixOperator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },

    PostfixOperation {
        x: Box<Expression>,
        op: PostfixOperator,
    }
}

pub enum Node {
    Statement(Statement),
    Expression(Expression)
}
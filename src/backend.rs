use anyhow::Result;
trait Repr<T> where T: std::fmt::Display {
    fn represent() -> Result<String>;
}

/*
    impl Repr<ASTNode> for C {

    }

    impl Repr<ASTNode> for Go {

    } 

    impl Repr<ASTNode> for
 */
use crate::typer::Type;
use crate::ir::NodeIndex;


#[derive(Debug, PartialEq, Clone)]
pub struct Array {
    elem_ty: Type,
    size_node: NodeIndex,
}

#[derive(Debug, PartialEq, Clone)]
pub struct DynamicArray {
    elem_ty: Type,
}

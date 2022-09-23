use std::ops::Deref;

use crate::bytecode::{Module, Instruction, InstructionPayload, Value, ValuePayload, Expression};
use crate::errors::Result;
use crate::ir::{UnaryOperation, BinaryOperation};
use crate::typer::Type;

fn emit_for_unary_operation(op: &UnaryOperation) -> String {
    match op {
        UnaryOperation::Not => format!("!"),
    }
}
fn emit_for_binary_operation(op: &BinaryOperation) -> String {
    match op {
        BinaryOperation::Sum => "+".to_string(),
        BinaryOperation::Subtract => "-".to_string(),
        BinaryOperation::Divide => "/".to_string(),
        BinaryOperation::Modulu => "%".to_string(),
        BinaryOperation::Multiply => "*".to_string(),
        BinaryOperation::Greater => ">".to_string(),
        BinaryOperation::GreaterEqual => ">=".to_string(),
        BinaryOperation::Less => "<".to_string(),
        BinaryOperation::LessEqual => "<=".to_string(),
        BinaryOperation::Equal => "==".to_string(),
        BinaryOperation::NotEqual => "!=".to_string(),
        BinaryOperation::BitwiseOr => "|".to_string(),
        BinaryOperation::BitwiseAnd => "&".to_string(),
        BinaryOperation::BitwiseXor => "^".to_string(),
        BinaryOperation::BinaryAnd => "&&".to_string(),
        BinaryOperation::BinaryOr => "||".to_string(),
    }
}
fn emit_for_value(value: &Value) -> String {
    match value.payload {
        crate::bytecode::ValuePayload::Type(ref ty) => {
            return emit_for_type(&ty);
        },
        crate::bytecode::ValuePayload::Expression(ref expr) => {
            match expr {
                crate::bytecode::Expression::Cast { expr, ty } => {
                    return format!("({}) {}", emit_for_type(ty), emit_for_value(expr));
                }
                crate::bytecode::Expression::SizeOf(expr) => {
                    return format!("sizeof({})", emit_for_value(expr));
                }
                crate::bytecode::Expression::Unsigned(number) => format!("{}", number),
                crate::bytecode::Expression::Signed(number) => format!("{}", number),
                crate::bytecode::Expression::StringLiteral(ref s) => format!("\"{}\"", s),
                crate::bytecode::Expression::Float(number) => format!("{}", number),
                crate::bytecode::Expression::Bool(b) => {
                    format!("{}", b)
                }
                crate::bytecode::Expression::Char(c) => format!("'{}'", c),
                crate::bytecode::Expression::Identifier(i) => format!("{}", i),
                crate::bytecode::Expression::Paren(inner) => format!("({})", emit_for_value(inner)),
                crate::bytecode::Expression::UnaryOperation { operator, expr } => format!("{}{}", emit_for_unary_operation(operator), emit_for_value(expr)),
                crate::bytecode::Expression::BinaryOperation { operation, left, right } => format!("{} {} {}", emit_for_value(left), emit_for_binary_operation(operation), emit_for_value(right)),
                crate::bytecode::Expression::ArrayIndex { arr, idx } => format!("{}[{}]", emit_for_value(arr), emit_for_value(idx)),
                crate::bytecode::Expression::NamespaceAccess { namespace, field } => {
                    if namespace.ty.is_pointer() {
                        return format!("{}->{}", emit_for_value(namespace), emit_for_value(field));
                    }
                    return format!("{}.{}", emit_for_value(namespace), emit_for_value(field));
                }
                crate::bytecode::Expression::Call { ref fn_name, ref args } => {
                    let mut args_str: Vec<String> = vec![];
                    for arg in args {
                        args_str.push(emit_for_value(arg));
                    }

                    return format!("{}({})", emit_for_value(fn_name), args_str.join(", "));
                },
                crate::bytecode::Expression::PointerOf(ref obj) => format!("&({})", emit_for_value(obj)),
                crate::bytecode::Expression::Deref(ref pointer) => format!("*({})", emit_for_value(pointer)),
                crate::bytecode::Expression::Label(_) => unreachable!(),
                crate::bytecode::Expression::Function { args, ret, body } => unreachable!(),
            }
        },
    }
}

fn emit_ty_forward_decl(ty: &Type) -> String {
    match ty {
        Type::Label => "".to_string(),
        Type::NoType => "".to_string(),
        Type::Type(inner) => return emit_ty_forward_decl(inner),
        Type::SignedInt(_) => "int".to_string(),
        Type::UnsignedInt(_) => "unsigned int".to_string(),
        Type::Float(_) => "double".to_string(),
        Type::Bool => "bool".to_string(),
        Type::CIntPtr => "intptr".to_string(),
        Type::CUintPtr => "uintptr".to_string(),
        Type::CVarArgs => "...".to_string(),
        Type::CString => "*char".to_string(),
        Type::Char => "char".to_string(),
        Type::String => "char*".to_string(),
        Type::Array(_, ref elem_ty) => format!("{}[]", emit_ty_forward_decl(elem_ty)),
        Type::Struct { fields } => {
            return "struct".to_string();
        },
        Type::Enum { variants } => unreachable!(),
        Type::TypeRef { name, actual_ty } => format!("{}", name),
        Type::Pointer(obj) => format!("{}*", emit_ty_forward_decl(obj)),
        Type::FnType(ref args, ref ret) => {
            unreachable!();
        },
        Type::Void => format!("void"),
        Type::SizeOfCall => "".to_string(),
        Type::CastCall => "".to_string(),
    }
}

fn emit_for_type(ty: &Type) -> String {
    match ty {
        Type::Label => unreachable!(),
        Type::NoType => "".to_string(),
        Type::Type(inner) => return emit_for_type(inner),
        Type::SignedInt(_) => "int".to_string(),
        Type::UnsignedInt(_) => "unsigned int".to_string(),
        Type::Float(_) => "double".to_string(),
        Type::Bool => "bool".to_string(),
        Type::CIntPtr => "intptr".to_string(),
        Type::CUintPtr => "uintptr".to_string(),
        Type::CVarArgs => "...".to_string(),
        Type::CString => "*char".to_string(),
        Type::Char => "char".to_string(),
        Type::String => "char*".to_string(),
        Type::Array(size, ref elem_ty) => format!("{}[{}]", emit_for_type(elem_ty), size),
        Type::Struct { fields } => {
            let mut fields_str: Vec<String> = vec![];
            for (field, fty) in fields {
                fields_str.push(format!("{} {}", emit_for_type(fty), field));
            }
            return format!("struct {{{}}}", fields_str.join(";\n"));
        },
        Type::Enum { variants } => unreachable!(),
        Type::TypeRef { name, actual_ty } => format!("{}", name),
        Type::Pointer(obj) => format!("{}*", emit_for_type(obj)),
        Type::FnType(ref args, ref ret) => {
            unreachable!();
        },
        Type::Void => format!("void"),
        Type::SizeOfCall => "".to_string(),
        Type::CastCall => "".to_string(),
    }
}

fn emit_for_instruction(inst: &Instruction) -> String {
    match &inst.payload {
        InstructionPayload::JumpTrue { cond, label } => {
            return format!("if ({}) goto {};", emit_for_value(cond), label);
        }
        InstructionPayload::JumpFalse { cond, label } => {
            return format!("if (!({})) goto {};", emit_for_value(cond), label);
        }
        InstructionPayload::Host(ref path) => format!("#include <{}>", path),
        InstructionPayload::Definition { mutable, ref name, ref ty, ref value } => {
            if let ValuePayload::Expression(Expression::Function { ref args, ref ret, ref body }) = value.payload {
                let mut instructions = Vec::new();
                let mut argss = Vec::new();
                for inst in body {
                    instructions.push(format!("    {}", emit_for_instruction(inst)));
                }
                for (name, ty) in args {
                    argss.push(format!("{} {}", emit_for_type(ty), name));
                }
                return format!("{} {}({}) {{\n{}\n}}", emit_for_type(&ret), name, argss.join(", "), instructions.join("\n"));
            } else if let ValuePayload::Type(ref td) = value.payload {
                let td = td.deref().clone().to_owned();
                match td {
                    Type::Type(t) => {
                        match *t {
                            Type::Struct { ref fields } => {
                                let mut fields_str: Vec<String> = vec![];
                                for (field, fty) in fields {
                                    fields_str.push(format!("    {} {};", emit_for_type(fty), field));
                                }
                                return format!("typedef struct {} {{\n{}\n}} {};", name, fields_str.join("\n"), name);
                            },
                            _ => {}
                        }
                    },
                    _ => {}
                }
            }
            return format!("{} {} = {};", emit_for_type(ty), name.to_string(), emit_for_value(value))
        },
        InstructionPayload::Declaration { name, ty } => {
            if let Type::FnType(ref args, ref ret) = ty {
                let mut argss = Vec::new();
                for (name, ty) in args {
                    argss.push(format!("{} {}", emit_for_type(ty), name));
                }
                return format!("{} {}({});", emit_for_type(&ret), name, argss.join(", "));
            } else if let Type::Array(size, ref elem_ty) = ty {
                return format!("{} {}[{}];", emit_for_type(elem_ty), name, size);
            }
            return format!("{} {};", emit_for_type(ty), name);
        },
        InstructionPayload::Set { lhs, rhs } => format!("{} = {};", emit_for_value(lhs), emit_for_value(rhs)),
        InstructionPayload::Block {instructions: ref instructions } => {
            let mut code: Vec<String> = vec![];
            for instruction in instructions {
                code.push(emit_for_instruction(instruction));
            }
        
            return format!("{{\n{}\n}}", code.join("\n"));
        },
       
        InstructionPayload::Call { function, args } => {
            let mut argss = vec![];
            for arg in args {
                argss.push(emit_for_value(arg));
            }
            format!("{}({});", emit_for_value(function), argss.join(", "))
        },
        InstructionPayload::Jump(label) => format!("goto {};", label),
        InstructionPayload::Label(label) => format!("{}:;", label),
        InstructionPayload::Return(ref thing) => {
            format!("return ({});", emit_for_value(thing))
        } ,
    }
}

pub fn emit_for_module(module: Module) -> String {
    let mut code: Vec<String> = vec![];
    for instruction in &module.instructions {
        if let InstructionPayload::Host(_) = instruction.payload {
            code.push(emit_for_instruction(instruction));
        }
    }
    code.push("// LOKI GENERATED FORWARD DECLARATIONS".to_string());
    for instruction in &module.instructions {
        if let InstructionPayload::Definition { mutable, ref name, ref ty, ref value } = instruction.payload {
            if ty.is_type_definition() {
                code.push(format!("{} {};", emit_ty_forward_decl(&ty), name));
            } else {
                if let Type::FnType(ref args, ref ret) = ty {
                    let mut argss = Vec::new();
                    for (name, ty) in args {
                        argss.push(format!("{} {}", emit_for_type(ty), name));
                    }
                    code.push(format!("{} {}({});", emit_for_type(&ret), name, argss.join(", ")));
                }
            }
        }
    }
    code.push("// LOKI GENERATED FORWARD DECLARATIONS".to_string());
    for instruction in &module.instructions {
        if let InstructionPayload::Definition { mutable: _, name: _, ty: _, ref value } = instruction.payload {
            if value.ty.is_type_definition() {
                code.push(emit_for_instruction(instruction));
            }
        }
    }


    for instruction in &module.instructions {
        if let InstructionPayload::Definition { mutable: _, name: _, ty: _, ref value } = instruction.payload {
            if !value.ty.is_type_definition() {
                code.push(emit_for_instruction(instruction));
            }
        }
    }
    return code.join("\n");

}
const std = @import("std");
const Self = @This();
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Backend = @import("CodeGen.zig");
const Error = error{};

fn inferType(expr: *Node) []const u8 {
    switch (expr.data) {
        .int => {
            return "int";
        },
        .uint => {
            return "unsigned int";
        },
        .float => {
            return "float";
        },
        .@"bool" => {
            return "bool";
        },
        .char => {
            return "char";
        },
        .string_literal => {
            return "char[]";
        },
    }
}

fn generateFnArgs(alloc: std.mem.Allocator, args: [][2]*Node) []const u8 {
    var i: u16 = 0;
    var list = std.ArrayList(u8).init(alloc);

    while (i < args.len) : (i += 1) {
        list.appendSlice(std.fmt.format("{} {},", .{ args[i][1], args[i][0] }));
    }

    _ = list.pop();

    return list.toOwnedSlice();
}

fn generateForDecl(node: *Node) ![]const u8 {
    switch (node.data.decl.val) {
        .fn_def => {
            const fn_def = node.data.decl.fn_def;
            const name = node.data.decl.name;
            const args = generateFnArgs(fn_def.signature.args);
            const ret_ty = generateForNode(fn_def.signature.ret_ty);
            const body = generateForNode(fn_def.block);

            return std.fmt.format("{} {}({}) {{\n{}\n}}", .{ ret_ty, name, args, body });
        },
        .@"struct" => {},
        .@"enum" => {},
        .@"union" => {},
        else => {
            const name = node.data.decl.name;
            const ty = inferType(node.data.decl.val);
            const val = generateForNode(node.data.decl.val);
            return std.fmt.format("{} {} = {}", .{ ty, name, val });
        },
    }
}

fn generateForNode(node: *Node) Error![]const u8 {
    switch (node.data) {
        .@"import" => {
            const import_path = node.data.import;
            return std.fmt.format("#include {}", .{import_path});
        },
        .@"decl" => {
            return generateForDecl(node);
        },
        .@"int" => {
            return std.fmt.format("{}", .{node.data.int});
        },
        .@"uint" => {
            return std.fmt.format("{}", .{node.data.uint});
        },
        .@"float" => {
            return std.fmt.format("{}", .{node.data.float});
        },
        .@"string_literal" => {
            return std.fmt.format("{}", .{node.data.string_literal});
        },
        .@"bool" => {
            return std.fmt.format("{}", .{node.data.@"bool"});
        },
        .@"identifier" => {
            return std.fmt.format("{}", .{node.data.@"identifier"});
        },
        .@"char" => {
            return std.fmt.format("'{}'", .{node.data.@"char"});
        },
        .@"paren_expr" => {
            unreachable;
        },
        .@"type_def_struct" => {
            unreachable;
        },
        .@"type_def_enum" => {
            unreachable;
        },
        .@"type_def_union" => {
            unreachable;
        },
        .@"increment" => {
            unreachable;
        },
        .@"decrement" => {
            unreachable;
        },
        .@"fn_call" => {
            unreachable;
        },
        .@"fn_sign" => {
            unreachable;
        },
        .@"fn_def" => {
            const sign = generateForNode(node.data.fn_def.signature);
            const body = generateForNode(node.data.@"fn_def".block);
            return std.fmt.format("{} {{\n{}\n}}", sign, body);
        },
        .@"bool_ty" => {
            return "bool";
        },
        .@"int_ty" => {
            return "int";
        },
        .@"uint_ty" => {
            return "unsigned int";
        },
        .@"char_ty" => {
            return "char";
        },
        .@"void_ty" => {
            return "void";
        },
        .@"float_ty" => {
            return "float";
        },
        .@"string_ty" => {
            return "char[]";
        },
    }
}

fn generate(ast: Ast) Error![]const u8 {
    _ = ast;
}

fn getBackend() Backend {
    return Backend.init(generate);
}

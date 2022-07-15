const std = @import("std");
const Self = @This();
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Backend = @import("CodeGen.zig");
const Error = error{};

fn allocPrint(allocator: std.mem.Allocator, comptime fmt: []const u8, args: anytype) []u8 {
    return std.fmt.allocPrint(allocator, fmt, args) catch unreachable;
}
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
        else => {
            unreachable;
        },
    }
}

fn generateFnArgs(alloc: std.mem.Allocator, args: [][2]*Node) []const u8 {
    var i: u16 = 0;
    var list = std.ArrayList(u8).init(alloc);

    while (i < args.len) : (i += 1) {
        list.appendSlice(allocPrint(alloc, "{s} {s},", .{ args[i][1], args[i][0] })) catch unreachable;
    }

    _ = list.pop();

    return list.toOwnedSlice();
}

fn generateForBlock(alloc: std.mem.Allocator, block: []*Node) []const u8 {
    _ = alloc;
    _ = block;
    return "";
}

fn generateForDecl(alloc: std.mem.Allocator, node: *Node) []const u8 {
    switch (node.data.decl.val.data) {
        .fn_def => {
            const fn_def = node.data.decl.val.data.fn_def;
            const name = node.data.decl.name;
            const args = generateFnArgs(alloc, fn_def.signature.args);
            const ret_ty = generateForNode(alloc, fn_def.signature.ret_ty);
            const body = generateForBlock(alloc, fn_def.block);

            return allocPrint(alloc, "{s} {s}({s}) {{\n{s}\n}}", .{ ret_ty, name, args, body });
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
        else => {
            const name = node.data.decl.name;
            const ty = inferType(node.data.decl.val);
            const val = generateForNode(alloc, node.data.decl.val);
            return allocPrint(alloc, "{s} {s} = {s}", .{ ty, name, val });
        },
    }
}

fn generateForNode(alloc: std.mem.Allocator, node: *Node) []const u8 {
    switch (node.data) {
        .@"import" => {
            const import_path = node.data.import;
            return allocPrint(alloc, "#include \"{s}\"", .{import_path});
        },
        .@"decl" => {
            return generateForDecl(alloc, node);
        },
        .@"int" => {
            return allocPrint(alloc, "{}", .{node.data.int});
        },
        .@"uint" => {
            return allocPrint(alloc, "{}", .{node.data.uint});
        },
        .@"float" => {
            return allocPrint(alloc, "{}", .{node.data.float});
        },
        .@"string_literal" => {
            return allocPrint(alloc, "{s}", .{node.data.string_literal});
        },
        .@"bool" => {
            return allocPrint(alloc, "{}", .{node.data.@"bool"});
        },
        .@"identifier" => {
            return allocPrint(alloc, "{s}", .{node.data.@"identifier"});
        },
        .@"char" => {
            return allocPrint(alloc, "'{c}'", .{node.data.@"char"});
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
            unreachable;
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
        else => {
            unreachable;
        },
    }
}

fn generate(ast: Ast) Error![]const u8 {
    _ = ast;
}

fn getBackend() Backend {
    return Backend.init(generate);
}

test "import node should generate a #include" {
    var node = Node{
        .data = .{ .import = "stdio.h" },
        .loc = .{ .start = 0, .end = 0 },
    };

    const include_str = generateForNode(std.testing.allocator, &node);
    try std.testing.expectEqualStrings("#include \"stdio.h\"", include_str);
    defer std.testing.allocator.free(include_str);
}

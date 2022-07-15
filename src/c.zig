const std = @import("std");
const Self = @This();
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Backend = @import("CodeGen.zig");
const Error = error{};
const Parser = @import("Parser.zig");

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

fn generateForFnCallArgs(alloc: std.mem.Allocator, args: []*Node) []const u8 {
    var i: u16 = 0;
    var list = std.ArrayList(u8).init(alloc);

    while (i < args.len) : (i += 1) {
        const arg_val = generateForNode(alloc, args[i]);
        const arg = allocPrint(alloc, "{s},", .{arg_val});
        list.appendSlice(arg) catch unreachable;
        defer alloc.free(arg);
        defer alloc.free(arg_val);
    }
    if (list.items.len < 1) {
        return "";
    }

    _ = list.pop();

    return list.toOwnedSlice();
}

fn generateFnDefArgs(alloc: std.mem.Allocator, args: [][2]*Node) []const u8 {
    var i: u16 = 0;
    var list = std.ArrayList(u8).init(alloc);

    while (i < args.len) : (i += 1) {
        const arg = allocPrint(alloc, "{s} {s},", .{ args[i][1], args[i][0] });
        list.appendSlice(arg) catch unreachable;
        defer alloc.free(arg);
    }
    if (list.items.len < 1) {
        return "";
    }

    _ = list.pop();

    return list.toOwnedSlice();
}

fn generateForBlock(alloc: std.mem.Allocator, block: []*Node) []const u8 {
    var i: u16 = 0;
    var list = std.ArrayList(u8).init(alloc);

    while (i < block.len) : (i += 1) {
        const stmt = generateForNode(alloc, block[i]);
        list.appendSlice(stmt) catch unreachable;
        list.append(';') catch unreachable;
        defer alloc.free(stmt);
    }

    if (list.items.len < 1) {
        return "";
    }

    return list.toOwnedSlice();
}

fn generateForDecl(alloc: std.mem.Allocator, node: *Node) []const u8 {
    switch (node.data.decl.val.data) {
        .fn_def => {
            const fn_def = node.data.decl.val.data.fn_def;
            const name = node.data.decl.name;
            const args = generateFnDefArgs(alloc, fn_def.signature.args);
            const ret_ty = generateForNode(alloc, fn_def.signature.ret_ty);
            const body = generateForBlock(alloc, fn_def.block);

            defer alloc.free(body);

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
            return allocPrint(alloc, "\"{s}\"", .{node.data.string_literal});
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
            const fn_call = node.data.fn_call;
            const name = generateForNode(alloc, fn_call.name);
            const args = generateForFnCallArgs(alloc, fn_call.args);
            defer alloc.free(name);
            defer alloc.free(args);

            return allocPrint(alloc, "{s}({s})", .{ name, args });
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

test "decl fn def" {
    var parser = try Parser.init(std.testing.allocator, "main :: fn() void {\nprintf(\"Hello world\");\n}");
    defer parser.deinit();
    var ast = try parser.getAst(std.testing.allocator);
    defer ast.deinit(std.testing.allocator);
    var main_decl = ast.top_level.items[0];
    const code = generateForNode(std.testing.allocator, &main_decl);
    defer std.testing.allocator.free(code);

    try std.testing.expectEqualStrings("void main() {\nprintf(\"Hello world\");\n}", code);
}

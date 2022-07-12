const std = @import("std");
const print = std.debug.print;
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Decl = Ast.Decl;
const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;
const Self = @This();
const Stack = @import("stack.zig").Stack;
const State = enum {
    start,
    block,
    import,
    import_waiting_for_string,
    waiting_for_expr,
    saw_identifier,
    var_decl,
    const_decl,
    saw_if,
    saw_fn,
};

const Error = error{
    expects_semicolon,
};
fn strEql(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}
alloc: std.mem.Allocator,
src: []const u8,
cur: u64,
state: State = .start,
tokens: std.ArrayList(Token),

pub fn init(alloc: std.mem.Allocator, src: []const u8) Self {
    return .{
        .cur = 0,
        .src = src,
        .tokens = undefined,
        .alloc = alloc,
    };
}

fn forwardToken(self: *Self) void {
    self.cur += 1;
}

fn curToken(self: *Self) Token {
    return self.tokens.items[self.cur];
}

fn expectDecl(self: *Self) !Decl {
    // either const or var decls
    // this will parse until semicolon and end of value expression.
    const ident_token = self.curToken();

    self.forwardToken();
    var decl_ty: Ast.Decl.Tag = undefined;

    switch (self.curToken().ty) {
        .double_colon => {
            decl_ty = .@"const";
        },
        .equal => {
            decl_ty = .@"const";
        },
        else => {
            unreachable;
            // compile error;
        },
    }

    self.forwardToken();
    const expr = self.expectExpr();
    const objects = try self.alloc.alloc(Node, 1);
    objects[0] = expr;

    return Ast.Decl{
        .name = ident_token.val.identifier,
        .ty = decl_ty,
        .val = &objects[0],
    };
}

fn expectExpr(self: *Self) Node {
    var node: Node = undefined;
    switch (self.curToken().ty) {
        .unsigned_int => {
            node = .{
                .data = .{ .unsigned_int = self.curToken().val.unsigned_int },
                .loc = self.curToken().loc,
            };
            self.forwardToken();
        },
        .keyword_true => {
            node = .{
                .data = .{ .@"bool" = true },
                .loc = self.curToken().loc,
            };
            self.forwardToken();
        },
        .keyword_false => {
            node = .{
                .data = .{ .@"bool" = false },
                .loc = self.curToken().loc,
            };
            self.forwardToken();
        },
        .char => {
            node = .{
                .data = .{ .@"char" = self.curToken().val.char },
                .loc = self.curToken().loc,
            };
            self.forwardToken();
        },
        .string_literal => {
            node = .{
                .data = .{ .@"string_literal" = self.curToken().val.string_literal },
                .loc = self.curToken().loc,
            };
            self.forwardToken();
        },
        .identifier => {
            node = .{
                .data = .{ .@"identifier" = self.curToken().val.identifier },
                .loc = self.curToken().loc,
            };
            self.forwardToken();
        },
        else => {
            print("expectExpr ty: {}", .{self.curToken().ty});
            unreachable;
        },
    }
    // we expect all our switch cases to move until semicolon and stop on it so by
    // going to next token we should give control back to the main loop.
    if (self.curToken().ty != .semi_colon) {
        // compile error
        unreachable;
    }
    self.forwardToken();
    return node;
}

fn expectSemiColon(self: *Self) !void {
    if (self.curToken().ty == .semi_colon) {
        return;
    } else {
        return Error.expects_semicolon;
    }
}

fn expectImport(self: *Self) ![]const u8 {
    self.forwardToken();

    switch (self.curToken().ty) {
        .string_literal => {
            const import_string = self.curToken().val.string_literal;
            self.forwardToken();
            try self.expectSemiColon();
            self.forwardToken();
            return import_string;
        },

        else => {
            unreachable;
        },
    }
}

pub fn getAst(self: *Self, alloc: std.mem.Allocator) !Ast {
    var tokenizer = Tokenizer.init(self.src);
    self.tokens = std.ArrayList(Token).init(alloc);
    defer self.tokens.deinit();
    while (true) {
        const token = try tokenizer.next();
        if (token.ty == .EOF) break;
        try self.tokens.append(token);
    }

    var ast = Ast.init(alloc);

    while (true) {
        if (self.cur >= self.tokens.items.len) {
            break;
        }
        if (self.state == .start) {
            print("\n{}\n", .{self.curToken()});
            switch (self.curToken().ty) {
                .identifier => {
                    const decl = try self.expectDecl();
                    try ast.addTopLevelNode(.{
                        .data = .{ .decl = decl },
                        .loc = decl.val.loc,
                    });
                },

                .keyword_import => {
                    const import_str = try self.expectImport();
                    try ast.addTopLevelNode(.{
                        .data = .{ .import = import_str },
                        .loc = self.curToken().loc,
                    });
                },

                else => {
                    unreachable;
                },
            }
        } else {
            unreachable;
            //compile error
        }
    }

    return ast;
}

test "all static expressions" {
    var parser = Self.init(std.testing.allocator,
        \\import "std.loki";
        \\a :: 2;
        \\b :: "salam";
        \\c :: 'c';
        \\d :: true;
        \\e :: false;
        \\f :: a;
    );

    var ast = try parser.getAst(std.testing.allocator);
    defer ast.deinit(std.testing.allocator);
    try std.testing.expectEqualStrings("std.loki", ast.top_level.items[0].data.@"import");

    try std.testing.expectEqualStrings("a", ast.top_level.items[1].data.@"decl".name);
    try std.testing.expectEqual(@as(u64, 2), ast.top_level.items[1].data.@"decl".val.data.@"unsigned_int");

    try std.testing.expectEqualStrings("b", ast.top_level.items[2].data.@"decl".name);
    try std.testing.expectEqualStrings("salam", ast.top_level.items[2].data.@"decl".val.data.@"string_literal");

    try std.testing.expectEqualStrings("c", ast.top_level.items[3].data.@"decl".name);
    try std.testing.expectEqual(@as(u8, 'c'), ast.top_level.items[3].data.@"decl".val.data.@"char");

    try std.testing.expectEqualStrings("d", ast.top_level.items[4].data.@"decl".name);
    try std.testing.expectEqual(true, ast.top_level.items[4].data.@"decl".val.data.@"bool");

    try std.testing.expectEqualStrings("e", ast.top_level.items[5].data.@"decl".name);
    try std.testing.expectEqual(false, ast.top_level.items[5].data.@"decl".val.data.@"bool");
    try std.testing.expectEqualStrings("f", ast.top_level.items[6].data.@"decl".name);
    try std.testing.expectEqualStrings("a", ast.top_level.items[6].data.@"decl".val.data.identifier);
}

// test "hello world program" {
//     var parser = Self.init(
//         \\import "std.loki";
//         \\main :: fn() void {
//         \\     printf("Hello World from loki");
//         \\};
//     );

//     const ast = try parser.getAst(std.testing.allocator);
//     _ = ast;
// }

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

fn peekToken(self: *Self) Token {
    return self.tokens.items[self.cur + 1];
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
    const expr = try self.expectExpr();
    const objects = try self.alloc.alloc(Node, 1);
    objects[0] = expr;

    self.forwardToken(); // ;
    self.forwardToken(); // next start
    return Ast.Decl{
        .name = ident_token.val.identifier,
        .ty = decl_ty,
        .val = &objects[0],
    };
}

fn expectExpr(self: *Self) !Node {
    var node: Node = undefined;
    switch (self.curToken().ty) {
        .unsigned_int => {
            node = .{
                .data = .{ .unsigned_int = self.curToken().val.unsigned_int },
                .loc = self.curToken().loc,
            };
        },
        .keyword_true => {
            node = .{
                .data = .{ .@"bool" = true },
                .loc = self.curToken().loc,
            };
        },
        .keyword_false => {
            node = .{
                .data = .{ .@"bool" = false },
                .loc = self.curToken().loc,
            };
        },
        .char => {
            node = .{
                .data = .{ .@"char" = self.curToken().val.char },
                .loc = self.curToken().loc,
            };
        },
        .string_literal => {
            node = .{
                .data = .{ .@"string_literal" = self.curToken().val.string_literal },
                .loc = self.curToken().loc,
            };
        },
        .identifier => {
            node = .{
                .data = .{ .@"identifier" = self.curToken().val.identifier },
                .loc = self.curToken().loc,
            };
        },
        .keyword_fn => {
            node = try self.expectFnDef();
        },
        .keyword_bool => {
            node = .{
                .data = .bool_ty,
                .loc = self.curToken().loc,
            };
        },
        .keyword_int => {
            node = .{
                .data = .int_ty,
                .loc = self.curToken().loc,
            };
        },
        .keyword_float => {
            node = .{
                .data = .float_ty,
                .loc = self.curToken().loc,
            };
        },
        .keyword_uint => {
            node = .{
                .data = .uint_ty,
                .loc = self.curToken().loc,
            };
        },
        .keyword_string => {
            node = .{
                .data = .string_ty,
                .loc = self.curToken().loc,
            };
        },
        .keyword_char => {
            node = .{
                .data = .char_ty,
                .loc = self.curToken().loc,
            };
        },
        else => {
            print("expectExpr ty: {}", .{self.curToken().ty});
            unreachable;
        },
    }
    // we expect all our switch cases to stay at last token.
    // if (self.curToken().ty != .semi_colon) {
    //     // compile error
    //     unreachable;
    // }
    return node;
}

fn expectFnSignature(self: *Self) !Node {
    self.forwardToken(); // this will move us to open paren
    if (self.curToken().ty != .open_paren) {
        unreachable;
        // compile error
    }

    self.forwardToken();
    var args = std.ArrayList([2]*Node).init(self.alloc);

    while (true) {
        if (self.curToken().ty == .close_paren) break;
        if (self.curToken().ty == .identifier) {
            const ident: Node = .{
                .data = .{
                    .identifier = self.curToken().val.identifier,
                },
                .loc = self.curToken().loc,
            };

            self.forwardToken();
            const ty = try self.expectExpr();
            var objects = try self.alloc.alloc(Node, 2);
            objects[0] = ident;
            objects[1] = ty;
            try args.append([2]*Node{ &objects[0], &objects[1] });
        } else {
            unreachable;
            // compile error
        }
    }
    self.forwardToken();

    const ret_ty = try self.expectExpr();

    var objects = try self.alloc.alloc(Node, 1);
    objects[0] = ret_ty;

    objects = try self.alloc.alloc(Node, 1);
    objects[0] = ret_ty;

    return Node{
        .data = .{ .@"fn_sign" = .{
            .args = args.toOwnedSlice(),
            .ret_ty = &objects[0],
        } },
        .loc = self.curToken().loc,
    };
}

fn expectFnCall(self: *Self) !Ast.FnCall {
    const name = .{ .data = .{ .identifier = self.curToken().val.identifier }, .loc = self.curToken().loc };
    self.forwardToken();
    if (self.curToken().ty != .open_paren) unreachable;
    var args = std.ArrayList(Node).init(self.alloc);
    while (true) {
        const expr = try self.expectExpr();
        try args.append(expr);
    }

    return Ast.FnCall{
        .name = name,
        .args = args.toOwnedSlice(),
    };
}

fn expectBlock(self: *Self) ![]*Node {
    if (self.curToken().ty != .lcbrace) {
        // compile error
        unreachable;
    }

    var nodes = std.ArrayList(*Node).init(self.alloc);
    while (true) {
        if (self.curToken().ty == .rcbrace) break;
        switch (self.curToken().ty) {
            .identifier => {
                if (self.peekToken().ty == .open_paren) {
                    // function call
                    const node: Node = .{
                        .data = .{ .fn_call = try self.expectFnCall() },
                        .loc = self.curToken().loc,
                    };
                    var objects = try self.alloc.alloc(Node, 1);
                    objects[0] = node;
                    try nodes.append(&objects[0]);
                } else if (self.peekToken().ty == .double_colon or self.peekToken().ty == .equal) {
                    const decl = try self.expectDecl();
                    const node: Node = .{
                        .data = .{ .decl = decl },
                        .loc = decl.val.loc,
                    };
                    var objects = try self.alloc.alloc(Node, 1);
                    objects[0] = node;

                    try nodes.append(&objects[0]);
                }
            },

            else => {
                unreachable;
            },
        }
    } else {
        unreachable;
        //compile error
    }

    return nodes.toOwnedSlice();
}

fn expectFnDef(self: *Self) !Node {
    const sign = try self.expectFnSignature();
    self.forwardToken();

    if (self.curToken().ty != .lcbrace) {
        // compile error
        unreachable;
    }

    const block = try self.expectBlock();

    if (self.curToken().ty != .rcbrace) {
        // compile error
        unreachable;
    }

    return Ast.Node{ .data = .{
        .fn_def = .{
            .signature = sign.data.@"fn_sign",
            .block = block,
        },
    }, .loc = self.curToken().loc };
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

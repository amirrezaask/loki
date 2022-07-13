const std = @import("std");
const print = std.debug.print;
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Decl = Ast.Decl;
const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;
const Self = @This();
const Stack = @import("stack.zig").Stack;
const Error = @import("errors.zig").Error;
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

fn strEql(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}
alloc: std.mem.Allocator,
src: []const u8,
cur: u64,
state: State = .start,
tokens: std.ArrayList(Token),

pub fn init(alloc: std.mem.Allocator, src: []const u8) Error!Self {
    var tokenizer = Tokenizer.init(src);
    var tokens = std.ArrayList(Token).init(alloc);
    while (true) {
        const token = tokenizer.next() catch return Error.AllocationFailed;
        if (token.ty == .EOF) break;
        tokens.append(token) catch return Error.AllocationFailed;
    }
    return Self{
        .cur = 0,
        .src = src,
        .tokens = tokens,
        .alloc = alloc,
    };
}

pub fn deinit(self: *Self) void {
    self.tokens.deinit();
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

fn expectDecl(self: *Self) Error!Decl {
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
    const expr = try Node.initAlloc(try self.expectExpr(), self.alloc);

    self.forwardToken(); // ;
    self.forwardToken(); // next start
    return Ast.Decl{
        .name = ident_token.val.identifier,
        .ty = decl_ty,
        .val = expr,
    };
}

fn expectExpr(self: *Self) Error!Node {
    var node: Node = undefined;
    switch (self.curToken().ty) {
        .unsigned_int => {
            node = .{
                .data = .{ .uint = self.curToken().val.unsigned_int },
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
        .keyword_void => {
            node = .{
                .data = .void_ty,
                .loc = self.curToken().loc,
            };
        },
        else => {
            print("expectExpr ty: {}", .{self.curToken().ty});
            unreachable;
        },
    }
    return node;
}

fn expectFnSignature(self: *Self) Error!Node {
    self.forwardToken(); // this will move us to open paren

    if (self.curToken().ty != .open_paren) return Error.ExpectsOpenParen;

    self.forwardToken();

    var args = std.ArrayList([2]*Node).init(self.alloc);

    while (true) {
        if (self.curToken().ty == .close_paren) break;
        if (self.curToken().ty == .identifier) {
            const ident = try Node.initAlloc(.{
                .data = .{
                    .identifier = self.curToken().val.identifier,
                },
                .loc = self.curToken().loc,
            }, self.alloc);

            self.forwardToken(); //:
            self.forwardToken();
            const ty = try Node.initAlloc(try self.expectExpr(), self.alloc);

            args.append([2]*Node{ ident, ty }) catch return Error.AllocationFailed;
            self.forwardToken(); // next type or end
        } else {
            unreachable;
        }
    }
    self.forwardToken();

    const ret_ty = try Node.initAlloc(try self.expectExpr(), self.alloc);

    return Node{
        .data = .{ .@"fn_sign" = .{
            .args = args.toOwnedSlice(),
            .ret_ty = ret_ty,
        } },
        .loc = self.curToken().loc,
    };
}

fn expectFnCall(self: *Self) Error!Ast.FnCall {
    const name = try Node.initAlloc(.{ .data = .{ .identifier = self.curToken().val.identifier }, .loc = self.curToken().loc }, self.alloc);
    self.forwardToken();
    if (self.curToken().ty != .open_paren) return Error.ExpectsOpenParen;
    self.forwardToken();
    var args = std.ArrayList(*Node).init(self.alloc);
    while (true) {
        if (self.curToken().ty == .close_paren) {
            break;
        }
        const expr = try Node.initAlloc(try self.expectExpr(), self.alloc);

        args.append(expr) catch return Error.AllocationFailed;
        self.forwardToken();
    }

    return Ast.FnCall{
        .name = name,
        .args = args.toOwnedSlice(),
    };
}

fn expectBlock(self: *Self) Error![]*Node {
    if (self.curToken().ty != .lcbrace) {
        return Error.ExpectsOpenCurlyBrace;
    }

    var nodes = std.ArrayList(*Node).init(self.alloc);
    self.forwardToken();
    while (true) {
        if (self.curToken().ty == .rcbrace) break;
        switch (self.curToken().ty) {
            .identifier => {
                if (self.peekToken().ty == .open_paren) {
                    // function call
                    const node = try Node.initAlloc(.{
                        .data = .{ .fn_call = try self.expectFnCall() },
                        .loc = self.curToken().loc,
                    }, self.alloc);
                    nodes.append(node) catch return Error.AllocationFailed;
                    self.forwardToken();
                    self.expectSemiColon();
                    self.forwardToken();
                } else if (self.peekToken().ty == .double_colon or self.peekToken().ty == .equal) {
                    const decl = try self.expectDecl();
                    const node = try Node.initAlloc(.{
                        .data = .{ .decl = decl },
                        .loc = decl.val.loc,
                    }, self.alloc);
                    nodes.append(node) catch return Error.AllocationFailed;
                }
            },

            else => {
                unreachable;
            },
        }
    }
    return nodes.toOwnedSlice();
}

fn expectFnDef(self: *Self) Error!Node {
    const sign = try self.expectFnSignature();
    self.forwardToken();
    if (self.curToken().ty != .lcbrace) {
        return Error.ExpectsCloseCurlyBrace;
    }

    // this function call causes a segfault
    const block = try self.expectBlock();

    if (self.curToken().ty != .rcbrace) {
        return Error.ExpectsCloseCurlyBrace;
    }

    return Ast.Node{ .data = .{
        .fn_def = .{
            .signature = sign.data.@"fn_sign",
            .block = block,
        },
    }, .loc = self.curToken().loc };
}

fn expectSemiColon(self: *Self) void {
    if (self.curToken().ty == .semi_colon) {
        return;
    } else {
        unreachable;
    }
}

fn expectImport(self: *Self) []const u8 {
    self.forwardToken();

    switch (self.curToken().ty) {
        .string_literal => {
            const import_string = self.curToken().val.string_literal;
            self.forwardToken();
            self.expectSemiColon();
            self.forwardToken();
            return import_string;
        },

        else => {
            unreachable;
        },
    }
}

pub fn getAst(self: *Self, alloc: std.mem.Allocator) Error!Ast {
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
                    ast.addTopLevelNode(.{
                        .data = .{ .decl = decl },
                        .loc = decl.val.loc,
                    }) catch return Error.AllocationFailed;
                },

                .keyword_import => {
                    const import_str = self.expectImport();
                    ast.addTopLevelNode(.{
                        .data = .{ .import = import_str },
                        .loc = self.curToken().loc,
                    }) catch return Error.AllocationFailed;
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

test "all expressions" {
    var parser = try Self.init(std.testing.allocator,
        \\import "std.loki";
        \\a :: 2;
        \\b :: "salam";
        \\c :: 'c';
        \\d :: true;
        \\e :: false;
        \\f :: a;
        \\t1 :: float;
        \\f1 :: fn(a: int) void {
        \\ printf("Hello World");
        \\};
    );

    defer parser.deinit();

    var ast = try parser.getAst(std.testing.allocator);
    defer ast.deinit(std.testing.allocator);

    try std.testing.expectEqualStrings("std.loki", ast.top_level.items[0].data.@"import");

    try std.testing.expectEqualStrings("a", ast.top_level.items[1].data.@"decl".name);
    try std.testing.expectEqual(@as(u64, 2), ast.top_level.items[1].data.@"decl".val.data.@"uint");

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

    try std.testing.expectEqualStrings("t1", ast.top_level.items[7].data.@"decl".name);
    try std.testing.expectEqual(Ast.Node.Data.float_ty, ast.top_level.items[7].data.@"decl".val.data);

    try std.testing.expectEqualStrings("f1", ast.top_level.items[8].data.@"decl".name);
    try std.testing.expectEqual(Ast.Node.Data.void_ty, ast.top_level.items[8].data.@"decl".val.data.fn_def.signature.ret_ty.data);
    try std.testing.expectEqualStrings("a", ast.top_level.items[8].data.@"decl".val.data.fn_def.signature.args[0][0].data.identifier);
    try std.testing.expectEqual(Ast.Node.Data.int_ty, ast.top_level.items[8].data.@"decl".val.data.fn_def.signature.args[0][1].data);
    try std.testing.expectEqualStrings("printf", ast.top_level.items[8].data.@"decl".val.data.fn_def.block[0].data.fn_call.name.data.identifier);
    try std.testing.expectEqualStrings("Hello World", ast.top_level.items[8].data.@"decl".val.data.fn_def.block[0].data.fn_call.args[0].data.string_literal);
}

test "hello world program" {
    var parser = try Self.init(std.testing.allocator,
        \\import "std.loki";
        \\main :: fn() void {
        \\     printf("Hello World from loki");
        \\};
    );

    defer parser.deinit();

    var ast = try parser.getAst(std.testing.allocator);
    defer ast.deinit(std.testing.allocator);

    try std.testing.expectEqualStrings("main", ast.top_level.items[1].data.@"decl".name);
    try std.testing.expectEqual(Ast.Node.Data.void_ty, ast.top_level.items[1].data.@"decl".val.data.fn_def.signature.ret_ty.data);
    try std.testing.expectEqualStrings("printf", ast.top_level.items[1].data.@"decl".val.data.fn_def.block[0].data.fn_call.name.data.identifier);
    try std.testing.expectEqualStrings("Hello World from loki", ast.top_level.items[1].data.@"decl".val.data.fn_def.block[0].data.fn_call.args[0].data.string_literal);
}

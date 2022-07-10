const std = @import("std");
const print = std.debug.print;
const Ast = @import("Ast.zig");
const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;
const Self = @This();
const Stack = @import("stack.zig").Stack;
const State = enum {
    start,
    import,
    waiting_for_string,
    waiting_for_expr,
    saw_identifier,
    var_decl,
    const_decl,
};

src: []const u8,
cur: u64,

pub fn init(src: []const u8) Self {
    return .{
        .cur = 0,
        .src = src,
    };
}

pub fn getAst(self: *Self, alloc: std.mem.Allocator) !Ast {
    var tokenizer = Tokenizer.init(self.src);
    var tokens_list = std.ArrayList(Token).init(alloc);
    while (true) {
        const token = try tokenizer.next();
        if (token.ty == .EOF) break;
        try tokens_list.append(token);
    }

    const tokens = tokens_list.toOwnedSlice();
    print("\n{any}\n", .{tokens});
    var states = Stack(State).init(alloc);
    var data = Stack(Token).init(alloc);
    var ast = Ast.init(alloc);
    states.push(.start);
    while (true) {
        const cur_token = tokens[self.cur];
        print("state is {}\n", .{states.top().?});
        switch (states.top().?) {
            .start => {
                switch (cur_token.ty) {
                    .identifier => {
                        try states.push(.saw_identifier);
                        try data.push(cur_token);
                        continue;
                    },
                    .keyword_import => {
                        switch (cur_token.val) {
                            .keyword => {
                                switch (cur_token.val.keyword) {
                                    .@"import" => {
                                        try states.push(.import);
                                        try states.push(.waiting_for_string);
                                        continue;
                                    },
                                    else => {
                                        unreachable;
                                    },
                                }
                            },
                            else => {
                                unreachable;
                            },
                        }
                    },
                    .equal => {},
                    else => {},
                }
            },
            .waiting_for_string => {
                switch (cur_token.ty) {
                    .string_literal => {
                        _ = states.pop();
                        switch (states.top().?) {
                            .import => {
                                try ast.top_level.append(.{
                                    .ty = .@"import",
                                    .val = .{ .import = .{
                                        .path = cur_token.val.string_literal,
                                    } },
                                });
                                _ = states.pop();
                            },
                            else => {
                                unreachable;
                                // is there any state that we are waiting_for just a string ?
                            },
                        }
                    },
                    else => {
                        unreachable;
                        //compile error.
                    },
                }
            },
            .saw_identifier => {
                switch (cur_token.ty) {
                    .equal => {
                        try states.push(.var_decl);
                    },
                    .double_colon => {
                        try states.push(.const_decl);
                    },
                    else => {
                        unreachable;
                        // compile error
                    },
                }
                continue;
            },
            .var_decl, .const_decl => {
                try states.push(.waiting_for_expr);
                continue;
            },
            .waiting_for_expr => {
                _ = states.pop();
                print("waiting_for_expr state is {}\n", .{states.top().?});
                print("token is {}", .{cur_token.ty});
                var node: Ast.Node = .{
                    .ty = .@"undefined",
                    .val = Ast.Node.Val.@"undefined",
                };
                switch (cur_token.ty) {
                    .keyword => {
                        // fn function def
                    },
                    .lcbrace => {
                        // code block
                    },
                    .identifier => {
                        switch (states.top().?) {
                            .var_decl => {
                                node.ty = .@"decl";
                                node.val = .{ .decl = .{ .ty = .@"var", .name = data.pop().?.val.identifier, .val = .{ .identifier = cur_token.val.identifier } } };
                            },
                            .const_decl => {
                                node.ty = .@"decl";
                                node.val = .{ .decl = .{ .ty = .@"const", .name = data.pop().?.val.identifier, .val = .{ .identifier = cur_token.val.identifier } } };
                            },
                            else => {
                                unreachable;
                                // compile error
                            },
                        }
                    },
                    .float => {
                        switch (states.top().?) {
                            .var_decl => {
                                node.ty = .@"decl";
                                node.val = .{ .decl = .{ .ty = .@"var", .name = data.pop().?.val.identifier, .val = .{ .float = cur_token.val.float } } };
                            },
                            .const_decl => {
                                node.ty = .@"decl";
                                node.val = .{ .decl = .{ .ty = .@"const", .name = data.pop().?.val.identifier, .val = .{ .float = cur_token.val.float } } };
                            },
                            else => {
                                unreachable;
                                // compile error
                            },
                        }
                    },
                    .char => {
                        switch (states.top().?) {
                            .var_decl => {
                                node.ty = .@"decl";
                                node.val = .{ .decl = .{ .ty = .@"var", .name = data.pop().?.val.identifier, .val = .{ .char = cur_token.val.char } } };
                            },
                            .const_decl => {
                                node.ty = .@"decl";
                                node.val = .{ .decl = .{ .ty = .@"const", .name = data.pop().?.val.identifier, .val = .{ .char = cur_token.val.char } } };
                            },
                            else => {
                                unreachable;
                                // compile error
                            },
                        }
                    },
                    .unsigned_int => {
                        switch (states.top().?) {
                            .var_decl => {
                                node.ty = .@"decl";
                                node.val = .{ .decl = .{ .ty = .@"var", .name = data.pop().?.val.identifier, .val = .{ .unsigned_int = cur_token.val.unsigned_int } } };
                            },
                            .const_decl => {
                                node.ty = .@"decl";
                                node.val = .{ .decl = .{ .ty = .@"const", .name = data.pop().?.val.identifier, .val = .{ .unsigned_int = cur_token.val.unsigned_int } } };
                            },
                            else => {
                                unreachable;
                                // compile error
                            },
                        }
                    },
                    .string_literal => {
                        switch (states.top().?) {
                            .var_decl => {
                                node.ty = .@"decl";
                                node.val = .{ .decl = .{ .ty = .@"var", .name = data.pop().?.val.identifier, .val = .{ .string_literal = cur_token.val.string_literal } } };
                            },
                            .const_decl => {
                                node.ty = .@"decl";
                                node.val = .{ .decl = .{ .ty = .@"const", .name = data.pop().?.val.identifier, .val = .{ .string_literal = cur_token.val.string_literal } } };
                            },
                            else => {
                                unreachable;
                                // compile error
                            },
                        }
                    },
                    else => {
                        unreachable;
                        //compile error ,expected an expression
                    },
                }

                try ast.top_level.append(node);
                _ = states.pop();
            },
            else => {
                unreachable;
            },
        }
        self.cur += 1;
    }
}

test "just a const" {
    var parser = Self.init(
        \\import "std.loki";
        \\a :: 2;
    );

    const ast = try parser.getAst(std.testing.allocator);
    _ = ast;
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

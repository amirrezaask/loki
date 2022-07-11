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
    var tokens = std.ArrayList(Token).init(alloc);
    defer tokens.deinit();
    while (true) {
        const token = try tokenizer.next();
        if (token.ty == .EOF) break;
        try tokens.append(token);
    }

    print("\n{any}\n", .{tokens});
    var states = Stack(State).init(alloc);
    defer states.deinit();
    var data = Stack(Token).init(alloc);
    defer data.deinit();
    var ast = Ast.init(alloc);
    try states.push(.start);
    while (true) {
        if (self.cur >= tokens.items.len) break;
        const cur_token = tokens.items[self.cur];
        print("state: {} - token: {}\n", .{ states.top().?, cur_token.ty });
        switch (states.top().?) {
            .start => {
                switch (cur_token.ty) {
                    .identifier => {
                        try states.push(.saw_identifier);
                        try data.push(cur_token);
                        self.cur += 1;
                        continue;
                    },
                    .keyword_import => {
                        try states.push(.import);
                        try states.push(.waiting_for_string);
                        self.cur += 1;
                        continue;
                    },
                    .semi_colon => {
                        self.cur += 1;
                        continue;
                    },
                    else => {
                        unreachable;
                    },
                }
            },
            .waiting_for_string => {
                _ = states.pop();
                if (cur_token.ty == .string_literal and states.top().? == .import) {
                    try ast.top_level.append(.{
                        .loc = cur_token.loc,
                        .data = .{
                            .import = cur_token.val.string_literal,
                        },
                    });
                    _ = states.pop();
                    self.cur += 1;
                    continue;
                } else {
                    unreachable;
                    // compile error
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
                try states.push(.waiting_for_expr);
                self.cur += 1;
                continue;
            },
            .waiting_for_expr => {
                _ = states.pop();
                var node: Ast.Node = undefined;
                switch (cur_token.ty) {
                    .keyword_fn => {}, // fn def TODO
                    .keyword_if => {}, // if expr TODO
                    .keyword_true => {
                        if (states.top().? == .const_decl and states.top().? == .var_decl) {
                            const decl = &Decl{
                                .name = data.pop().?.val.identifier,
                                .val = .{ .loc = cur_token.loc, .data = .{
                                    .@"bool" = true,
                                } },
                            };
                            switch (states.top().?) {
                                .const_decl => {
                                    node = .{
                                        .data = .{ .@"const_decl" = decl },
                                        .loc = cur_token.loc,
                                    };
                                },
                                .var_decl => {
                                    node = .{
                                        .data = .{ .@"var_decl" = decl },
                                        .loc = cur_token.loc,
                                    };
                                },
                                else => {
                                    unreachable;
                                },
                            }
                        }
                    }, // bool true
                    .keyword_false => {
                        if (states.top().? == .const_decl and states.top().? == .var_decl) {
                            const decl = &Decl{
                                .name = data.pop().?.val.identifier,
                                .val = Node{
                                    .loc = cur_token.loc,
                                    .data = .{
                                        .@"bool" = false,
                                    },
                                },
                            };
                            switch (states.top().?) {
                                .const_decl => {
                                    node = .{
                                        .data = .{ .@"const_decl" = decl },
                                        .loc = cur_token.loc,
                                    };
                                },
                                .var_decl => {
                                    node = .{
                                        .data = .{ .@"var_decl" = decl },
                                        .loc = cur_token.loc,
                                    };
                                },
                                else => {
                                    unreachable;
                                },
                            }
                        }
                    }, // bool false

                    .keyword_struct => {}, //TODO
                    .keyword_union => {}, // TODO
                    .keyword_enum => {}, //TODO

                    .lcbrace => {}, // code block //TODO
                    .identifier => {
                        if (states.top().? == .const_decl and states.top().? == .var_decl) {
                            const decl = &Decl{
                                .name = data.pop().?.val.identifier,
                                .val = Node{ .loc = cur_token.loc, .data = .{
                                    .@"identifier" = cur_token.val.identifier,
                                } },
                            };

                            switch (states.top().?) {
                                .const_decl => {
                                    node = .{
                                        .data = .{ .@"const_decl" = decl },
                                        .loc = cur_token.loc,
                                    };
                                },
                                .var_decl => {
                                    node = .{
                                        .data = .{ .@"var_decl" = decl },
                                        .loc = cur_token.loc,
                                    };
                                },
                                else => {
                                    unreachable;
                                },
                            }
                        }
                    }, // another ident
                    .string_literal => {
                        if (states.top().? == .const_decl and states.top().? == .var_decl) {
                            const decl = &Decl{
                                .name = data.pop().?.val.identifier,
                                .val = Node{
                                    .loc = cur_token.loc,
                                    .data = .{
                                        .@"string_literal" = cur_token.val.string_literal,
                                    },
                                },
                            };
                            switch (states.top().?) {
                                .const_decl => {
                                    node = .{
                                        .data = .{ .@"const_decl" = decl },
                                        .loc = cur_token.loc,
                                    };
                                },
                                .var_decl => {
                                    node = .{
                                        .data = .{ .@"var_decl" = decl },
                                        .loc = cur_token.loc,
                                    };
                                },
                                else => {
                                    unreachable;
                                },
                            }
                        }
                    },
                    .unsigned_int => {
                        if (states.top().? == .const_decl and states.top().? == .var_decl) {
                            const decl = &Decl{
                                .name = data.pop().?.val.identifier,
                                .val = Node{
                                    .loc = cur_token.loc,
                                    .data = .{
                                        .@"unsigned_int" = cur_token.val.unsigned_int,
                                    },
                                },
                            };
                            switch (states.top().?) {
                                .const_decl => {
                                    node = .{
                                        .data = .{ .@"const_decl" = decl },
                                        .loc = cur_token.loc,
                                    };
                                },
                                .var_decl => {
                                    node = .{
                                        .data = .{ .@"var_decl" = decl },
                                        .loc = cur_token.loc,
                                    };
                                },
                                else => {
                                    unreachable;
                                },
                            }
                        }
                    },
                    .float => {
                        if (states.top().? == .const_decl and states.top().? == .var_decl) {
                            const decl = &Decl{
                                .name = data.pop().?.val.identifier,
                                .val = Node{ .loc = cur_token.loc, .data = .{
                                    .@"float" = cur_token.val.float,
                                } },
                            };
                            switch (states.top().?) {
                                .const_decl => {
                                    node = .{
                                        .data = .{ .@"const_decl" = decl },
                                        .loc = cur_token.loc,
                                    };
                                },
                                .var_decl => {
                                    node = .{
                                        .data = .{ .@"var_decl" = decl },
                                        .loc = cur_token.loc,
                                    };
                                },
                                else => {
                                    unreachable;
                                },
                            }
                        }
                    },
                    .char => {
                        if (states.top().? == .const_decl and states.top().? == .var_decl) {
                            const decl = &Decl{
                                .name = data.pop().?.val.identifier,
                                .val = Node{ .loc = cur_token.loc, .data = .{
                                    .@"char" = cur_token.val.char,
                                } },
                            };
                            switch (states.top().?) {
                                .const_decl => {
                                    node = .{
                                        .data = .{ .@"const_decl" = decl },
                                        .loc = cur_token.loc,
                                    };
                                },
                                .var_decl => {
                                    node = .{
                                        .data = .{ .@"var_decl" = decl },
                                        .loc = cur_token.loc,
                                    };
                                },
                                else => {
                                    unreachable;
                                },
                            }
                        }
                    },
                    else => { // compile error
                    },
                }

                node.loc = cur_token.loc;
                try ast.top_level.append(node);
                self.cur += 1;
                _ = states.pop();
                _ = states.pop();
                continue;
            },
            else => {
                unreachable;
            },
        }
    }

    return ast;
}

test "just a const" {
    var parser = Self.init(
        \\import "std.loki";
        \\a :: 2;
    );

    var ast = try parser.getAst(std.testing.allocator);
    print("\n{r}\n", .{ast.top_level.items});
    defer ast.deinit();
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

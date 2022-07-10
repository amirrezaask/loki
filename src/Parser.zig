const std = @import("std");
const Ast = @import("Ast.zig");
const Tokenizer = @import("Tokenizer.zig");
const Self = @This();
const State = enum {
    start,
    import,
    waiting_for_string,
    waiting_for_expr,
    saw_identifier,
    var_decl,
    const_decl,
};

fn Stack(comptime T: type) type {
    return struct {
        allocator: std.mem.Allocator,
        array_list: std.ArrayList(T),
        pub fn init(alloc: std.mem.Allocator) Stack {
            return .{ .allocator = alloc, .array_list = std.ArrayList(State).init(alloc) };
        }

        pub fn pop(self: *Stack) State {
            const last = self.array_list[self.array_list.len - 1];
            self.array_list = self.array_list[0 .. self.array_list.len - 1];
            return last;
        }

        pub fn push(self: *Stack, s: State) !void {
            try self.array_list.append(s);
        }

        pub fn top(self: *Stack) State {
            return self.array_list[self.array_list.len - 1];
        }
    };
}

src: []const u8,
cur: u64,

pub fn init(src: []const u8) Self {
    return .{
        .src = src,
    };
}

pub fn getAst(self: *Self, alloc: std.mem.Allocator) !Ast {
    const tokenizer = Tokenizer.init(self.src);
    var tokens_list = std.ArrayList(Tokenizer.Token).init(alloc);
    while (true) {
        const token = try tokenizer.next();
        if (token.ty == .EOF) break;
        tokens_list.append(token);
    }

    const tokens = tokens_list.toOwnedSlice();
    std.debug.print("tokens aquired...", .{});
    var states = Stack(State).init(alloc);
    var data = Stack(Tokenizer.Token).init(alloc);
    var ast = Ast.init(alloc);
    while (true) : (self.cur += 1) {
        const cur_token = tokens[self.cur];
        switch (states.top()) {
            .start => {
                switch (cur_token.ty) {
                    .identifier => {
                        states.push(.saw_identifier);
                        data.push(cur_token);
                        continue;
                    },
                    .keyword => {
                        switch (cur_token.val) {
                            .keyword => {
                                switch (cur_token.val.keyword) {
                                    .@"import" => {
                                        states.push(.import);
                                        states.push(.waiting_for_string);
                                        continue;
                                    },
                                }
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
                        switch (states.top()) {
                            .import => {
                                try ast.top_level.append(.{
                                    .ty = .@"import",
                                    .val = .{ .import = .{
                                        .path = cur_token.val.string_literal,
                                    } },
                                });
                            },
                            else => {
                                // is there any state that we are waiting_for just a string ?
                            },
                        }
                    },
                    else => {
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
                        // compile error
                    },
                }
                continue;
            },
            .var_decl, .const_decl => {
                states.push(.waiting_for_expr);
                continue;
            },
            .waiting_for_expr => {
                var node: Ast.Node = .{
                    .ty = .@"undefined",
                    .val = Ast.Node.Val.@"undefined",
                };
                switch (cur_token.ty) {
                    .keyword => {},
                    .unsigned_int => {
                        switch (states.top()) {
                            .var_decl => {
                                node.ty = .@"decl";
                                node.val = .{ .decl = .{ .ty = .@"var", .name = data.pop().val.identifier, .val = cur_token.val.unsigned_int } };
                            },
                            .const_decl => {
                                node.ty = .@"decl";
                                node.val = .{ .decl = .{ .ty = .@"const", .name = data.pop().val.identifier, .val = cur_token.val.unsigned_int } };
                            },
                            else => {
                                // compile error
                            },
                        }
                    },
                    .string_literal => {
                        switch (states.top()) {
                            .var_decl => {
                                node.ty = .@"decl";
                                node.val = .{ .decl = .{ .ty = .@"var", .name = data.pop().val.identifier, .val = cur_token.val.string_literal } };
                            },
                            .const_decl => {
                                node.ty = .@"decl";
                                node.val = .{ .decl = .{ .ty = .@"const", .name = data.pop().val.identifier, .val = cur_token.val.string_literal } };
                            },
                            else => {
                                // compile error
                            },
                        }
                    },
                }

                ast.top_level.append();
            },
        }
    }
}

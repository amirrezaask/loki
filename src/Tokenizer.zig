const std = @import("std");
const testing = std.testing;

pub const Self = @This();
pub const Keyword = enum { @"if", @"for", @"while", @"enum", @"struct", @"union", @"fn", @"true", @"false" };
fn strEql(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

pub const Token = struct {
    pub const Type = enum {
        EOF,
        lcbrace,
        rcbrace,
        lbrace,
        rbrace,
        open_paren,
        close_paren,
        comma,
        bang,
        sharp,
        dollor,

        minus,
        plus,
        asterix,
        forward_slash,
        percent,

        plus_equal,
        minus_equal,
        div_equal,
        mod_equal,
        mul_equal,

        equal,
        colon,
        double_colon,
        double_equal,
        semi_colon,
        ampersand,
        hat,
        atsign,
        dot,
        single_quote,
        double_quote,
        back_slash,
        pipe,
        identifier,
        keyword,
        char,
        int,
        float,
        string_literal,
    };
    pub const Val = union(enum) {
        nothing: void,
        int: i64,
        keyword: Keyword,
        identifier: []const u8,
    };
    pub const Loc = struct {
        start: u64,
        end: u64,
    };
    ty: Type,
    val: Val,
    loc: Loc,
};

pub const State = enum {
    start,
    string,
    in_char_literal,
    int_decimal,
    saw_equal,
    saw_bang,
    saw_plus,
    saw_minus,
    saw_slash,
    saw_asterix,
    saw_percent,
    saw_colon,
    saw_hat,
    saw_left_angle_bracket,
    saw_right_angle_bracket,
};

src: []const u8,
cur: u64,

pub fn init(input: []const u8) Self {
    return .{ .cur = 0, .src = input };
}

pub fn next(self: *Self) !Token {
    var start_of_token = self.cur;
    var state: State = .start;
    var result: Token = .{
        .val = Token.Val.nothing,
        .loc = .{
            .start = start_of_token,
            .end = start_of_token,
        },
        .ty = .EOF,
    };
    while (true) {
        if (self.src.len <= self.cur) {
            switch (state) {
                .int_decimal => {
                    const parsed_int = try std.fmt.parseInt(i64, self.src[start_of_token..self.cur], 0);
                    result.ty = .int;
                    result.loc = .{
                        .start = start_of_token,
                        .end = self.cur,
                    };
                    result.val = .{ .int = parsed_int };
                    return result;
                },
                else => {
                    @panic("not implemented");
                },
            }
        }

        const c = self.src[self.cur];
        switch (state) {
            .start => switch (c) {
                '1'...'9' => {
                    self.cur += 1;
                    state = .int_decimal;
                },
                '<' => {
                    state = .saw_left_angle_bracket;
                },
                '>' => {
                    state = .saw_right_angle_bracket;
                },
                '{' => {
                    result.ty = .lcbrace;

                    self.cur += 1;
                    return result;
                },
                '}' => {
                    result.ty = .rcbrace;

                    self.cur += 1;
                    return result;
                },
                '[' => {
                    result.ty = .lbrace;

                    self.cur += 1;
                    return result;
                },
                ']' => {
                    result.ty = .rbrace;
                    self.cur += 1;
                    return result;
                },
                '(' => {
                    result.ty = .open_paren;
                    self.cur += 1;
                    return result;
                },
                ')' => {
                    result.ty = .close_paren;

                    self.cur += 1;
                    return result;
                },
                ',' => {
                    result.ty = .comma;

                    self.cur += 1;
                    return result;
                },
                '!' => {
                    state = .saw_bang;
                },
                '#' => {
                    result.ty = .sharp;

                    self.cur += 1;
                    return result;
                },
                '*' => {
                    state = .saw_asterix;
                },
                '%' => {
                    state = .saw_percent;
                },

                '-' => {
                    state = .saw_minus;
                },
                '+' => {
                    state = .saw_plus;
                },
                '=' => {
                    state = .saw_equal;
                },
                ':' => {
                    state = .saw_colon;
                },
                ';' => {
                    result.ty = .semi_colon;

                    self.cur += 1;
                    return result;
                },
                '$' => {
                    result.ty = .dollor;

                    self.cur += 1;
                    return result;
                },
                '&' => {
                    result.ty = .ampersand;
                    return result;
                },
                '^' => {
                    state = .saw_hat;
                },
                '@' => {
                    result.ty = .atsign;

                    self.cur += 1;
                    return result;
                },
                '.' => {
                    result.ty = .dot;

                    self.cur += 1;
                    return result;
                },
                '\'' => {
                    state = .in_char_literal;
                },
                '"' => {
                    state = .string;
                },
                '/' => {
                    state = .saw_slash;
                },
                '\\' => {
                    result.ty = .back_slash;

                    self.cur += 1;
                    return result;
                },
                '|' => {
                    result.ty = .pipe;

                    self.cur += 1;
                    return result;
                },
                ' ' => {
                    const thing = self.src[start_of_token..self.cur];
                    if (strEql("if", thing)) {
                        result.ty = .keyword;
                        result.val = .{ .keyword = Keyword.@"if" };
                        result.loc.end = self.cur - 1;
                        self.cur += 1;
                        return result;
                    } else if (strEql("for", thing)) {
                        result.ty = .keyword;
                        result.val = .{ .keyword = Keyword.@"for" };
                        result.loc.end = self.cur - 1;
                        self.cur += 1;
                        return result;
                    } else if (std.mem.trim(u8, thing, &[_]u8{ ' ', '\n', '\r' }).len == 0) {
                        self.cur += 1;
                        continue;
                    } else {
                        // probably an identifier
                        self.cur += 1;
                        result.ty = .identifier;
                        result.val = .{ .identifier = self.src[start_of_token .. self.cur - thing.len] };
                        result.loc.end = self.cur - thing.len - 1;
                        return result;
                    }
                },
                else => {},
            },
            .saw_colon => {
                switch (c) {
                    ':' => {
                        self.cur += 1;
                        result.ty = .double_colon;
                        return result;
                    },
                    else => {
                        return result;
                    },
                }
            },
            .saw_plus => {
                switch (c) {
                    '=' => {
                        self.cur += 1;
                        return result;
                    },
                    else => {
                        return result;
                    },
                }
            },
            .saw_minus => {
                switch (c) {
                    '=' => {
                        self.cur += 1;
                        return result;
                    },
                    else => {
                        return result;
                    },
                }
            },
            .saw_slash => {
                switch (c) {
                    '=' => {
                        self.cur += 1;
                        return result;
                    },
                    else => {
                        return result;
                    },
                }
            },
            .saw_asterix => {
                switch (c) {
                    '=' => {
                        self.cur += 1;
                        return result;
                    },
                    else => {
                        return result;
                    },
                }
            },
            .saw_percent => {
                switch (c) {
                    '=' => {
                        self.cur += 1;
                        return result;
                    },
                    else => {
                        return result;
                    },
                }
            },
            .string => {
                switch (c) {
                    '"' => {
                        // ending string
                        result.ty = .string_literal;
                        result.loc = .{ .start = start_of_token + 1, .end = self.cur - 1 };
                        return result;
                    },
                    else => {},
                }
            },
            .int_decimal => {
                switch (c) {
                    '1'...'9' => {},
                    '0' => {},
                    else => {
                        result.ty = .int;
                        result.loc = .{ .start = start_of_token, .end = self.cur + 1 };
                        return result;
                    },
                }
            },
            .saw_equal => {
                switch (c) {
                    '=' => {
                        self.cur += 1;
                        result.ty = .double_equal;
                        result.loc = .{
                            .start = start_of_token,
                            .end = self.cur,
                        };
                        return result;
                    },
                    else => {
                        return result;
                    },
                }
            },
            .in_char_literal => {
                switch (c) {
                    else => {
                        self.cur += 2;
                        result.ty = .char;
                        result.loc = .{
                            .start = start_of_token,
                            .end = self.cur,
                        };
                        return result;
                    },
                }
            },
            .saw_bang => {},
            .saw_hat => {},
            .saw_left_angle_bracket => {},
            .saw_right_angle_bracket => {},
        }
        self.cur += 1;
    }
}

test "int" {
    var t = Self.init("123");
    var tok = try t.next();

    try testing.expectEqual(Self.Token{ .ty = .int, .val = .{ .int = 123 }, .loc = .{ .start = 0, .end = 3 } }, tok);
}

test "string" {
    var t = Self.init("\"123\"");
    var tok = try t.next();

    try testing.expectEqual(Token.Type.string_literal, tok.ty);
    try testing.expectEqual(Token.Loc{
        .start = 1,
        .end = 3,
    }, tok.loc);
}
test "keywords" {
    var t = Self.init("if ");
    var tok = try t.next();

    try testing.expectEqual(Token.Type.keyword, tok.ty);
    try testing.expectEqual(Token.Val{ .keyword = .@"if" }, tok.val);
    try testing.expectEqual(Token.Loc{
        .start = 0,
        .end = 1,
    }, tok.loc);

    t = Self.init("for ");
    tok = try t.next();

    try testing.expectEqual(Token.Type.keyword, tok.ty);
    try testing.expectEqual(Token.Val{ .keyword = .@"for" }, tok.val);
    try testing.expectEqual(Token.Loc{
        .start = 0,
        .end = 2,
    }, tok.loc);
}

test "complex" {
    var t = Self.init("if (x < 2 ) {} ");
    var tok = try t.next();

    try testing.expectEqual(Token.Type.keyword, tok.ty);
    try testing.expectEqual(Token.Val{ .keyword = .@"if" }, tok.val);
    try testing.expectEqual(Token.Loc{
        .start = 0,
        .end = 1,
    }, tok.loc);

    tok = try t.next();

    try testing.expectEqual(Token.Type.open_paren, tok.ty);
    try testing.expectEqual(Token.Loc{
        .start = 3,
        .end = 3,
    }, tok.loc);

    tok = try t.next();

    try testing.expectEqual(Token.Type.identifier, tok.ty);
    try testing.expectEqualStrings("x", tok.val.identifier);
    try testing.expectEqual(Token.Loc{
        .start = 4,
        .end = 4,
    }, tok.loc);
}

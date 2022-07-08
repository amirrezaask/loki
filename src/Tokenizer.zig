const std = @import("std");
const testing = std.testing;

pub const Self = @This();

pub const Keyword = enum { @"if", @"for", @"while", @"enum", @"struct", @"union", @"fn", @"true", @"false", @"return", @"void", @"int", @"unsigned_int", @"string", @"byte" };

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
        left_angle,
        right_angle,

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
        less_equal,
        greater_equal,

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
        unsigned_int,
        float,
        string_literal,
    };
    pub const Val = union(enum) {
        nothing: void,
        unsigned_int: u64,
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
    unsigned_int,
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

// fn handle_ws(self: *Self) !Token {

// }

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
                .unsigned_int => {
                    const parsed_uint = try std.fmt.parseUnsigned(u64, self.src[start_of_token..self.cur], 0);
                    result.ty = .unsigned_int;
                    result.loc = .{
                        .start = start_of_token,
                        .end = self.cur,
                    };
                    result.val = .{ .unsigned_int = parsed_uint };
                    return result;
                },

                else => {
                    // identifier probably
                    const thing = self.src[start_of_token..self.cur];
                    result.ty = .identifier;
                    result.val = .{ .identifier = thing };
                    result.loc.end = self.cur - 1;
                    return result;
                },
            }
        }

        const c = self.src[self.cur];
        switch (state) {
            .start => switch (c) {
                '1'...'9' => {
                    self.cur += 1;
                    result.ty = .unsigned_int;
                    state = .unsigned_int;
                    continue;
                },
                '<' => {
                    state = .saw_left_angle_bracket;
                },
                '>' => {
                    state = .saw_right_angle_bracket;
                },
                '{' => {
                    result.ty = .lcbrace;
                    result.loc = .{
                        .start = self.cur,
                        .end = self.cur,
                    };
                    self.cur += 1;
                    return result;
                },
                '}' => {
                    result.ty = .rcbrace;
                    result.loc = .{
                        .start = self.cur,
                        .end = self.cur,
                    };
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
                    result.loc = .{
                        .start = self.cur,
                        .end = self.cur,
                    };
                    self.cur += 1;
                    return result;
                },
                ')' => {
                    result.ty = .close_paren;
                    result.loc = .{
                        .start = self.cur,
                        .end = self.cur,
                    };
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
                    var keyword_iter: u8 = 0;
                    while (keyword_iter < @typeInfo(Keyword).Enum.fields.len) : (keyword_iter += 1) {
                        const keyword = @intToEnum(Keyword, keyword_iter);
                        if (strEql(@tagName(keyword), thing)) {
                            result.ty = .keyword;
                            result.val = .{ .keyword = keyword };
                            result.loc.end = self.cur - 1;
                            self.cur += 1;
                            return result;
                        }
                    }
                    if (std.mem.trim(u8, thing, &[_]u8{ ' ', '\n', '\r' }).len == 0) {
                        self.cur += 1;
                        start_of_token = self.cur;
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
            .unsigned_int => {
                switch (c) {
                    '1'...'9' => {},
                    '0' => {},
                    ' ' => {
                        result.ty = .unsigned_int;
                        const parsed_uint = try std.fmt.parseUnsigned(u64, self.src[start_of_token..self.cur], 0);
                        result.val = .{ .unsigned_int = parsed_uint };
                        result.loc = .{ .start = start_of_token, .end = self.cur - 1 };
                        return result;
                    },
                    else => {
                        result.ty = .unsigned_int;
                        const parsed_uint = try std.fmt.parseUnsigned(u64, self.src[start_of_token .. self.cur - 1], 0);
                        result.val = .{ .unsigned_int = parsed_uint };
                        result.loc = .{ .start = start_of_token, .end = self.cur - 1 };
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
            .saw_left_angle_bracket => {
                switch (c) {
                    '=' => {
                        result.ty = .less_equal;
                        result.loc = .{
                            .start = start_of_token,
                            .end = self.cur,
                        };
                        self.cur += 1;
                        return result;
                    },
                    else => {
                        result.ty = .left_angle;
                        result.loc = .{
                            .start = start_of_token,
                            .end = self.cur - 1,
                        };
                        return result;
                    },
                }
            },
            .saw_right_angle_bracket => {
                switch (c) {
                    '=' => {
                        result.ty = .greater_equal;
                        result.loc = .{
                            .start = start_of_token,
                            .end = self.cur,
                        };
                        self.cur += 1;
                        return result;
                    },
                    else => {
                        result.ty = .right_angle;
                        result.loc = .{
                            .start = start_of_token,
                            .end = self.cur - 1,
                        };
                        return result;
                    },
                }
            },
        }
        self.cur += 1;
    }
}

test "int" {
    var t = Self.init("123");
    var tok = try t.next();

    try testing.expectEqual(Self.Token{ .ty = .unsigned_int, .val = .{ .unsigned_int = 123 }, .loc = .{ .start = 0, .end = 3 } }, tok);
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

test "identifiers" {
    var t = Self.init("asd");
    var tok = try t.next();

    try testing.expectEqual(Token.Type.identifier, tok.ty);
    try testing.expectEqual(Token.Val{ .identifier = "asd" }, tok.val);
    try testing.expectEqual(Token.Loc{
        .start = 0,
        .end = 2,
    }, tok.loc);
}

test "if and it's cond" {
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

    tok = try t.next();

    try testing.expectEqual(Token.Type.left_angle, tok.ty);
    try testing.expectEqual(Token.Loc{
        .start = 6,
        .end = 6,
    }, tok.loc);

    tok = try t.next();

    try testing.expectEqual(Token.Type.unsigned_int, tok.ty);
    try testing.expectEqual(@intCast(u64, 2), tok.val.unsigned_int);
    try testing.expectEqual(Token.Loc{
        .start = 8,
        .end = 8,
    }, tok.loc);

    tok = try t.next();

    try testing.expectEqual(Token.Type.close_paren, tok.ty);
    try testing.expectEqual(Token.Loc{
        .start = 10,
        .end = 10,
    }, tok.loc);

    tok = try t.next();

    try testing.expectEqual(Token.Type.lcbrace, tok.ty);
    try testing.expectEqual(Token.Loc{
        .start = 12,
        .end = 12,
    }, tok.loc);

    tok = try t.next();

    try testing.expectEqual(Token.Type.rcbrace, tok.ty);
    try testing.expectEqual(Token.Loc{
        .start = 13,
        .end = 13,
    }, tok.loc);
}

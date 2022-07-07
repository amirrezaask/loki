const std = @import("std");
const testing = std.testing;

pub const Self = @This();
pub const Keyword = enum { @"if", @"for", @"while", @"enum", @"struct", @"union", @"fn" };

pub const Token = union(enum) {
    EOF: void,
    lcbrace: void,
    rcbrace: void,
    lbrace: void,
    rbrace: void,
    open_paren: void,
    close_paren: void,
    comma: void,
    bang: void,
    sharp: void,
    dollor: void,

    minus: void,
    plus: void,
    asterix: void,
    forward_slash: void,
    percent: void,

    plus_equal: void,
    minus_equal: void,
    div_equal: void,
    mod_equal: void,
    mul_equal: void,

    equal: void,
    colon: void,
    double_colon: void,
    double_equal: void,
    semi_colon: void,
    ampersand: void,
    hat: void,
    atsign: void,
    dot: void,
    single_quote: void,
    double_quote: void,
    back_slash: void,
    pipe: void,
    identifier: []const u8,
    keyword: Keyword,
    char: u8,
    int: []const u8,
    float: []const u8,
    string_literal: []const u8,
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
};

src: []const u8,
cur: u64,

pub fn init(input: []const u8) Self {
    return .{ .cur = 0, .src = input };
}

pub fn next(self: *Self) !Token {
    var start_of_token = self.cur;
    var state: State = .start;
    while (true) {
        if (self.src.len <= self.cur) {
            switch(state) {
                .int_decimal => {
                    return Token { .int = self.src[start_of_token..self.cur]};
                },
                else => {}
            }
        }
        const c = self.src[self.cur]; 
        switch (state) {
            .start => switch (c) {
                '1'...'9' => {
                    state = .int_decimal;
                },
                '{' => {
                    return .lcbrace;
                },
                '}' => {
                    return .rcbrace;
                },
                '[' => {
                    return .lbrace;
                },
                ']' => {
                    return .rbrace;
                },
                '(' => {
                    return .open_paren;
                },
                ')' => {
                    return .close_paren;
                },
                ',' => {
                    return .comma;
                },
                '!' => {
                    state = .saw_bang;
                },
                '#' => {
                    return .sharp;
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
                    return .semi_colon;
                },
                '$' => {
                    return .dollor;
                },
                '&' => {
                    return .ampersand;
                },
                '^' => {
                    state = .saw_hat;
                },
                '@' => {
                    return .atsign;
                },
                '.' => {
                    return .dot;
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
                    return .back_slash;
                },
                '|' => {
                    return .pipe;
                },
                else => {},
            },
            .saw_colon => {
                switch (c) {
                    ':' => {
                        self.cur += 1;
                        return .double_colon;
                    },
                    else => {
                        return .colon;
                    },
                }
            },
            .saw_plus => {
                switch (c) {
                    '=' => {
                        self.cur += 1;
                        return .plus_equal;
                    },
                    else => {
                        return .plus;
                    },
                }
            },
            .saw_minus => {
                switch (c) {
                    '=' => {
                        self.cur += 1;
                        return .minus_equal;
                    },
                    else => {
                        return .minus;
                    },
                }
            },
            .saw_slash => {
                switch (c) {
                    '=' => {
                        self.cur += 1;
                        return .div_equal;
                    },
                    else => {
                        return .forward_slash;
                    },
                }
            },
            .saw_asterix => {
                switch (c) {
                    '=' => {
                        self.cur += 1;
                        return .mul_equal;
                    },
                    else => {
                        return .asterix;
                    },
                }
            },
            .saw_percent => {
                switch (c) {
                    '=' => {
                        self.cur += 1;
                        return .mod_equal;
                    },
                    else => {
                        return .percent;
                    },
                }
            },
            .string => {
                switch (c) {
                    '"' => {
                        // ending string
                        return Token{ .string_literal = self.src[start_of_token+1..self.cur] };
                    },
                    else => {},
                }
            },
            .int_decimal => {
                switch (c) {
                    '1'...'9' => {},
                    '0' => {},
                    else => {
                        return Token{ .int = self.src[start_of_token..self.cur] };
                    },
                }
            },
            .saw_equal => {
                switch (c) {
                    '=' => {
                        self.cur += 1;
                        return .double_equal;
                    },
                    else => {
                        return Token.equal;
                    },
                }
            },
            .in_char_literal => {
                switch (c) {
                    else => {
                        self.cur += 2;
                        return Token{ .char = self.src[self.cur]};
                    },
                }
            },
            .saw_bang => {},
            .saw_hat => {},
        }
        self.cur += 1;
    }
}

test "int" {
    var t = Self.init("123");
    var tok = try t.next();

    try testing.expectEqual(Self.Token{ .int = "123" }, tok);
}

test "string" {
    var t = Self.init("\"123\"");
    var tok = try t.next();

    try testing.expectEqualStrings(tok.string_literal, "123");

}

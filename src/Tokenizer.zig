pub const Self = @This();
pub const Keyword = enum { @"if", @"for", @"while", @"enum", @"struct", @"union", @"fn" };

pub const Token = union(enum) {
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
    decimal: u64,
    float: f64,
};

pub const State = enum {
    start,
    in_string,
    in_char_literal,
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
    return .{ .src = input };
}

pub fn next(self: Self) !Token {
    var state: State = .start;
    while (true) {
        const c = self.src[self.cur];
        switch (state) {
            .start => switch (c) {
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
                    state = .in_string;
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
            .in_string => {
                switch (c) {
                    '"' => {
                        break;
                    },
                    '\\' => {},
                    else => {
                        state = .in_string;
                    },
                }
            },
        }
        self.cur += 1;
    }
}

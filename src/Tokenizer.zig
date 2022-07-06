pub const State = enum {
    start,
};

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
    asterix: void,
    percent: void,
    dollor: void,
    minus: void,
    plus: void,
    equal: void,
    double_colon: void,
    ampersand: void,
    hat: void,
    atsign: void,
    dot: void,
    semi_colon: void,
    single_quote: void,
    double_quote: void,
    forward_slash: void,
    back_slash: void,
    pipe: void,
    identifier: []const u8,
    keyword: Keyword,
    char: u8,
    decimal: u64,
    float: f64,
};

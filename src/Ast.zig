const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const Loc = Tokenizer.Token.Loc;
const Self = @This();
pub const Decl = struct {
    name: []const u8,
    val: *Node,
};

pub const Struct = struct {
    name: []const u8,
};
pub const Union = struct {
    name: []const u8,
};
pub const Enum = struct {};
pub const FnCall = struct {
    name: *Node,
    args: []Node,
};
pub const Node = struct { data: union(enum) {
    @"undefined",
    @"const_decl": Decl,
    @"var_decl": Decl,
    @"import": []const u8,
    @"int": i64,
    @"unsigned_int": u64,
    @"float": f64,
    @"string_literal": []const u8,
    @"bool": bool,
    @"identifier": []const u8,
    @"char": u8,
    @"paren_expr": *Node,
    @"type_def_struct": Struct,
    @"type_def_enum": Enum,
    @"type_def_union": Union,
    @"increment": *Node,
    @"decrement": *Node,
    @"fn_call": FnCall,
}, loc: Loc };

top_level: std.ArrayList(Node),
pub fn deinit(self: *Self) void {
    self.top_level.deinit();
}

pub fn init(alloc: std.mem.Allocator) Self {
    return .{
        .top_level = std.ArrayList(Node).init(alloc),
    };
}

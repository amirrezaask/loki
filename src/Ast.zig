const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const Loc = Tokenizer.Token.Loc;
const Self = @This();
pub const Decl = struct {
    pub const Tag = enum {
        @"const",
        @"var",
    };
    name: []const u8,
    ty: Tag,
    val: *Node,
};

pub const FnSign = struct {
    args: [][2]*Node,
    ret_ty: *Node,
};

pub const FnDef = struct {
    signature: FnSign,
    block: []*Node,
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
pub const Node = struct {
    data: union(enum) {
        @"undefined",
        @"decl": Decl,
        @"import": []const u8,
        @"int": i64,
        @"uint": u64,
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
        @"fn_sign": FnSign,
        @"fn_def": FnDef,
        @"bool_ty",
        @"int_ty",
        @"uint_ty",
        @"char_ty",
        @"void_ty",
        @"float_ty",
        @"string_ty",
    },
    loc: Loc,
};

top_level: std.ArrayList(Node),

pub fn deinit(self: *Self, alloc: std.mem.Allocator) void {
    for (self.top_level.items) |node| {
        switch (node.data) {
            .@"decl" => {
                const ptr = node.data.decl.val;
                alloc.destroy(ptr);
            },
            else => {},
        }
    }
    self.top_level.deinit();
}

pub fn addTopLevelNode(self: *Self, node: Node) !void {
    try self.top_level.append(node);
}

pub fn init(alloc: std.mem.Allocator) Self {
    return .{
        .top_level = std.ArrayList(Node).init(alloc),
    };
}

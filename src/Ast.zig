const std = @import("std");
const Self = @This();

pub const Node = struct {
    pub const Decl = struct {
        ty: enum {
            @"const",
            @"var",
        },
        name: []const u8,
        val: Expr,
    };
    pub const Import = struct {
        path: []const u8,
    };

    pub const Struct = struct {};
    pub const Union = struct {};
    pub const Enum = struct {};
    pub const FnCall = struct {};
    pub const Expr = union(enum) {
        @"int": i64,
        @"float": f64,
        @"string": []const u8,
        @"bool": bool,
        @"identifier": []const u8,
        paren_expr: Expr,
        type_def: union(enum) {
            @"struct": Struct,
            @"enum": Enum,
            @"union": Union,
        },
        increment: Expr,
        decrement: Expr,
        call: FnCall,
    };
    pub const Type = enum {
        @"undefined",
        @"decl",
        @"import",
        @"expr",
    };
    pub const Val = union(enum) {
        @"undefined": void,
        @"decl": Decl,
        @"import": Import,
        @"expr": Expr,
    };
    ty: Type,
    val: Val,
};

top_level: std.ArrayList(Node),

pub fn init(alloc: std.mem.Allocator) Self {
    return .{
        .top_level = std.ArrayList(Node).init(alloc),
    };
}

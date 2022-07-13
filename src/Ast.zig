const std = @import("std");
const Tokenizer = @import("Tokenizer.zig");
const Loc = Tokenizer.Token.Loc;
const Self = @This();
const Error = @import("errors.zig").Error;
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
    args: []*Node,
};
pub const Node = struct {
    pub const Data = union(enum) {
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
    };
    data: Data,
    loc: Loc,

    pub fn initAlloc(self: Node, alloc: std.mem.Allocator) Error!*Node {
        var node_heap_ptr = alloc.create(Node) catch return Error.AllocationFailed;
        node_heap_ptr.* = self;
        return node_heap_ptr;
    }

    pub fn deinit(self: *Node, alloc: std.mem.Allocator) void {
        switch (self.data) {
            .@"decl" => {
                self.data.decl.val.deinit(alloc);
                alloc.destroy(self.data.decl.val);
            },
            .fn_call => {
                self.data.fn_call.name.deinit(alloc);
                alloc.destroy(self.data.fn_call.name);
                for (self.data.fn_call.args) |arg| {
                    arg.deinit(alloc);
                    alloc.destroy(arg);
                }
                alloc.free(self.data.@"fn_call".args);
            },
            .fn_def => {
                for (self.data.fn_def.block) |stmt| {
                    stmt.deinit(alloc);
                    alloc.destroy(stmt);
                }
                alloc.free(self.data.@"fn_def".block);
                for (self.data.fn_def.signature.args) |arg| {
                    arg[0].deinit(alloc);
                    alloc.destroy(arg[0]);
                    arg[1].deinit(alloc);
                    alloc.destroy(arg[1]);
                }
                alloc.free(self.data.fn_def.signature.args);
                self.data.fn_def.signature.ret_ty.deinit(alloc);
                alloc.destroy(self.data.fn_def.signature.ret_ty);
            },
            else => {},
        }
    }
};

top_level: std.ArrayList(Node),

pub fn deinit(self: *Self, alloc: std.mem.Allocator) void {
    for (self.top_level.items) |*node| {
        node.deinit(alloc);
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

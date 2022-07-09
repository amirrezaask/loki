const std = @import("std");
const Ast = @import("Ast.zig");
const Tokenizer = @import("Tokenizer.zig");
const Self = @This();

src: []const u8,

pub fn init(src: []const u8) Self {
    return .{
        .src = src,
    };
}

pub fn ast(self: Self, alloc: std.mem.Allocator) !Ast {
    const tokenizer = Tokenizer.init(self.src);
    var tokens = std.ArrayList(Tokenizer.Token).init(alloc);
    while (true) {
        const token = try tokenizer.next();
        if (token.ty == .EOF) break;
        tokens.append(token);
    }
}

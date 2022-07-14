pub const Ast = @import("Ast.zig");
const Self = @This();

generate: fn (ast: Ast) []const u8,

pub fn init(generator: fn (ast: Ast) []const u8) Self {
    return .{ .generate = generator };
}

pub fn generate(self: Self, ast: Ast) []const u8 {
    return self.generate(ast);
}

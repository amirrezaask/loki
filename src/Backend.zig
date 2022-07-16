const std = @import("std");
pub const Ast = @import("Ast.zig");
const Self = @This();

_generate: fn (allocator: std.mem.Allocator, ast: *Ast) []const u8,

pub fn init(generator: fn (allocator: std.mem.Allocator, ast: *Ast) []const u8) Self {
    return .{ ._generate = generator };
}

pub fn generate(self: Self, allocator: std.mem.Allocator, ast: *Ast) []const u8 {
    return self._generate(allocator, ast);
}

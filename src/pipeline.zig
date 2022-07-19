const std = @import("std");
const Backend = @import("Backend.zig");
const Parser = @import("Parser.zig");
const Self = @This();
backend: Backend,
src_path: []const u8,

pub fn init(backend: Backend, src_path: []const u8) Self {
    return .{
        .backend = backend,
        .src_path = src_path,
    };
}

pub fn emit(self: Self, alloc: std.mem.Allocator) void {
    var path_buffer: [std.fs.MAX_PATH_BYTES]u8 = undefined;

    const abs_path = std.fs.realpath(self.src_path, &path_buffer) catch unreachable;

    const src_file = std.fs.openFileAbsolute(abs_path, .{ .read = true }) catch unreachable;
    defer src_file.close();

    const src = src_file.readToEndAlloc(alloc, std.math.maxInt(usize)) catch unreachable;
    defer alloc.free(src);

    var parser = Parser.init(
        alloc,
        src,
    ) catch unreachable;
    defer parser.deinit();

    var ast = parser.getAst(alloc) catch unreachable;
    defer ast.deinit(alloc);

    const code = self.backend.generate(alloc, &ast);
    defer alloc.free(code);

    std.debug.print("{s}", .{code});
}
pub fn compile() void {}
pub fn run() void {}

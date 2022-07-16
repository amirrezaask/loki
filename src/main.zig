const std = @import("std");
const Pipeline = @import("Pipeline.zig");
const Backend = @import("Backend.zig");
const c_backend = @import("c.zig");

const main_usage =
    \\Usage:
    \\ loki [command] [args]
    \\
    \\Commands:
    \\ emit-c: Emits equivalent C code generated using C backend
    \\ compile: Compiles your code
    \\ run: Runs your code
;

fn strEql(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    const args = (try std.process.argsAlloc(alloc))[1..];

    // std.debug.print("{s}\n", .{args});
    if (args.len < 1) {
        std.debug.print("{s}\n", .{main_usage});
        return;
    }

    const command = args[0];
    if (strEql("help", command)) {
        std.debug.print("{s}\n", .{main_usage});
        return;
    } else if (strEql("emit-c", command)) {
        if (args.len < 2) {
            std.debug.print("emit-c command needs a filename\n", .{});
            return;
        }
        const file_name = args[1];
        var pipeline = Pipeline.init(Backend.init(c_backend.generate), file_name);
        pipeline.emit(alloc);
    }
}

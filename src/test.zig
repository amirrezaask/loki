const std = @import("std");
const U = union(enum) {
    @"undef",
    @"1",
    @"2",
};

const S = struct {
    list: std.ArrayList(U),
};

fn func() !S {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var alloc = gpa.allocator();
    var s = .{
        .list = std.ArrayList(U).init(alloc),
    };

    try s.list.append(U.@"1");
    try s.list.append(U.@"2");
    try s.list.append(U.@"undef");

    return s;
}

test "some" {
    var s = try func();

    for (s.list.items) |item| {
        std.debug.print("{}\n", .{item});
    }
}

const std = @import("std");

pub fn Stack(comptime T: type) type {
    return struct {
        allocator: std.mem.Allocator,
        array_list: std.ArrayList(T),
        pub fn init(alloc: std.mem.Allocator) @This() {
            return .{ .allocator = alloc, .array_list = std.ArrayList(T).init(alloc) };
        }

        pub fn deinit(self: *@This()) void {
            self.array_list.deinit();
        }

        pub fn pop(self: *@This()) ?T {
            if (self.array_list.items.len == 0) return null;
            const stack_top = self.array_list.items.len - 1;
            const last = self.array_list.items[stack_top];
            self.array_list.items = self.array_list.items[0 .. self.array_list.items.len - 1];
            return last;
        }

        pub fn push(self: *@This(), s: T) !void {
            try self.array_list.append(s);
        }

        pub fn top(self: *@This()) ?T {
            if (self.array_list.items.len == 0) return null;
            const stack_top = self.array_list.items.len - 1;
            if (stack_top < 0) return null;
            return self.array_list.items[stack_top];
        }
    };
}

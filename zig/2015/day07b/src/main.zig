const std = @import("std");

const Operation = struct {
    const Operator = enum { Assign, Not, And, Or, LeftShift, RightShift, };

    operator: Operator,
    src1: []const u8,
    src2: ?[]const u8,
    amt: ?u5,
};

fn usage(progname: []const u8) void {
    std.debug.print("usage: {s} <input file>\n", .{progname});
    std.process.exit(1);
}

fn processFile(allocator: std.mem.Allocator, filename: []const u8) !i32 {
    var operations = std.StringHashMap(Operation).init(allocator);
    var file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    var read_buf: [131072]u8 = undefined;
    var reader = file.reader(&read_buf);
    var r_interface = &reader.interface;

    while ( true ) {
        const line = r_interface.takeDelimiterExclusive('\n') catch |err| switch(err) {
            error.EndOfStream => break,
            else => return err,
        };

        const arrowIdx = std.mem.indexOf(u8, line, " -> ").?;
        const dest = try std.mem.Allocator.dupeZ(allocator, u8, line[(arrowIdx + 4)..]);
        var src1: []const u8 = undefined;
        var src2: ?[]const u8 = null;
        var amt: ?u5 = null;
        if ( std.mem.indexOf(u8, line, "NOT ") != null ) {
            src1 = try std.mem.Allocator.dupeZ(allocator, u8, line[4..arrowIdx]);
            try operations.put(dest, .{ .operator = Operation.Operator.Not, .src1 = src1, .src2 = null, .amt = null });
        } else if ( std.mem.indexOf(u8, line, " AND ") != null ) {
            const andIdx = std.mem.indexOf(u8, line, " AND ").?;
            src1 = try std.mem.Allocator.dupeZ(allocator, u8, line[0..andIdx]);
            src2 = try std.mem.Allocator.dupeZ(allocator, u8, line[(andIdx + 5)..arrowIdx]);
            try operations.put(dest, .{ .operator = Operation.Operator.And, .src1 = src1, .src2 = src2, .amt = null });
        } else if ( std.mem.indexOf(u8, line, " OR ") != null ) {
            const orIdx = std.mem.indexOf(u8, line, " OR ").?;
            src1 = try std.mem.Allocator.dupeZ(allocator, u8, line[0..orIdx]);
            src2 = try std.mem.Allocator.dupeZ(allocator, u8, line[(orIdx + 4)..arrowIdx]);
            try operations.put(dest, .{ .operator = Operation.Operator.Or, .src1 = src1, .src2 = src2, .amt = null });
        } else if ( std.mem.indexOf(u8, line, " LSHIFT ") != null ) {
            const lshiftIdx = std.mem.indexOf(u8, line, " LSHIFT ").?;
            src1 = try std.mem.Allocator.dupeZ(allocator, u8, line[0..lshiftIdx]);
            amt = try std.fmt.parseInt(u5, line[(lshiftIdx + 8)..arrowIdx], 10);
            try operations.put(dest, .{ .operator = Operation.Operator.LeftShift, .src1 = src1, .src2 = null, .amt = amt });
        } else if ( std.mem.indexOf(u8, line, " RSHIFT ") != null ) {
            const rshiftIdx = std.mem.indexOf(u8, line, " RSHIFT ").?;
            src1 = try std.mem.Allocator.dupeZ(allocator, u8, line[0..rshiftIdx]);
            amt = try std.fmt.parseInt(u5, line[(rshiftIdx + 8)..arrowIdx], 10);
            try operations.put(dest, .{ .operator = Operation.Operator.RightShift, .src1 = src1, .src2 = null, .amt = amt });
        } else {
            src1 = try std.mem.Allocator.dupeZ(allocator, u8, line[0..arrowIdx]);
            try operations.put(dest, .{ .operator = Operation.Operator.Assign, .src1 = src1, .src2 = null, .amt = null });
        }
    }

    var cache = std.StringHashMap(i32).init(allocator);
    const a = eval(allocator, &operations, &cache, "a");
    const aStr = try std.fmt.allocPrint(allocator, "{any}", .{a});
    try operations.put("b", .{ .operator = Operation.Operator.Assign, .src1 = aStr, .src2 = null, .amt = null });
    cache = std.StringHashMap(i32).init(allocator);
    return eval(allocator, &operations, &cache, "a");
}

fn eval(allocator: std.mem.Allocator, ops: *std.StringHashMap(Operation), cache: *std.StringHashMap(i32), expr: []const u8) !i32 {
    const maybeInt = std.fmt.parseInt(i32, expr, 10) catch null;
    if ( maybeInt ) |n| {
        return n;
    } else if ( cache.get(expr) ) |n| {
        return n;
    } else {
        const op = ops.get(expr).?;
        const r = switch ( op.operator ) {
            .Assign => try eval(allocator, ops, cache, op.src1),
            .Not => ~(try eval(allocator, ops, cache, op.src1)),
            .And => (try eval(allocator, ops, cache, op.src1)) & (try eval(allocator, ops, cache, op.src2.?)),
            .Or => (try eval(allocator, ops, cache, op.src1)) | (try eval(allocator, ops, cache, op.src2.?)),
            .LeftShift => (try eval(allocator, ops, cache, op.src1)) << op.amt.?,
            .RightShift => (try eval(allocator, ops, cache, op.src1)) >> op.amt.?,
        };
        const masked = r & 0xffff;
        try cache.put(expr, masked);
        return masked;
    }
}

pub fn main() !void {
    var stdout_buffer: [1024]u8 = undefined;
    const allocator = std.heap.page_allocator;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    var stdout = &stdout_writer.interface;
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if ( args.len < 2 ) {
        usage(args[0]);
    }

    const filename = args[1];

    const result = processFile(allocator, filename) catch |err| {
        std.debug.print("error while processing file `{s}': {s}\n", .{filename, @errorName(err)});
        std.process.exit(1);
    };

    try stdout.print("result = {}\n", .{result});
    try stdout.flush();
}

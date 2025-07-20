const std = @import("std");

fn usage(progname: []const u8) void {
    std.debug.print("usage: {s} <input file>\n", .{progname});
    std.process.exit(1);
}

fn min(a: u32, b: u32) u32 {
    return if (a < b) a else b;
}

fn processFile(allocator: std.mem.Allocator, filename: []const u8) !u32 {
    var file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    var reader = std.io.bufferedReader(file.reader());
    var stream = reader.reader();

    var total_len: u32 = 0;
    var line_buf = std.ArrayList(u8).init(allocator);
    defer line_buf.deinit();

    while ( try stream.readUntilDelimiterOrEofAlloc(allocator, '\n', 16) ) |line| {
        defer allocator.free(line);

        var parts_iter = std.mem.splitScalar(u8, line, 'x');
        var parts_count: usize = 0;
        var dims: [3]u32 = undefined;
        while ( parts_iter.next() ) |part| {
            if ( parts_count >= dims.len ) return error.InvalidInput;
            const val = std.fmt.parseInt(u32, part, 10) catch return error.InvalidNumber;
            dims[parts_count] = val;
            parts_count += 1;
        }

        const perim1 = 2 * (dims[0] + dims[1]);
        const perim2 = 2 * (dims[0] * dims[2]);
        const perim3 = 2 * (dims[1] * dims[2]);

        const present_len = min(min(perim1, perim2), perim3);
        const bow_len = dims[0] * dims[1] * dims[2];
        
        total_len += present_len + bow_len;
    }

    return total_len;
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const stdout = std.io.getStdOut().writer();
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
}

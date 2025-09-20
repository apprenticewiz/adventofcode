const std = @import("std");

fn usage(progname: []const u8) void {
    std.debug.print("usage: {s} <key>\n", .{progname});
    std.process.exit(1);
}

fn processFile(allocator: std.mem.Allocator, key: []const u8) !u32 {
    var n: u32 = 1;

    var buf = std.array_list.Managed(u8).init(allocator);
    defer buf.deinit();

    while ( true ) {
        const b = try std.fmt.allocPrint(allocator, "{s}{d}", .{key, n});
        defer allocator.free(b);

        const md5 = std.crypto.hash.Md5;
        var out: [md5.digest_length]u8 = undefined;
        md5.hash(b, &out, .{});

        const hex = try std.fmt.allocPrint(allocator, "{x}", .{out});
        defer allocator.free(hex);

        if ( std.mem.startsWith(u8, hex, "00000") ) {
            return n;
        }
        n += 1;
    }

    return 0;
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

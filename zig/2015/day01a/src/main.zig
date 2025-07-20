const std = @import("std");

fn usage(progname: []const u8) void {
    std.debug.print("usage: {s} <input file>\n", .{progname});
    std.process.exit(1);
}

fn processFile(allocator: std.mem.Allocator, filename: []const u8) !i32 {
    var file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    var reader = std.io.bufferedReader(file.reader());
    var stream = reader.reader();

    var counter: i32 = 0;
    var line_buf = std.ArrayList(u8).init(allocator);
    defer line_buf.deinit();

    while ( try stream.readUntilDelimiterOrEofAlloc(allocator, '\n', 131072)) |line| {
        defer allocator.free(line);
        var it = std.unicode.Utf8Iterator {
            .bytes = line,
            .i = 0,
        };
        while ( it.nextCodepoint() ) |cp| {
            switch ( cp ) {
                '(' => counter += 1,
                ')' => counter -= 1,
                else => {},
            }
        }
    }

    return counter;
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

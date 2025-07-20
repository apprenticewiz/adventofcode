const std = @import("std");

const geometry = @import("geometry");

fn usage(progname: []const u8) void {
    std.debug.print("usage: {s} <input file>\n", .{progname});
    std.process.exit(1);
}

fn processFile(allocator: std.mem.Allocator, filename: []const u8) !u32 {
    var file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    var reader = std.io.bufferedReader(file.reader());
    var stream = reader.reader();

    var line_buf = std.ArrayList(u8).init(allocator);
    defer line_buf.deinit();

    var santa = geometry.Position2D(i32).init(0, 0);
    var robo_santa = geometry.Position2D(i32).init(0, 0);
    var positions = std.AutoHashMap(geometry.Position2D(i32), void).init(allocator);
    var i: u32 = 0;
    defer positions.deinit();
    try positions.put(santa, {});

    while ( try stream.readUntilDelimiterOrEofAlloc(allocator, '\n', 16384) ) |line| {
        defer allocator.free(line);

        for ( line ) |ch| {
            if ( i % 2 == 0 ) {
                switch ( ch ) {
                    '^' => santa.y += 1,
                    'v' => santa.y -= 1,
                    '<' => santa.x -= 1,
                    '>' => santa.x += 1,
                    else => continue,
                }
                try positions.put(santa, {});
            } else {
                switch ( ch ) {
                    '^' => robo_santa.y += 1,
                    'v' => robo_santa.y -= 1,
                    '<' => robo_santa.x -= 1,
                    '>' => robo_santa.x += 1,
                    else => continue,
                }
                try positions.put(robo_santa, {});
            }
            i += 1;
        }
    }

    return positions.count();
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

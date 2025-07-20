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

    var total_area: u32 = 0;
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

        const area1 = dims[0] * dims[1];
        const area2 = dims[0] * dims[2];
        const area3 = dims[1] * dims[2];

        const surface_area = area1 * 2 + area2 * 2 + area3 * 2;
        const min_area = min(min(area1, area2), area3);
        
        total_area += surface_area + min_area;
    }

    return total_area;
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

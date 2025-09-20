const std = @import("std");

fn usage(progname: []const u8) void {
    std.debug.print("usage: {s} <input file>\n", .{progname});
    std.process.exit(1);
}

fn min(a: u32, b: u32) u32 {
    return if (a < b) a else b;
}

fn processFile(filename: []const u8) !u32 {
    var file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    var read_buf: [131072]u8 = undefined;
    var reader = file.reader(&read_buf);
    var r_interface = &reader.interface;

    var total_area: u32 = 0;

    while ( true ) {
        const line = r_interface.takeDelimiterExclusive('\n') catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };
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

    const result = processFile(filename) catch |err| {
        std.debug.print("error while processing file `{s}': {s}\n", .{filename, @errorName(err)});
        std.process.exit(1);
    };

    try stdout.print("result = {}\n", .{result});
    try stdout.flush();
}

const std = @import("std");

fn usage(progname: []const u8) void {
    std.debug.print("usage: {s} <input file>\n", .{progname});
    std.process.exit(1);
}

fn processFile(filename: []const u8) !i32 {
    var file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    var read_buf: [1000000]u8 = undefined;
    var reader = file.reader(&read_buf);
    var r_interface = &reader.interface;

    var counter: i32 = 0;
    var pos: i32 = 0;

    while ( true ) {
        const line = r_interface.takeDelimiterExclusive('\n') catch |err| switch(err) {
            error.EndOfStream => break,
            else => return err,
        };
        for ( line ) |ch| {
            pos += 1;
            switch (ch) {
                '(' => counter += 1,
                ')' => counter -= 1,
                else => {},
            }
            if ( counter < 0 ) {
                return pos;
            }
        }
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

    const result = processFile(filename) catch |err| {
        std.debug.print("error while processing file `{s}': {s}\n", .{filename, @errorName(err)});
        std.process.exit(1);
    };

    try stdout.print("result = {}\n", .{result});
    try stdout.flush();
}

const std = @import("std");

fn usage(progname: []const u8) void {
    std.debug.print("usage: {s} <input file>\n", .{progname});
    std.process.exit(1);
}

fn prop1(line: []const u8) bool {
    var vowels: u32 = 0;
    for ( line ) |ch| {
        switch ( ch ) {
            'a', 'e', 'i', 'o', 'u' => vowels += 1,
            else => {},
        }
    }
    return vowels >= 3;
}

fn prop2(line: []const u8) bool {
    for (0..(line.len - 1)) |i| {
        if ( line[i] == line[i + 1] ) {
            return true;
        }
    }
    return false;
}

fn prop3(line: []const u8) bool {
    return std.mem.count(u8, line, "ab") == 0 and
        std.mem.count(u8, line, "cd") == 0 and
        std.mem.count(u8, line, "pq") == 0 and
        std.mem.count(u8, line, "xy") == 0;
}

fn processFile(filename: []const u8) !u32 {
    var file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    var read_buf: [131072]u8 = undefined;
    var reader = file.reader(&read_buf);
    var r_interface = &reader.interface;

    var result: u32 = 0;

    while ( true ) {
        const line = r_interface.takeDelimiterExclusive('\n') catch |err| switch(err) {
            error.EndOfStream => break,
            else => return err,
        };
        if ( prop1(line) and prop2(line) and prop3(line) ) {
            result += 1;
        }
    }

    return result;
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

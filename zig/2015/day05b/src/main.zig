const std = @import("std");

fn usage(progname: []const u8) void {
    std.debug.print("usage: {s} <input file>\n", .{progname});
    std.process.exit(1);
}

fn prop1(line: []const u8) bool {
    for (0..(line.len - 3)) |i| {
        const first_pair = line[i..i + 2];
        for ((i + 2)..(line.len - 1)) |j| {
            const second_pair = line[j..j + 2];
            if ( std.mem.eql(u8, first_pair, second_pair) ) {
                return true;
            }
        }
    }
    return false;
}

fn prop2(line: []const u8) bool {
    for (0..(line.len - 2)) |i| {
        if ( line[i] == line[i + 2] ) {
            return true;
        }
    }
    return false;
}

fn processFile(allocator: std.mem.Allocator, filename: []const u8) !u32 {
    var file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    var reader = std.io.bufferedReader(file.reader());
    var stream = reader.reader();

    var result: u32 = 0;
    var line_buf = std.ArrayList(u8).init(allocator);
    defer line_buf.deinit();

    while ( try stream.readUntilDelimiterOrEofAlloc(allocator, '\n', 256) ) |line| {
        defer allocator.free(line);
        if ( prop1(line) and prop2(line) ) {
            result += 1;
        }
    }

    return result;
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

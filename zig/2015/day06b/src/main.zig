const std = @import("std");

const Action = enum {
    TurnOn,
    TurnOff,
    Toggle,
};

var grid:[1000*1000]i32 = .{0} ** (1000*1000); 

fn usage(progname: []const u8) void {
    std.debug.print("usage: {s} <input file>\n", .{progname});
    std.process.exit(1);
}

fn perform(action: Action, r1: u32, c1: u32, r2: u32, c2: u32) void {
    for (r1..(r2+1)) |row| {
        for (c1..(c2+1)) |col| {
            switch ( action ) {
                Action.TurnOn => grid[row*1000 + col] += 1,
                Action.TurnOff => grid[row*1000 + col] =
                    if (grid[row*1000 + col] > 0) (grid[row*1000 + col] - 1) else 0,
                Action.Toggle => grid[row*1000 + col] += 2,
            }
        }
    }
}

fn sum() u32 {
    var total: u32 = 0;
    for (0..1000) |row| {
        for (0..1000) |col| {
            total += @intCast(grid[row*1000 + col]);
        }
    }
    return total;
}

fn processFile(filename: []const u8) !u32 {
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

        var action: Action = undefined;
        var coordIdx: usize = 0;
        if ( std.mem.startsWith(u8, line, "turn on") ) {
            action = Action.TurnOn;
            coordIdx = 8;
        } else if ( std.mem.startsWith(u8, line, "turn off") ) {
            action = Action.TurnOff;
            coordIdx = 9;
        } else if ( std.mem.startsWith(u8, line, "toggle") ) {
            action = Action.Toggle;
            coordIdx = 7;
        } else {
            std.debug.print("error: malformed input line: {s}\n", .{line});
            std.process.exit(1);
        }
        const coords = line[coordIdx..];
        const throughIdx = std.mem.indexOf(u8, coords, " through ").?;
        const coord1 = coords[0..throughIdx];
        const coord2 = coords[throughIdx+9..];
        const comma1Idx = std.mem.indexOf(u8, coord1, ",").?;
        const r1 = try std.fmt.parseInt(u32, coord1[0..comma1Idx], 10);
        const c1 = try std.fmt.parseInt(u32, coord1[comma1Idx+1..], 10);
        const comma2Idx = std.mem.indexOf(u8, coord2, ",").?;
        const r2 = try std.fmt.parseInt(u32, coord2[0..comma2Idx], 10);
        const c2 = try std.fmt.parseInt(u32, coord2[comma2Idx+1..], 10);
        perform(action, r1, c1, r2, c2);
    }

    return sum();
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

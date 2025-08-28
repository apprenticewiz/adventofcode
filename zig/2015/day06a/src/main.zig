const std = @import("std");

const Action = enum {
    TurnOn,
    TurnOff,
    Toggle,
};

fn usage(progname: []const u8) void {
    std.debug.print("usage: {s} <input file>\n", .{progname});
    std.process.exit(1);
}

fn perform(grid: *[1000*1000]bool, action: Action, r1: u32, c1: u32, r2: u32, c2: u32) void {
    for (r1..(r2+1)) |row| {
        for (c1..(c2+1)) |col| {
            switch ( action ) {
                Action.TurnOn => grid.*[row*1000 + col] = true,
                Action.TurnOff => grid.*[row*1000 + col] = false,
                Action.Toggle => grid.*[row*1000 + col] = !grid.*[row*1000 + col],
            }
        }
    }
}

fn count(grid: *[1000*1000]bool) u32 {
    var total: u32 = 0;
    for (0..1000) |row| {
        for (0..1000) |col| {
            if ( grid.*[row*1000 + col] ) {
                total += 1;
            }
        }
    }
    return total;
}

fn processFile(allocator: std.mem.Allocator, filename: []const u8) !u32 {
    var grid: [1000*1000]bool = .{false} ** (1000*1000);
    var file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    var reader = std.io.bufferedReader(file.reader());
    var stream = reader.reader();

    var line_buf = std.ArrayList(u8).init(allocator);
    defer line_buf.deinit();
    
    while ( try stream.readUntilDelimiterOrEofAlloc(allocator, '\n', 256) ) |line| {
        defer allocator.free(line);

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
        perform(&grid, action, r1, c1, r2, c2);
    }

    return count(&grid);
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

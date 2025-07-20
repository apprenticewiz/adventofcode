pub fn Position2D(comptime T: type) type {
    return struct {
        x: T,
        y: T,

        pub fn init(x: T, y: T) Position2D(T) {
            return Position2D(T){ .x = x, .y = y };
        }

        pub fn hash(self: Position2D(T)) u32 {
            return @as(u32, self.x) * 31 + @as(u32, self.y);
        }

        pub fn eql(a: Position2D(T), b: Position2D(T)) bool {
            return a.x == b.x and a.y == b.y;
        }
    };
}

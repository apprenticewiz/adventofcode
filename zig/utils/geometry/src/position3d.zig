pub fn Position3D(comptime T: type) type {
    return struct {
        x: T,
        y: T,
        z: T,

        pub fn init(x: T, y: T, z: T) Position3D(T) {
            return Position3D(T){ .x = x, .y = y, .z = z };
        }

        pub fn hash(self: Position3D(T)) u32 {
            return (@as(u32, self.x) * 31 + @as(u32, self.y)) * 31 + @as(u32, self.z);
        }

        pub fn eql(a: Position3D(T), b: Position3D(T)) bool {
            return a.x == b.x and a.y == b.y and a.z == b.z;
        }
    };
}

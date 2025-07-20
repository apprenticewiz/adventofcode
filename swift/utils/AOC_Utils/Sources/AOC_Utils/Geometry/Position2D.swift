extension Geometry {
    public struct Position2D<T: Numeric & Hashable>: Hashable {
        public var x: T
        public var y: T

        public init(_ x: T, _ y: T) {
            self.x = x
            self.y = y
        }
    }
}

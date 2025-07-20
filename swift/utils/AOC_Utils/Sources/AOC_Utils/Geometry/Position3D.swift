extension Geometry {
    public struct Position3D<T: Numeric & Hashable>: Hashable {
        public var x: T
        public var y: T
        public var z: T

        public init(_ x: T, _ y: T, _ z: T) {
            self.x = x
            self.y = y
            self.z = z
        }
    }
}

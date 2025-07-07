use num_traits::Num;

#[derive (Eq, Clone, Copy, Hash, PartialEq)]
pub struct Position2D<T: Num> {
    pub x: T,
    pub y: T,
}

impl<T: Num> Position2D<T> {
    pub fn new(x: T, y: T) -> Position2D<T> {
        Position2D::<T> { x, y }
    }
}

#[derive (Eq, Clone, Copy, Hash, PartialEq)]
pub struct Position3D<T: Num> {
    pub x: T,
    pub y: T,
    pub z: T,
}

impl<T: Num> Position3D<T> {
    pub fn new(x: T, y: T, z: T) -> Position3D<T> {
        Position3D::<T> { x, y, z }
    }
}

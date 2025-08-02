class Position2D<T> {
    final T x;
    final T y;

    Position2D(this.x, this.y);

    @override
    bool operator ==(Object other) =>
        other is Position2D<T> && other.x == x && other.y == y;
    
    @override
    int get hashCode => x.hashCode ^ y.hashCode;
}

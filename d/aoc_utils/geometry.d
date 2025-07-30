module aoc_utils.geometry;

struct Position2D(T)
    if ( is(T : int) || is(T : float) )
{
    T x;
    T y;

    this(T x_, T y_) {
        x = x_;
        y = y_;
    }
}

struct Position3D(T)
    if ( is(T : int) || is(T : float) )
{
    T x;
    T y;
    T z;

    this(T x_, T y_, T z_) {
        x = x_;
        y = y_;
        z = z_;
    }
}

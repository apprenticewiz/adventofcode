package aoc_utils.geometry;

public class Position3D<T> {
    public T x;
    public T y;
    public T z;

    public Position3D(T x, T y, T z) {
        this.x = x;
        this.y = y;
	this.z = z;
    }
}

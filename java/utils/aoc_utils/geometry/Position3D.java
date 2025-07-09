package aoc_utils.geometry;

import java.util.Objects;

public class Position3D<T> {
    public T x;
    public T y;
    public T z;

    public Position3D(T x, T y, T z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    public Position3D(Position3D<T> other) {
        this.x = other.x;
        this.y = other.y;
        this.z = other.z;
    }

    public boolean equals(Object obj) {
        if ( this == obj ) {
            return true;
        } else if ( !(obj instanceof Position3D<?> other) ) {
            return false;
        } else {
            return x == other.x && y == other.y && z == other.z;
        }
    }

    public int hashCode() {
        return Objects.hash(x, y, z);
    }
}

package aoc_utils.geometry;

import java.util.Objects;

public class Position2D<T> {
    public T x;
    public T y;

    public Position2D(T x, T y) {
        this.x = x;
        this.y = y;
    }

    public Position2D(Position2D<T> other) {
        this.x = other.x;
        this.y = other.y;
    }

    public boolean equals(Object obj) {
        if ( this == obj ) {
            return true;
        } else if ( !(obj instanceof Position2D<?> other) ) {
            return false;
        } else {
            return x == other.x && y == other.y;
        }
    }

    public int hashCode() {
        return Objects.hash(x, y);
    }
}

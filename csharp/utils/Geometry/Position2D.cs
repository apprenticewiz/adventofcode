using System;
using System.Numerics;

namespace AOC_Utils.Geometry;

public struct Position2D<T> : IEquatable<Position2D<T>>
    where T : INumber<T>
{
    public T X { get; set; }
    public T Y { get; set; }

    public Position2D(T x, T y)
    {
        X = x;
        Y = y;
    }

    public Position2D<T> Add(Position2D<T> other)
    {
        return new Position2D<T>(X + other.X, Y + other.Y);
    }

    public override bool Equals(object? obj) => obj is Position2D<T> other && Equals(other);
    public bool Equals(Position2D<T> other) => X == other.X && Y == other.Y;

    public override int GetHashCode() => HashCode.Combine(X, Y);
    public override string ToString() => $"Position2D({X}, {Y})";
}

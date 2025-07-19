using System;
using System.Numerics;

namespace AOC_Utils.Geometry;

public struct Position3D<T> : IEquatable<Position3D<T>>
    where T : INumber<T>
{
    public T X { get; set; }
    public T Y { get; set; }
    public T Z { get; set; }

    public Position3D(T x, T y, T z)
    {
        X = x;
        Y = y;
	Z = z;
    }

    public Position3D<T> Add(Position3D<T> other)
    {
        return new Position3D<T>(X + other.X, Y + other.Y, Z + other.Z);
    }

    public override bool Equals(object? obj) => obj is Position3D<T> other && Equals(other);
    public bool Equals(Position3D<T> other) => X == other.X && Y == other.Y && Z == other.Z;

    public override int GetHashCode() => HashCode.Combine(X, Y, Z);
    public override string ToString() => $"Position3D({X}, {Y}, {Z})";
}

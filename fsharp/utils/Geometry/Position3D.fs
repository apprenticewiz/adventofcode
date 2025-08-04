namespace AOC_Utils.Geometry

open System
open System.Numerics

[<Struct>]
type Position3D<'T when 'T :> INumber<'T> and 'T : equality> =
    val X: 'T
    val Y: 'T
    val Z: 'T

    new(x: 'T, y: 'T, z: 'T) = { X = x; Y = y; Z = z }

    member this.Add(other: Position3D<'T>) =
        Position3D<'T>(this.X + other.X, this.Y + other.Y, this.Z + other.Z)

    override this.ToString() =
        $"Position3D({this.X}, {this.Y}, {this.Z})"

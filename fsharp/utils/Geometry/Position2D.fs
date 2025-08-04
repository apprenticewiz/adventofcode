namespace AOC_Utils.Geometry

open System
open System.Numerics

[<Struct>]
type Position2D<'T when 'T :> INumber<'T> and 'T : equality> =
    val X: 'T
    val Y: 'T

    new(x: 'T, y: 'T) = { X = x; Y = y }

    member this.Add(other: Position2D<'T>) =
        Position2D<'T>(this.X + other.X, this.Y + other.Y)

    override this.ToString() =
        $"Position2D({this.X}, {this.Y})"

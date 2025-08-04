Imports System

Namespace Geometry
	Public Structure Position2D(Of T)
		Implements IEquatable(Of Position2D(Of T))

		Public Property X As T
		Public Property Y As T

		Public Sub New(x As T, y As T)
			Me.X = x
			Me.Y = y
		End Sub

		Public Overloads Overrides Function Equals(obj As Object) As Boolean
			If TypeOf obj Is Position2D(Of T) Then
				Return Equals(CType(obj, Position2D(Of T)))
			End If
			Return False
		End Function

		Public Overloads Function Equals(other As Position2D(Of T)) As Boolean Implements IEquatable(Of Position2D(Of T)).Equals
			Return Me.X.Equals(other.X) AndAlso Me.Y.Equals(other.Y)
		End Function

		Public Overrides Function GetHashCode() As Integer
			Return HashCode.Combine(Me.X, Me.Y)
		End Function

		Public Overrides Function ToString() As String
			Return $"Position2D({Me.X}, {Me.Y})"
		End Function
	End Structure
End Namespace

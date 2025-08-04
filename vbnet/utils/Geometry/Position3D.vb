Imports System

Namespace Geometry
	Public Structure Position3D(Of T)
		Implements IEquatable(Of Position3D(Of T))

		Public Property X As T
		Public Property Y As T
		Public Property Z As T

		Public Sub New(x As T, y As T, z As T)
			Me.X = x
			Me.Y = y
			Me.Z = z
		End Sub

		Public Overloads Overrides Function Equals(obj As Object) As Boolean
			If TypeOf obj Is Position3D(Of T) Then
				Return Equals(CType(obj, Position3D(Of T)))
			End If
			Return False
		End Function

		Public Overloads Function Equals(other As Position3D(Of T)) As Boolean Implements IEquatable(Of Position3D(Of T)).Equals
			Return Me.X.Equals(other.X) AndAlso Me.Y.Equals(other.Y) AndAlso Me.Z.Equals(other.Z)
		End Function

		Public Overrides Function GetHashCode() As Integer
			Return HashCode.Combine(Me.X, Me.Y, Me.Z)
		End Function

		Public Overrides Function ToString() As String
			Return $"Position3D({Me.X}, {Me.Y}, {Me.Z})"
		End Function
	End Structure
End Namespace

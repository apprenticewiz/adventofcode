IMPLEMENTATION MODULE Geometry;

FROM DynamicStrings IMPORT String, ConCat, ConCatChar, InitStringChar;
FROM StringConvert IMPORT IntegerToString;

PROCEDURE Position2DToString(P : Position2D) : String;
VAR
    S : String;
BEGIN
    S := InitStringChar('(');
    S := ConCat(S, IntegerToString(P.X, 1, ' ', FALSE, 10, FALSE));
    S := ConCatChar(S, ',');
    S := ConCat(S, IntegerToString(P.Y, 1, ' ', FALSE, 10, FALSE));
    S := ConCatChar(S, ')');
    RETURN S;
END Position2DToString;

END Geometry.

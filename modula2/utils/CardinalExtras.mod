IMPLEMENTATION MODULE CardinalExtras;

PROCEDURE MinVal(X, Y : CARDINAL) : CARDINAL;
BEGIN
    IF X < Y THEN
        RETURN X;
    ELSE
        RETURN Y;
    END;
END MinVal;

END CardinalExtras.

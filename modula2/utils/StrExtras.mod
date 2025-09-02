IMPLEMENTATION MODULE StrExtras;

FROM StrLib IMPORT StrLen;

PROCEDURE CardToStr(Src      : CARDINAL;
                    VAR Dest : ARRAY OF CHAR);
VAR
    TempBuf : ARRAY [0..7] OF CHAR;
    I, N    : CARDINAL;
    Length  : CARDINAL;
    Digit   : CARDINAL;

BEGIN
    I := 0;
    N := Src;
    LOOP
        Digit := N MOD 10;
        TempBuf[I] := CHR(ORD('0') + Digit);
        N := N DIV 10;
        INC(I);
        IF N = 0 THEN
            EXIT;
        END;
    END;
    Length := I;
    FOR I := 0 TO (Length - 1) DO
        Dest[I] := TempBuf[Length - 1 - I];
    END;
    Dest[Length] := 0C;
END CardToStr;

PROCEDURE StrCompare(Str1, Str2 : ARRAY OF CHAR) : INTEGER;
VAR
    I : CARDINAL;

BEGIN
    I := 0;
    WHILE (Str1[I] # 0C) AND (Str2[I] # 0C) DO
        IF Str1[I] < Str2[I] THEN
            RETURN -1;
        ELSIF Str1[I] > Str2[I] THEN
            RETURN 1;
        ELSE
            INC(I);
        END;
    END;
    IF Str1[I] < Str2[I] THEN
        RETURN -1;
    ELSIF Str1[I] > Str2[I] THEN
        RETURN 1;
    ELSE
        RETURN 0;
    END;

END StrCompare;

PROCEDURE StrIndex(Haystack  : ARRAY OF CHAR;
                   Needle    : ARRAY OF CHAR;
                   VAR Found : BOOLEAN) : CARDINAL;
VAR
    I           : CARDINAL;
    HaystackLen : CARDINAL;
    NeedleLen   : CARDINAL;
    Sub         : ARRAY[1..128] OF CHAR;

BEGIN
    I := 0;
    Found := FALSE;
    HaystackLen := StrLen(Haystack);
    NeedleLen := StrLen(Needle);
    IF NeedleLen = 0 THEN
        Found := TRUE;
    ELSIF NeedleLen > HaystackLen THEN
        Found := FALSE;
    ELSE
        WHILE I <= HaystackLen - NeedleLen DO
            StrSlice(Haystack, Sub, I, NeedleLen);
            IF StrCompare(Sub, Needle) = 0 THEN
               Found := TRUE;
               RETURN I;
            ELSE
               INC(I);
            END;
        END;
    END;
    RETURN 0;

END StrIndex;

PROCEDURE StrSlice(Src      : ARRAY OF CHAR;
                   VAR Dest : ARRAY OF CHAR;
                   From     : CARDINAL;
                   Length   : CARDINAL);
VAR
    I, J, ActualLen : CARDINAL;

BEGIN
    I := From;
    J := 0;
    IF Length = 0 THEN
        ActualLen := StrLen(Src) - From;
    ELSE
        ActualLen := Length;
    END;
    WHILE (J < ActualLen) AND (Src[I] # 0C) DO
        Dest[J] := Src[I];
        INC(I);
        INC(J);
    END;
    Dest[J] := 0C;

END StrSlice;

PROCEDURE StrToCard(Src         : ARRAY OF CHAR;
                    VAR Dest    : CARDINAL;
                    VAR Success : BOOLEAN);
VAR
    Result : CARDINAL;
    I      : CARDINAL;

BEGIN
    Dest := 0;
    Success := TRUE;
    I := 0;
    WHILE (Src[I] # 0C) AND Success DO
        IF NOT ((Src[I] >= '0') AND (Src[I] <= '9')) THEN
            Success := FALSE;
        ELSE
            Dest := Dest * 10 + (ORD(Src[I]) - ORD('0'));
            INC(I);
        END;
    END;
END StrToCard;

END StrExtras.

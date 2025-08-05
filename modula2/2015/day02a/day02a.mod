MODULE day02a;

FROM FIO      IMPORT Close, EOF, File, IsNoError, OpenToRead, ReadString;
FROM InOut    IMPORT WriteCard, WriteLn, WriteString;
FROM libc     IMPORT exit;
FROM Strings  IMPORT Extract, FindNext, Length;
FROM SYSTEM   IMPORT ADDRESS;
FROM UnixArgs IMPORT GetArgC, GetArgV;

TYPE
    Arg = ARRAY [0..1023] OF CHAR;
    ArgPointer = POINTER TO Arg;
    ArgVArray = ARRAY [0..1023] OF ArgPointer;
    ArgVPointer = POINTER TO ArgVArray;

VAR
    Argc          : CARDINAL;
    Argv          : ArgVPointer;
    ProgName      : Arg;
    FileName      : Arg;
    Result        : INTEGER;

PROCEDURE Usage(ProgName : Arg);
BEGIN
    WriteString('usage: ');
    WriteString(ProgName);
    WriteString(' <input file>');
    WriteLn;
    exit(1);
END Usage;

PROCEDURE ParseCard(S: ARRAY OF CHAR) : CARDINAL;
VAR
    I, Result     : CARDINAL;

BEGIN
    Result := 0;
    FOR I := 0 TO Length(S) - 1 DO
       IF (S[I] >= '0') AND (S[I] <= '9') THEN
           Result := Result * 10 + ORD(S[I]) - ORD('0');
       END;
    END;
    RETURN Result;
END ParseCard;

PROCEDURE MinVal(X : CARDINAL; Y : CARDINAL) : CARDINAL;
BEGIN
    IF X < Y THEN
        RETURN X;
    ELSE
        RETURN Y;
    END;
END MinVal;

PROCEDURE Process(FileName : Arg) : CARDINAL;
VAR
    InFile               : File;
    TotalArea            : CARDINAL;
    Line, Tok            : ARRAY [1..16] OF CHAR;
    Found                : BOOLEAN;
    X1, X2               : CARDINAL;
    L, W, H              : CARDINAL;
    Area1, Area2, Area3  : CARDINAL;
    SurfaceArea, MinArea : CARDINAL;

BEGIN
    TotalArea := 0;
    InFile := OpenToRead(FileName);
    IF NOT IsNoError(InFile) THEN
        WriteString('error: unable to open input file: ');
        WriteString(FileName);
        WriteLn;
        exit(1);
    END;
    LOOP
        ReadString(InFile, Line);
        IF EOF(InFile) THEN
            EXIT;
        END;
        FindNext('x', Line, 1, Found, X1);
        Extract(Line, 0, X1, Tok);
        L := ParseCard(Tok);
        FindNext('x', Line, X1 + 1, Found, X2);
        Extract(Line, X1 + 1, X2 - X1 - 1, Tok);
        W := ParseCard(Tok);
        Extract(Line, X2 + 1, Length(Line) - X2, Tok);
        H := ParseCard(Tok);
        Area1 := L * W;
        Area2 := L * H;
        Area3 := W * H;
        SurfaceArea := 2 * Area1 + 2 * Area2 + 2 * Area3;
        MinArea := MinVal(Area1, MinVal(Area2, Area3));
        TotalArea := TotalArea + SurfaceArea + MinArea;
    END;
    Close(InFile);
    RETURN TotalArea;
END Process;

BEGIN
    Argc := CARDINAL(GetArgC());
    Argv := ArgVPointer(GetArgV());
    ProgName := Argv^[0]^; 
    IF Argc < 2 THEN
        Usage(ProgName);
    END;
    FileName := Argv^[1]^;
    Result := Process(FileName);
    WriteString('result = ');
    WriteCard(Result, 1);
    WriteLn;

END day02a.

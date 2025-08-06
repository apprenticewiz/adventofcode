MODULE day02a;

FROM DynamicStrings   IMPORT String, Index, Slice;
FROM FIO              IMPORT File, Close, EOF, IsNoError;
FROM InOut            IMPORT WriteCard, WriteLn, WriteString;
FROM libc             IMPORT exit;
FROM SFIO             IMPORT OpenToRead;
FROM StringConvert    IMPORT StringToCardinal;

FROM Args             IMPORT ArgCount, GetArgument;
FROM CardinalExtras   IMPORT MinVal;
FROM DynamicStringFIO IMPORT ReadDynString;
FROM DynamicStringIO  IMPORT WriteDynString;

VAR
    Argc          : CARDINAL;
    ProgName      : String;
    FileName      : String;
    Result        : CARDINAL;

PROCEDURE Usage(ProgName : String);
BEGIN
    WriteString('usage: ');
    WriteDynString(ProgName);
    WriteString(' <input file>');
    WriteLn;
    exit(1);
END Usage;

PROCEDURE Process(FileName : String) : CARDINAL;
VAR
    InFile               : File;
    TotalArea            : CARDINAL;
    Line, Tok            : String;
    Found                : BOOLEAN;
    X1, X2               : INTEGER;
    L, W, H              : CARDINAL;
    Area1, Area2, Area3  : CARDINAL;
    SurfaceArea, MinArea : CARDINAL;

BEGIN
    TotalArea := 0;
    InFile := OpenToRead(FileName);
    IF NOT IsNoError(InFile) THEN
        WriteString('error: unable to open input file: ');
        WriteDynString(FileName);
        WriteLn;
        exit(1);
    END;
    LOOP
        Line := ReadDynString(InFile);
        IF EOF(InFile) THEN
            EXIT;
        END;
        X1 := Index(Line, 'x', 0);
        Tok := Slice(Line, 0, X1);
        L := StringToCardinal(Tok, 10, Found);
        X2 := Index(Line, 'x', X1 + 1);
        Tok := Slice(Line,  X1 + 1, X2);
        W := StringToCardinal(Tok, 10, Found);
        Tok := Slice(Line, X2 + 1, 0);
        H := StringToCardinal(Tok, 10, Found);
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
    Argc := ArgCount();
    ProgName := GetArgument(0);
    IF Argc < 2 THEN
        Usage(ProgName);
    END;
    FileName := GetArgument(1);
    Result := Process(FileName);
    WriteString('result = ');
    WriteCard(Result, 1);
    WriteLn;

END day02a.

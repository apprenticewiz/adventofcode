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
    TotalLen            : CARDINAL;
    Line, Tok            : String;
    Found                : BOOLEAN;
    X1, X2               : INTEGER;
    L, W, H              : CARDINAL;
    Perim1, Perim2, Perim3  : CARDINAL;
    PresentLen, BowLen : CARDINAL;

BEGIN
    TotalLen := 0;
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
        Perim1 := 2 * (L + W);
        Perim2 := 2 * (L + H);
        Perim3 := 2 * (W + H);
        PresentLen := MinVal(Perim1, MinVal(Perim2, Perim3));
        BowLen := L * W * H;
        TotalLen := TotalLen + PresentLen + BowLen;
    END;
    Close(InFile);
    RETURN TotalLen;
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

MODULE day06a;

FROM DynamicStrings   IMPORT String, Equal, Index, InitString, Length, Slice, char;
FROM FIO              IMPORT File, Close, EOF, IsNoError;
FROM InOut            IMPORT WriteCard, WriteLn, WriteString;
FROM libc             IMPORT exit;
FROM SFIO             IMPORT OpenToRead;
FROM StringConvert    IMPORT StringToCardinal;

FROM Args             IMPORT ArgCount, GetArgument;
FROM DynamicStringFIO IMPORT ReadDynString;
FROM DynamicStringIO  IMPORT WriteDynString;

CONST
    RowMax = 1000;
    ColMax = 1000;

TYPE
    GridType = ARRAY [1..RowMax] OF ARRAY [1..ColMax] OF BOOLEAN;
    ActionType = (TurnOn, TurnOff, Toggle);

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

PROCEDURE InitGrid(VAR Grid : GridType);
VAR
    Row, Col             : CARDINAL;
BEGIN
    FOR Row := 1 TO RowMax DO
        FOR Col := 1 TO ColMax DO
            Grid[Row, Col] := FALSE
        END;
    END;
END InitGrid;

PROCEDURE Perform(VAR Grid       : GridType;
                  Action         : ActionType;
                  R1, C1, R2, C2 : CARDINAL);
VAR
    Row, Col             : CARDINAL;
BEGIN
    FOR Row := R1 TO R2 DO
        FOR Col := C1 TO C2 DO
            CASE Action OF
                TurnOn  : Grid[Row, Col] := TRUE |
                TurnOff : Grid[Row, Col] := FALSE |
                Toggle  : Grid[Row, Col] := NOT Grid[Row, Col]
            END;
        END;
    END;
END Perform;

PROCEDURE CountGrid(Grid : GridType) : CARDINAL;
VAR
    Row, Col             : CARDINAL;
    Total                : CARDINAL;
BEGIN
    Total := 0;
    FOR Row := 1 TO RowMax DO
        FOR Col := 1 TO ColMax DO
            IF Grid[Row, Col] THEN
                Total := Total + 1;
            END;
        END;
    END;
    RETURN Total
END CountGrid;

PROCEDURE Process(FileName : String) : CARDINAL;
VAR
    InFile                : File;
    Count                 : CARDINAL;
    Line                  : String;
    Grid                  : GridType;
    Action                : ActionType;
    OnStr, OffStr, TogStr : String;
    CoordIdx              : CARDINAL;
    Coords                : String;
    ThroughIdx            : CARDINAL;
    Coord1, Coord2        : String;
    CommaIdx              : CARDINAL;
    Tok                   : String;
    Found                 : BOOLEAN;
    R1, C1, R2, C2        : CARDINAL;

BEGIN
    InitGrid(Grid);
    OnStr := InitString('turn on');
    OffStr := InitString('turn off');
    TogStr := InitString('toggle');
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
        IF Equal(Slice(Line, 0, 7), OnStr) THEN
            Action := TurnOn;
            CoordIdx := 8;
        ELSIF Equal(Slice(Line, 0, 8), OffStr) THEN
            Action := TurnOff;
            CoordIdx := 9;
        ELSIF Equal(Slice(Line, 0, 6), TogStr) THEN
            Action := Toggle;
            CoordIdx := 7;
        ELSE
            WriteString('error: malformed input line: ');
            WriteDynString(Line);
            WriteLn;
            exit(1);
        END;
        Coords := Slice(Line, CoordIdx, 0);
        ThroughIdx := Index(Coords, ' ', 0);
        Coord1 := Slice(Coords, 0, ThroughIdx);
        CommaIdx := Index(Coord1,',', 0);
        Tok := Slice(Coord1, 0, CommaIdx);
        R1 := StringToCardinal(Tok, 10, Found) + 1;
        Tok := Slice(Coord1, CommaIdx + 1, 0);
        C1 := StringToCardinal(Tok, 10, Found) + 1;
        Coord2 := Slice(Coords, ThroughIdx + 9, 0);
        CommaIdx := Index(Coord2, ',', 0);
        Tok := Slice(Coord2, 0, CommaIdx);
        R2 := StringToCardinal(Tok, 10, Found) + 1;
        Tok := Slice(Coord2, CommaIdx + 1, 0);
        C2 := StringToCardinal(Tok, 10, Found) + 1;
        Perform(Grid, Action, R1, C1, R2, C2);
    END;
    Close(InFile);
    RETURN CountGrid(Grid);
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
END day06a.

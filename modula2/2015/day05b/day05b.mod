MODULE day05b;

FROM DynamicStrings   IMPORT String, Equal, InitString, Length, Slice, char;
FROM FIO              IMPORT File, Close, EOF, IsNoError;
FROM InOut            IMPORT WriteCard, WriteLn, WriteString;
FROM libc             IMPORT exit;
FROM SFIO             IMPORT OpenToRead;
FROM StringConvert    IMPORT StringToCardinal;

FROM Args             IMPORT ArgCount, GetArgument;
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

PROCEDURE Prop1(S : String) : BOOLEAN;
VAR
    I, J                 : CARDINAL;
    A, B, C, D           : CHAR;
BEGIN
    FOR I := 0 TO (Length(S) - 4) DO
        A := char(S, I);
        B := char(S, I + 1);
        FOR J := I + 2 TO (Length(S) - 2) DO
            C := char(S, J);
            D := char(S, J + 1);
            IF (A = C) AND (B = D) THEN
                RETURN TRUE;
            END;
        END;
    END;
    RETURN FALSE;
END Prop1;

PROCEDURE Prop2(S : String) : BOOLEAN;
VAR
    I                    : CARDINAL;
BEGIN
    FOR I := 0 TO (Length(S) - 3) DO
        IF char(S, I) = char(S, I + 2) THEN
            RETURN TRUE;
        END;
    END;
    RETURN FALSE;
END Prop2;

PROCEDURE Process(FileName : String) : CARDINAL;
VAR
    InFile               : File;
    Count                : CARDINAL;
    Line                 : String;

BEGIN
    Count := 0;
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
        IF Prop1(Line) AND Prop2(Line) THEN
            INC(Count);
        END;
    END;
    Close(InFile);
    RETURN Count;
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
END day05b.

MODULE day01a;

FROM DynamicStrings  IMPORT String;
FROM FIO             IMPORT File, Close, EOF, IsNoError, ReadChar;
FROM InOut           IMPORT WriteCard, WriteLn, WriteString;
FROM libc            IMPORT exit;
FROM SFIO            IMPORT OpenToRead;

FROM Args            IMPORT ArgCount, GetArgument;
FROM DynamicStringIO IMPORT WriteDynString;

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
    InFile        : File;
    Floors        : INTEGER;
    Pos           : CARDINAL;
    Ch            : CHAR;

BEGIN
    Floors := 0;
    Pos := 0;
    InFile := OpenToRead(FileName);
    IF NOT IsNoError(InFile) THEN
        WriteString('error: unable to open input file: ');
        WriteDynString(FileName);
        WriteLn;
        exit(1);
    END;
    LOOP
        Ch := ReadChar(InFile);
        INC(Pos);
        CASE Ch OF
            '(': INC(Floors); |
            ')': DEC(Floors);
        END;
        IF Floors < 0 THEN
            EXIT;
        END;
        IF EOF(InFile) THEN
            Pos := 0;
            EXIT;
        END;
    END;
    Close(InFile);
    RETURN Pos;
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

END day01a.

MODULE day01a;

FROM DynamicStrings IMPORT String;
FROM FIO            IMPORT File, Close, EOF, IsNoError, ReadChar;
FROM InOut          IMPORT WriteInt, WriteLn, WriteS, WriteString;
FROM libc           IMPORT exit;
FROM SFIO           IMPORT OpenToRead;

FROM Args           IMPORT ArgCount, GetArgument;

VAR
    Argc          : CARDINAL;
    ProgName      : String;
    FileName      : String;
    Result        : INTEGER;

PROCEDURE Usage(ProgName : String);
BEGIN
    WriteString('usage: ');
    ProgName := WriteS(ProgName);
    WriteString(' <input file>');
    WriteLn;
    exit(1);
END Usage;

PROCEDURE Process(FileName : String) : INTEGER;
VAR
    InFile        : File;
    Floors        : INTEGER;
    Ch            : CHAR;

BEGIN
    Floors := 0;
    InFile := OpenToRead(FileName);
    IF NOT IsNoError(InFile) THEN
        WriteString('error: unable to open input file: ');
        FileName := WriteS(FileName);
        WriteLn;
        exit(1);
    END;
    REPEAT
        Ch := ReadChar(InFile);
        CASE Ch OF
            '(': INC(Floors); |
            ')': DEC(Floors);
        END;
    UNTIL EOF(InFile);
    Close(InFile);
    RETURN Floors;
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
    WriteInt(Result, 1);
    WriteLn;

END day01a.

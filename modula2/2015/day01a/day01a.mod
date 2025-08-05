MODULE day01a;

FROM FIO      IMPORT Close, EOF, File, IsNoError, OpenToRead, ReadChar;
FROM InOut    IMPORT WriteInt, WriteLn, WriteString;
FROM libc     IMPORT exit;
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

PROCEDURE Process(FileName : Arg) : INTEGER;
VAR
    InFile        : File;
    Floors        : INTEGER;
    Ch            : CHAR;

BEGIN
    Floors := 0;
    InFile := OpenToRead(FileName);
    IF NOT IsNoError(InFile) THEN
        WriteString('error: unable to open input file: ');
        WriteString(FileName);
        exit(1);
    END;
    REPEAT
        Ch := ReadChar(InFile);
        CASE Ch OF
            '(': Floors := Floors + 1; |
            ')': Floors := Floors - 1;
        END;
    UNTIL EOF(InFile);
    Close(InFile);
    RETURN Floors;
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
    WriteInt(Result, 1);
    WriteLn;

END day01a.

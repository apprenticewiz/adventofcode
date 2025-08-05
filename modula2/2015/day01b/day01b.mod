MODULE day01b;

FROM FIO      IMPORT Close, EOF, File, IsNoError, OpenToRead, ReadChar;
FROM InOut    IMPORT WriteCard, WriteLn, WriteString;
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
    Result        : CARDINAL;

PROCEDURE Usage(ProgName : Arg);
BEGIN
    WriteString('usage: ');
    WriteString(ProgName);
    WriteString(' <input file>');
    WriteLn;
    exit(1);
END Usage;

PROCEDURE Process(FileName : Arg) : CARDINAL;
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
        WriteString(FileName);
        exit(1);
    END;
    LOOP
        Ch := ReadChar(InFile);
        Pos := Pos + 1;
        CASE Ch OF
            '(': Floors := Floors + 1; |
            ')': Floors := Floors - 1;
        END;
        IF Floors < 0 THEN
            EXIT;
        ELSIF EOF(InFile) THEN
            Pos := 0;
            EXIT;
        END;
    END;
    Close(InFile);
    RETURN Pos;
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

END day01b.

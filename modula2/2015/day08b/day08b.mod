MODULE day08b;

FROM DynamicStrings   IMPORT String, Length, char;
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

PROCEDURE Process(FileName : String) : CARDINAL;
VAR
    InFile               : File;
    Line                 : String;
    Result               : CARDINAL;
    CodeLen, EncLen      : CARDINAL;
    I                    : CARDINAL;
    Ch                   : CHAR;

BEGIN
    Result := 0;
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
        CodeLen := Length(Line);
        EncLen := 0;
        FOR I := 0 TO CodeLen - 1 DO
            Ch := char(Line, I);
            CASE Ch OF
              '\' : EncLen := EncLen + 2;
            | '"' : EncLen := EncLen + 2;
            ELSE EncLen := EncLen + 1;
            END;
        END;
        Result := Result + 2 + (EncLen - CodeLen);
    END;
    Close(InFile);
    RETURN Result;
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
END day08b.

PROGRAM Day08a(Input, Output);

VAR
    FileName : PChar;
    Res      : Integer;

PROCEDURE Usage();
VAR
    ProgName : PChar;
BEGIN
    ProgName := argv[0];
    WriteLn('usage: ', ProgName, ' <input file>');
    Halt(1)
END;

FUNCTION Process(FileName : PChar) : Integer;
VAR
    InFile          : Text;
    Res             : Integer;
    LineStr         : String;
    CodeLen, MemLen : Integer;
    I               : Integer;

BEGIN
    Res := 0;
    {$I-}
    Assign(InFile, FileName);
    Reset(InFile);
    IF IOResult <> 0 THEN
    BEGIN
        WriteLn(StdErr, 'error when opening file: ', FileName);
        Halt(1)
    END;
    {$I+}
    WHILE NOT EOF(InFile) DO
    BEGIN
        ReadLn(InFile, LineStr);
        CodeLen := Length(LineStr);
        MemLen := 0;
        I := 2;
        WHILE ( I < Length(LineStr) ) DO
        BEGIN
            IF LineStr[I] = '\' THEN
                CASE LineStr[I + 1] OF
                    '\', '"' : I := I + 2;
                    'x'      : I := I + 4;
                ELSE
                    I := I + 1;
                END
            ELSE
                I := I + 1;
            MemLen := MemLen + 1
        END;
        Res := Res + (CodeLen - MemLen)
    END;
    Close(InFile);
    Process := Res
END;

BEGIN
    IF argc < 2 THEN
        Usage();
    FileName := argv[1];
    Res := Process(FileName);
    WriteLn('result = ', Res)
END.

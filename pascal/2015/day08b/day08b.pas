PROGRAM Day08b(Input, Output);

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
    CodeLen, EncLen : Integer;
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
        EncLen := 0;
        FOR I := 1 TO Length(LineStr) DO
            CASE LineStr[I] OF
                '\', '"' : EncLen := EncLen + 2;
            ELSE
                EncLen := EncLen + 1;
            END;
        Res := Res + 2 + (EncLen - CodeLen)
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

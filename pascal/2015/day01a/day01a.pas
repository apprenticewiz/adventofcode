PROGRAM Day01a(Input, Output);

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
    InFile  : Text;
    Ch      : Char;
    Floor   : Integer;

BEGIN
    Floor := 0;
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
        Read(InFile, Ch);
        CASE Ch OF
            '(' : Floor := Floor + 1;
            ')' : Floor := Floor - 1;
        END;
    END;
    Close(InFile);
    Process := Floor
END;

BEGIN
    IF argc < 2 THEN
        Usage();
    FileName := argv[1];
    Res := Process(FileName);
    WriteLn('result = ', Res)
END.

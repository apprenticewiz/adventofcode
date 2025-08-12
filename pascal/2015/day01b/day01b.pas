PROGRAM Day01b(Input, Output);

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
    InFile   : Text;
    Ch       : Char;
    Floor, P : Integer;
    Found    : Boolean;

BEGIN
    Floor := 0;
    P := 0;
    Found := False;
    {$I-}
    Assign(InFile, FileName);
    Reset(InFile);
    IF IOResult <> 0 THEN
    BEGIN
        WriteLn(StdErr, 'error when opening file: ', FileName);
        Halt(1)
    END;
    {$I+}
    WHILE NOT EOF(InFile) AND NOT Found DO
    BEGIN
        Read(InFile, Ch);
        P := P + 1;
        CASE Ch OF
            '(' : Floor := Floor + 1;
            ')' : Floor := Floor - 1;
        END;
        IF Floor < 0 THEN
            Found := True
    END;
    Close(InFile);
    IF Found THEN
        Process := P
    ELSE
        Process := 0
END;

BEGIN
    IF argc < 2 THEN
        Usage();
    FileName := argv[1];
    Res := Process(FileName);
    WriteLn('result = ', Res)
END.

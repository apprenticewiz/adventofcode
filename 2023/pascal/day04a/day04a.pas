PROGRAM Day04a(INPUT, OUTPUT);
{$H+}

USES StrUtils, SysUtils;

TYPE
    ValueTypes = (Int, Bool);

    ValueType = RECORD
        CASE ValType : ValueTypes OF
            Int  : (IntVal  : INTEGER);
            Bool : (BoolVal : BOOLEAN); 
    END;

    Node = RECORD
        Key   : INTEGER;
        Value : ValueType;
        Next  : ^Node;
    END;

    NodePtr = ^Node;

    ResultStatus = (Success, Error);

    ResultType = RECORD
        Status : ResultStatus;
        Value  : ValueType;
    END;

VAR
    FileName : STRING;
    Res      : INT32;

PROCEDURE AppendList(VAR List : NodePtr; Key : INTEGER; Value : ValueType);
VAR
    Curr, Temp : NodePtr;
BEGIN
    New(Temp);
    Temp^.Key := Key;
    Temp^.Value := Value;
    Temp^.Next := NIL;
    Curr := List;
    IF List = NIL THEN
        List := Temp
    ELSE
    BEGIN
        WHILE Curr^.Next <> NIL DO
            Curr := Curr^.Next;
        Curr^.Next := Temp
    END
END;

FUNCTION SearchList(List : NodePtr; Key : INTEGER) : ResultType;
VAR
    Curr : NodePtr;
    Res  : ResultType;
BEGIN
    Curr := List;
    WHILE Curr <> NIL DO
    BEGIN
        IF Curr^.Key = Key THEN
        BEGIN
            Res.Status := Success;
            Res.Value := Curr^.Value;
            BREAK
        END
        ELSE
            Curr := Curr^.Next
    END;
    IF Curr = NIL THEN
    BEGIN
        Res.Status := Error;
        Res.Value.ValType := Bool;
        Res.Value.BoolVal := FALSE;
    END;
    SearchList := Res
END;

PROCEDURE FreeList(List : NodePtr);
VAR
    Curr, Temp : NodePtr;
BEGIN
    Curr := List;
    WHILE Curr <> NIL DO
    BEGIN
        Temp := Curr;
        Curr := Curr^.Next;
        Dispose(Temp)
    END
END;

PROCEDURE Usage;
BEGIN
    WriteLn('usage: ', ExtractFileName(paramStr(0)), ' <file>');
    Halt(1)
END;

FUNCTION Process(FileName : STRING) : INTEGER;
VAR
    InFile                            : TEXT;
    Line                              : STRING;
    Res                               : INTEGER = 0;
    WinningSet, HandSet, CurrPtr      : NodePtr;
    ColonPos, PipePos                 : INTEGER;
    CurrPos                           : LONGINT;
    Rest, WinningStr, HandStr, NumStr : STRING;
    Num                               : INTEGER;
    Value                             : ValueType;
    SearchResult                      : ResultType;
    CommonCount                       : INTEGER;
BEGIN
    Assign(InFile, FileName);
    Reset(InFile);
    WHILE NOT Eof(InFile) DO
    BEGIN
        ReadLn(InFile, Line);
        ColonPos := NPos(': ', Line, 1);
        IF ColonPos > 0 THEN
        BEGIN
            Rest := MidStr(Line, ColonPos + 2, Length(Line) - ColonPos - 1);
            PipePos := NPos(' | ', Rest, 1);
            IF PipePos > 0 THEN
            BEGIN
                WinningStr := MidStr(Rest, 1, PipePos - 1);
                WinningSet := NIL;
                CurrPos := 1;
                WHILE CurrPos <= Length(WinningStr) DO
                BEGIN
                    NumStr := ExtractSubstr(WinningStr, CurrPos, [' ']);
                    IF NumStr <> '' THEN
                    BEGIN
                        Num := StrToInt(NumStr);
                        Value.ValType := Bool;
                        Value.BoolVal := TRUE;
                        AppendList(WinningSet, Num, Value)
                    END
                END;
                HandStr := MidStr(Rest, PipePos + 3, Length(Rest) - PipePos - 2);
                HandSet := NIL;
                CurrPos := 1;
                WHILE CurrPos <= Length(HandStr) DO
                BEGIN
                    NumStr := ExtractSubstr(HandStr, CurrPos, [' ']);
                    IF NumStr <> '' THEN
                    BEGIN
                        Num := StrToInt(NumStr);
                        Value.ValType := Bool;
                        Value.BoolVal := TRUE;
                        AppendList(HandSet, Num, Value)
                    END
                END;
                CurrPtr := WinningSet;
                CommonCount := 0;
                WHILE CurrPtr <> NIL DO
                BEGIN
                    SearchResult := SearchList(HandSet, CurrPtr^.Key);
                    IF SearchResult.Status = Success THEN
                        CommonCount := CommonCount + 1;
                    CurrPtr := CurrPtr^.Next
                END;
                IF CommonCount > 0 THEN
                    Res := Res + 1 SHL (CommonCount - 1)
            END
        END
    END;
    Close(InFile);
    FreeList(WinningSet);
    FreeList(HandSet);
    Process := Res;
END;

BEGIN
    IF paramCount() < 1 THEN
        Usage;
    FileName := paramStr(1);
    Res := Process(FileName);
    WriteLn('result = ', Res)
END.

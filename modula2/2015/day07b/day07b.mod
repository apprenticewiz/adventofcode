MODULE day07b;

FROM DynamicStrings      IMPORT String, CopyOut;
FROM FIO                 IMPORT File, Close, EOF, IsNoError;
FROM InOut               IMPORT WriteCard, WriteLn, WriteString;
FROM libc                IMPORT exit;
FROM SFIO                IMPORT OpenToRead;
FROM StrExtras           IMPORT CardToStr, StrCompare, StrIndex, StrSlice, StrToCard;
FROM StrLib              IMPORT IsSubString, StrCopy, StrLen;
FROM SYSTEM              IMPORT SHIFT;

FROM Args                IMPORT ArgCount, GetArgument;
FROM DynamicStringFIO    IMPORT ReadDynString;
FROM DynamicStringIO     IMPORT WriteDynString;

CONST
    ItemMax = 1000;

TYPE
    Operators = (AssignOp, NotOp, AndOp, OrOp, LeftShiftOp, RightShiftOp);

    Operation = RECORD
        Source1 : ARRAY [0..31] OF CHAR;
        CASE Operator : Operators OF
            AndOp, OrOp:
              Source2 : ARRAY [0..31] OF CHAR;
          | LeftShiftOp, RightShiftOp:
              Amount : CARDINAL;
        END;
    END;

    OperationTableEntry = RECORD
        Key   : ARRAY [0..31] OF CHAR;
        Value : Operation;
    END;

    OperationTable = RECORD
        Entries   : ARRAY [1..ItemMax] OF OperationTableEntry;
        LastIndex : CARDINAL;
    END;

    CacheTableEntry = RECORD
        Key   : ARRAY [0..31] OF CHAR;
        Value : CARDINAL;
    END;

    CacheTable = RECORD
        Entries   : ARRAY [1..ItemMax] OF CacheTableEntry;
        LastIndex : CARDINAL;
    END;

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

PROCEDURE GetOp(VAR Ops   : OperationTable;
                Key       : ARRAY OF CHAR;
                VAR Value : Operation;
                VAR Found : BOOLEAN);
VAR
    I  : CARDINAL;

BEGIN
    I := 1;
    LOOP
        IF I = Ops.LastIndex THEN
            Found := FALSE;
            EXIT;
        ELSIF StrCompare(Ops.Entries[I].Key, Key) = 0 THEN
            Value := Ops.Entries[I].Value;
            Found := TRUE;
            EXIT;
        ELSE
            INC(I);
        END;
    END;
END GetOp;

PROCEDURE GetCache(VAR Cache : CacheTable;
                   Key       : ARRAY OF CHAR;
                   VAR Value : CARDINAL;
                   VAR Found : BOOLEAN);
VAR
    I  : CARDINAL;

BEGIN
    I := 1;
    LOOP
        IF I = Cache.LastIndex THEN
            Found := FALSE;
            EXIT;
        ELSIF StrCompare(Cache.Entries[I].Key, Key) = 0 THEN
            Value := Cache.Entries[I].Value;
            Found := TRUE;
            EXIT;
        ELSE
            INC(I);
        END;
    END;
END GetCache;

PROCEDURE PutCache(VAR Cache : CacheTable;
                   Key       : ARRAY OF CHAR;
                   Value     : CARDINAL);
VAR
    Found    : BOOLEAN;
    OldValue : CARDINAL;
    I        : CARDINAL;

BEGIN
    GetCache(Cache, Key, OldValue, Found);
    IF Found THEN
        FOR I := 1 TO Cache.LastIndex - 1 DO
            IF StrCompare(Cache.Entries[I].Key, Key) = 0 THEN
                Cache.Entries[I].Value := Value;
            END;
        END;
    ELSE
        StrCopy(Key, Cache.Entries[Cache.LastIndex].Key);
        Cache.Entries[Cache.LastIndex].Value := Value;
        INC(Cache.LastIndex);
    END;
END PutCache;

PROCEDURE Eval(VAR Ops   : OperationTable;
               VAR Cache : CacheTable;
               Expr      : ARRAY OF CHAR) : CARDINAL;
VAR
    Success, Found   : BOOLEAN;
    Result1, Result2 : CARDINAL;
    Op               : Operation;
    Work             : BITSET;

BEGIN
    StrToCard(Expr, Result1, Success);
    IF Success THEN
        RETURN Result1;
    END;
    GetCache(Cache, Expr, Result1, Found);
    IF Found THEN
        RETURN Result1;
    END;
    GetOp(Ops, Expr, Op, Found);
    CASE Op.Operator OF
        AssignOp:
            Result1 := Eval(Ops, Cache, Op.Source1);
            PutCache(Cache, Expr, Result1);
      | NotOp:
            Result1 := Eval(Ops, Cache, Op.Source1);
            Work := VAL(BITSET, Result1);
            Work := VAL(BITSET, 65535) - Work;
            Work := Work * VAL(BITSET, 65535);
            Result1 := VAL(CARDINAL, Work);
            PutCache(Cache, Expr, Result1);
      | AndOp:
            Result1 := Eval(Ops, Cache, Op.Source1);
            Result2 := Eval(Ops, Cache, Op.Source2);
            Work := VAL(BITSET, Result1);
            Work := Work * VAL(BITSET, Result2);
            Work := Work * VAL(BITSET, 65535);
            Result1 := VAL(CARDINAL, Work);
            PutCache(Cache, Expr, Result1);
      | OrOp:
            Result1 := Eval(Ops, Cache, Op.Source1);
            Result2 := Eval(Ops, Cache, Op.Source2);
            Work := VAL(BITSET, Result1);
            Work := Work + VAL(BITSET, Result2);
            Work := Work * VAL(BITSET, 65535);
            Result1 := VAL(CARDINAL, Work);
            PutCache(Cache, Expr, Result1);
      | LeftShiftOp:
            Result1 := Eval(Ops, Cache, Op.Source1);
            Work := VAL(BITSET, Result1);
            Work := SHIFT(Work, VAL(INTEGER, Op.Amount));
            Work := Work * VAL(BITSET, 65535);
            Result1 := VAL(CARDINAL, Work);
            PutCache(Cache, Expr, Result1);
      | RightShiftOp:
            Result1 := Eval(Ops, Cache, Op.Source1);
            Work := VAL(BITSET, Result1);
            Work := SHIFT(Work, -VAL(INTEGER, Op.Amount));
            Work := Work * VAL(BITSET, 65535);
            Result1 := VAL(CARDINAL, Work);
            PutCache(Cache, Expr, Result1);
    END;
    RETURN Result1;
END Eval;

PROCEDURE Process(FileName : String) : CARDINAL;
VAR
    InFile           : File;
    Line             : String;
    Operations       : OperationTable;
    Cache            : CacheTable;
    LineBuf          : ARRAY [0..31] OF CHAR;
    OpEntry          : OperationTableEntry;
    Found, Success   : BOOLEAN;
    ArrowIdx, OpIdx  : CARDINAL;
    Src1, Src2, Dest : ARRAY [0..31] OF CHAR;
    Amount           : CARDINAL;
    A                : CARDINAL;
    I                : CARDINAL;
    NumStr           : ARRAY [0..31] OF CHAR;

BEGIN
    Operations.LastIndex := 1;
    Cache.LastIndex := 1;
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
        CopyOut(LineBuf, Line);
        ArrowIdx := StrIndex(LineBuf, ' -> ', Found);
        StrSlice(LineBuf, Dest, ArrowIdx + 4, 0);
        IF IsSubString(LineBuf, 'NOT ') THEN
            StrSlice(LineBuf, Src1, 4, ArrowIdx - 4);
            OpEntry.Key := Dest;
            OpEntry.Value.Operator := NotOp;
            OpEntry.Value.Source1 := Src1;
            Operations.Entries[Operations.LastIndex] := OpEntry;
            INC(Operations.LastIndex);
        ELSIF IsSubString(LineBuf, ' AND ') THEN
            OpIdx := StrIndex(LineBuf, ' AND ', Found);
            StrSlice(LineBuf, Src1, 0, OpIdx);
            StrSlice(LineBuf, Src2, OpIdx + 5, ArrowIdx - (OpIdx + 5));
            OpEntry.Key := Dest;
            OpEntry.Value.Operator := AndOp;
            OpEntry.Value.Source1 := Src1;
            OpEntry.Value.Source2 := Src2;
            Operations.Entries[Operations.LastIndex] := OpEntry;
            INC(Operations.LastIndex);
        ELSIF IsSubString(LineBuf, ' OR ') THEN
            OpIdx := StrIndex(LineBuf, ' OR ', Found);
            StrSlice(LineBuf, Src1, 0, OpIdx);
            StrSlice(LineBuf, Src2, OpIdx + 4, ArrowIdx - (OpIdx + 4));
            OpEntry.Key := Dest;
            OpEntry.Value.Operator := OrOp;
            OpEntry.Value.Source1 := Src1;
            OpEntry.Value.Source2 := Src2;
            Operations.Entries[Operations.LastIndex] := OpEntry;
            INC(Operations.LastIndex);
        ELSIF IsSubString(LineBuf, ' LSHIFT ') THEN
            OpIdx := StrIndex(LineBuf, ' LSHIFT ', Found);
            StrSlice(LineBuf, Src1, 0, OpIdx);
            StrSlice(LineBuf, Src2, OpIdx + 8, ArrowIdx - (OpIdx + 8));
            OpEntry.Key := Dest;
            OpEntry.Value.Operator := LeftShiftOp;
            OpEntry.Value.Source1 := Src1;
            StrToCard(Src2, Amount, Success);
            OpEntry.Value.Amount := Amount;
            Operations.Entries[Operations.LastIndex] := OpEntry;
            INC(Operations.LastIndex);
        ELSIF IsSubString(LineBuf, ' RSHIFT ') THEN
            OpIdx := StrIndex(LineBuf, ' RSHIFT ', Found);
            StrSlice(LineBuf, Src1, 0, OpIdx);
            StrSlice(LineBuf, Src2, OpIdx + 8, ArrowIdx - (OpIdx + 8));
            OpEntry.Key := Dest;
            OpEntry.Value.Operator := RightShiftOp;
            OpEntry.Value.Source1 := Src1;
            StrToCard(Src2, Amount, Success);
            OpEntry.Value.Amount := Amount;
            Operations.Entries[Operations.LastIndex] := OpEntry;
            INC(Operations.LastIndex);
        ELSE
            StrSlice(LineBuf, Src1, 0, ArrowIdx);
            OpEntry.Key := Dest;
            OpEntry.Value.Operator := AssignOp;
            OpEntry.Value.Source1 := Src1;
            Operations.Entries[Operations.LastIndex] := OpEntry;
            INC(Operations.LastIndex);
        END;
    END;
    Close(InFile);
    A := Eval(Operations, Cache, 'a');
    I := 1;
    LOOP
        IF I = Operations.LastIndex THEN
            EXIT;
        ELSIF StrCompare(Operations.Entries[I].Key, 'b') = 0 THEN
            Operations.Entries[I].Value.Operator := AssignOp;
            CardToStr(A, NumStr);
            StrCopy(NumStr, Operations.Entries[I].Value.Source1);
            EXIT;
        ELSE
            INC(I);
        END;
    END;
    Cache.LastIndex := 1;
    RETURN Eval(Operations, Cache, 'a');
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
END day07b.

TYPE Operation
    ENUM OperatorType
        ASSIGN_OP
        NOT_OP
        AND_OP
        OR_OP
        LSHIFT_OP
        RSHIFT_OP
    END ENUM

    Op AS OperatorType
    Source1 AS String
    Source2 AS String
    Amount AS Integer
END TYPE

TYPE OperationTableEntry
    Key AS String
    Value AS Operation
    LeftChild AS Integer
    RightChild AS Integer
END TYPE

TYPE OperationTable
    Entries(0 TO 999) AS OperationTableEntry
    Root AS Integer
    NextFree AS Integer
END TYPE

TYPE CacheTableEntry
    Key AS String
    Value AS Integer
    LeftChild AS Integer
    RightChild AS Integer
END TYPE

TYPE CacheTable
    Entries(0 TO 999) AS CacheTableEntry
    Root AS Integer
    NextFree AS Integer
END TYPE

SUB Usage(ProgName AS ZString)
    PRINT "usage: "; ProgName; " <input file>"
    System(1)
END SUB

FUNCTION NewOpNode(BYREF Ops AS OperationTable, Key AS String, Value AS Operation) AS Integer
    DIM I AS Integer

    IF Ops.NextFree > 999 THEN
        RETURN -1
    END IF
    I = Ops.NextFree
    Ops.Entries(I).Key = Key
    Ops.Entries(I).Value = Value
    Ops.Entries(I).LeftChild = -1
    Ops.Entries(I).RightChild = -1
    Ops.NextFree += 1
    RETURN I
END FUNCTION

SUB InsertOpNode(BYREF Ops AS OperationTable, BYREF CurrRoot AS Integer, Key AS String, Value AS Operation)
    IF CurrRoot = -1 THEN
        CurrRoot = NewOpNode(Ops, Key, Value)
        IF Ops.Root = -1 THEN
            Ops.Root = CurrRoot
        END IF
    ELSE
        IF Key < Ops.Entries(CurrRoot).Key THEN
            InsertOpNode(Ops, Ops.Entries(CurrRoot).LeftChild, Key, Value)
        ELSEIF Key > Ops.Entries(CurrRoot).Key THEN
            InsertOpNode(Ops, Ops.Entries(CurrRoot).RightChild, Key, Value)
        ELSE
            Ops.Entries(CurrRoot).Value = Value
        END IF
    END IF
END SUB

FUNCTION OpsFindKey(BYREF Ops AS OperationTable, CurrRoot AS Integer, Key AS String) AS Integer
    DIM LeftIdx AS INTEGER
    DIM RightIdx AS INTEGER

    IF CurrRoot = -1 THEN
        RETURN -1
    ELSEIF Ops.Entries(CurrRoot).Key = Key THEN
        RETURN CurrRoot
    ELSE
        LeftIdx = OpsFindKey(Ops, Ops.Entries(CurrRoot).LeftChild, Key)
        IF LeftIdx <> -1 THEN
            RETURN LeftIdx
        END IF
        RightIdx = OpsFindKey(Ops, Ops.Entries(CurrRoot).RightChild, Key)
        IF RightIdx <> -1 THEN
            RETURN RightIdx
        END IF
        RETURN -1
    END IF
END FUNCTION

FUNCTION AllDigits(S AS String) AS Boolean
    FOR I AS Integer = 1 TO LEN(S)
        IF ASC(MID(S, I, 1)) < ASC("0") OR ASC(MID(S, I, 1)) > ASC("9") THEN
            RETURN FALSE
        END IF
    NEXT
    RETURN TRUE
END FUNCTION

FUNCTION NewCacheNode(BYREF Cache AS CacheTable, Key AS String, Value AS Integer) AS Integer
    DIM I AS Integer

    IF Cache.NextFree > 999 THEN
        RETURN -1
    END IF
    I = Cache.NextFree
    Cache.Entries(I).Key = Key
    Cache.Entries(I).Value = Value
    Cache.Entries(I).LeftChild = -1
    Cache.Entries(I).RightChild = -1
    Cache.NextFree += 1
    RETURN I
END FUNCTION

SUB InsertCacheNode(BYREF Cache AS CacheTable, BYREF CurrRoot AS Integer, Key AS String, Value AS Integer)
    IF CurrRoot = -1 THEN
        CurrRoot = NewCacheNode(Cache, Key, Value)
        IF Cache.Root = -1 THEN
            Cache.Root = CurrRoot
        END IF
    ELSE
        IF Key < Cache.Entries(CurrRoot).Key THEN
            InsertCacheNode(Cache, Cache.Entries(CurrRoot).LeftChild, Key, Value)
        ELSEIF Key > Cache.Entries(CurrRoot).Key THEN
            InsertCacheNode(Cache, Cache.Entries(CurrRoot).RightChild, Key, Value)
        ELSE
            Cache.Entries(CurrRoot).Value = Value
        END IF
    END IF
END SUB

FUNCTION CacheContainsKey(BYREF Cache AS CacheTable, CurrRoot AS Integer, Key AS String) AS Boolean
    IF CurrRoot = -1 THEN
        RETURN FALSE
    ELSEIF Cache.Entries(CurrRoot).Key = Key THEN
        RETURN TRUE
    ELSE
        RETURN CacheContainsKey(Cache, Cache.Entries(CurrRoot).LeftChild, Key) OR _
               CacheContainsKey(Cache, Cache.Entries(CurrRoot).RightChild, Key)
    END IF
END FUNCTION

FUNCTION CacheFindKey(BYREF Cache AS CacheTable, CurrRoot AS Integer, Key AS String) AS Integer
    DIM LeftIdx AS INTEGER
    DIM RightIdx AS INTEGER

    IF CurrRoot = -1 THEN
        RETURN -1
    ELSEIF Cache.Entries(CurrRoot).Key = Key THEN
        RETURN CurrRoot
    ELSE
        LeftIdx = CacheFindKey(Cache, Cache.Entries(CurrRoot).LeftChild, Key)
        IF LeftIdx <> -1 THEN
            RETURN LeftIdx
        END IF
        RightIdx = CacheFindKey(Cache, Cache.Entries(CurrRoot).RightChild, Key)
        IF RightIdx <> -1 THEN
            RETURN RightIdx
        END IF
        RETURN -1
    END IF
END FUNCTION

FUNCTION Eval(BYREF Ops AS OperationTable, BYREF Cache AS CacheTable, Expr AS String) AS Integer
    DIM I AS Integer
    DIM Val1 AS Integer
    DIM Val2 AS Integer
    DIM Result AS Integer

    IF AllDigits(Expr) THEN
        RETURN VALINT(Expr)
    ELSEIF CacheContainsKey(Cache, Cache.Root, Expr) THEN
        I = CacheFindKey(Cache, Cache.Root, Expr)
        RETURN Cache.Entries(I).Value
    ELSE
        I = OpsFindKey(Ops, Ops.Root, Expr)
        SELECT CASE Ops.Entries(I).Value.Op
            CASE Operation.OperatorType.ASSIGN_OP
                Result = Eval(Ops, Cache, Ops.Entries(I).Value.Source1)
                InsertCacheNode(Cache, Cache.Root, Expr, Result)
                RETURN Result
            CASE Operation.OperatorType.NOT_OP
                Result = (NOT Eval(Ops, Cache, Ops.Entries(I).Value.Source1)) AND &Hffff
                InsertCacheNode(Cache, Cache.Root, Expr, Result)
                RETURN Result
            CASE Operation.OperatorType.AND_OP
                Val1 = Eval(Ops, Cache, Ops.Entries(I).Value.Source1)
                Val2 = Eval(Ops, Cache, Ops.Entries(I).Value.Source2)
                Result = (Val1 AND Val2) AND &Hffff
                InsertCacheNode(Cache, Cache.Root, Expr, Result)
                RETURN Result
            CASE Operation.OperatorType.OR_OP
                Val1 = Eval(Ops, Cache, Ops.Entries(I).Value.Source1)
                Val2 = Eval(Ops, Cache, Ops.Entries(I).Value.Source2)
                Result = (Val1 OR Val2) AND &Hffff
                InsertCacheNode(Cache, Cache.Root, Expr, Result)
                RETURN Result
            CASE Operation.OperatorType.LSHIFT_OP
                Val1 = Eval(Ops, Cache, Ops.Entries(I).Value.Source1)
                Val2 = Ops.Entries(I).Value.Amount
                Result = (Val1 SHL Val2) AND &Hffff
                InsertCacheNode(Cache, Cache.Root, Expr, Result)
                RETURN Result
            CASE Operation.OperatorType.RSHIFT_OP
                Val1 = Eval(Ops, Cache, Ops.Entries(I).Value.Source1)
                Val2 = Ops.Entries(I).Value.Amount
                Result = (Val1 SHR Val2) AND &Hffff
                InsertCacheNode(Cache, Cache.Root, Expr, Result)
                RETURN Result
        END SELECT
    END IF
END FUNCTION

FUNCTION Process(FileName AS String) AS Integer
    DIM InFile AS Integer
    DIM LineStr AS String
    DIM Operations AS OperationTable
    DIM Cache AS CacheTable
    DIM Op AS Operation
    DIM Src1 AS String
    DIM Src2 AS String
    DIM Dest AS String
    DIM Amount AS Integer
    DIM ArrowIdx AS Integer
    DIM OpIdx AS Integer
    DIM A AS Integer

    Operations.NextFree = 0
    Operations.Root = -1
    Cache.NextFree = 0
    Cache.Root = -1

    InFile = FreeFile
    IF OPEN(FileName FOR Input AS #InFile) <> 0 THEN
        PRINT "error: could not open input file: "; FileName
        System(1)
    END IF

    WHILE NOT EOF(InFile)
        LINE INPUT #InFile, LineStr
        ArrowIdx = InStr(LineStr, " -> ")
        Dest = Mid(LineStr, ArrowIdx + 4)
        IF InStr(LineStr, "NOT ") <> 0 THEN
            Src1 = Mid(LineStr, 5, ArrowIdx - 5)
            Op.Op = Operation.OperatorType.NOT_OP
            Op.Source1 = Src1
            InsertOpNode(Operations, Operations.Root, Dest, Op)
        ELSEIF InStr(LineStr, " AND ") <> 0 THEN
            OpIdx = InStr(LineStr, " AND ")
            Src1 = Mid(LineStr, 1, OpIdx - 1)
            Src2 = Mid(LineStr, OpIdx + 5, ArrowIdx - (OpIdx + 5))
            Op.Op = Operation.OperatorType.AND_OP
            Op.Source1 = Src1
            Op.Source2 = Src2
            InsertOpNode(Operations, Operations.Root, Dest, Op)
        ELSEIF InStr(LineStr, " OR ") <> 0 THEN
            OpIdx = InStr(LineStr, " OR ")
            Src1 = Mid(LineStr, 1, OpIdx - 1)
            Src2 = Mid(LineStr, OpIdx + 4, ArrowIdx - (OpIdx + 4))
            Op.Op = Operation.OperatorType.OR_OP
            Op.Source1 = Src1
            Op.Source2 = Src2
            InsertOpNode(Operations, Operations.Root, Dest, Op)
        ELSEIF InStr(LineStr, " LSHIFT ") <> 0 THEN
            OpIdx = InStr(LineStr, " LSHIFT ")
            Src1 = Mid(LineStr, 1, OpIdx - 1)
            Src2 = Mid(LineStr, OpIdx + 8, ArrowIdx - (OpIdx + 8))
            Op.Op = Operation.OperatorType.LSHIFT_OP
            Op.Source1 = Src1
            Op.Amount = VALINT(Src2)
            InsertOpNode(Operations, Operations.Root, Dest, Op)
        ELSEIF InStr(LineStr, " RSHIFT ") <> 0 THEN
            OpIdx = InStr(LineStr, " RSHIFT ")
            Src1 = Mid(LineStr, 1, OpIdx - 1)
            Src2 = Mid(LineStr, OpIdx + 8, ArrowIdx - (OpIdx + 8))
            Op.Op = Operation.OperatorType.RSHIFT_OP
            Op.Source1 = Src1
            Op.Amount = VALINT(Src2)
            InsertOpNode(Operations, Operations.Root, Dest, Op)
        ELSE
            Op.Op = Operation.OperatorType.ASSIGN_OP
            Src1 = Mid(LineStr, 1, ArrowIdx - 1)
            Op.Source1 = Src1
            InsertOpNode(Operations, Operations.Root, Dest, Op)
        END IF
    WEND
    CLOSE #InFile
    A = Eval(Operations, Cache, "a")
    Op.Op = Operation.OperatorType.ASSIGN_OP
    Op.Source1 = Str(A)
    InsertOpNode(Operations, Operations.Root, "b", Op)
    Cache.NextFree = 0
    Cache.Root = -1
    RETURN Eval(Operations, Cache, "a")
END FUNCTION

DIM Argc AS Integer
DIM Argv AS ZString Ptr Ptr

DIM FileName AS String
DIM Result AS Integer

Argc = __FB_ARGC__
Argv = __FB_ARGV__

IF Argc < 2 THEN
    Usage(*Argv[0])
END IF

FileName = *Argv[1]
Result = Process(FileName)

PRINT "result ="; result


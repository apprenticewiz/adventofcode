IMPLEMENTATION MODULE StringSet;

FROM DynamicStrings IMPORT String, Dup, Fin, Length, char;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;
FROM SYSTEM IMPORT TSIZE;

PROCEDURE Compare(S1, S2 : String) : INTEGER;
VAR
    I, LenS1, LenS2  : INTEGER;
    Ch1, Ch2         : CHAR;
BEGIN
    I := 0;
    LenS1 := Length(S1);
    LenS2 := Length(S2);
    LOOP
       IF (I < LenS1) AND (I < LenS2) THEN
         Ch1 := char(S1, I);
         Ch2 := char(S2, I);
         IF Ch1 < Ch2 THEN
             RETURN -1;
         ELSIF Ch1 > Ch2 THEN
             RETURN 1;
         END;
       ELSIF (I = LenS1) AND (I < LenS2) THEN
           RETURN -1;
       ELSIF (I = LenS2) AND (I < LenS1) THEN
           RETURN 1;
       ELSIF (I = LenS1) AND (I = LenS2) THEN
           RETURN 0;
       END;
       INC(I);
    END;
END Compare;

PROCEDURE Create() : StringSet;
VAR
    NewSet    : StringSet;
BEGIN
    NewSet.Root := NIL;
    RETURN NewSet;
END Create;

PROCEDURE Delete(VAR S : StringSet);
BEGIN
    DeleteTreeAt(S.Root);
END Delete;

PROCEDURE DeleteTreeAt(VAR Node : SetNodePtr);
BEGIN
    IF Node <> NIL THEN
        DeleteTreeAt(Node^.Left);
        DeleteTreeAt(Node^.Right);
        Fin(Node^.Key);
        DEALLOCATE(Node, TSIZE(SetNode));
    END;
END DeleteTreeAt;

PROCEDURE Insert(VAR S : StringSet; Key : String);
BEGIN
    InsertIntoTreeAt(S.Root, Key);
END Insert;

PROCEDURE InsertIntoTreeAt(VAR Node: SetNodePtr; Key : String);
BEGIN
   IF Node = NIL THEN
       ALLOCATE(Node, TSIZE(SetNode));
       Node^.Key := Dup(Key);
       Node^.Left := NIL;
       Node^.Right := NIL;
   ELSIF Compare(Key, Node^.Key) = -1 THEN
       InsertIntoTreeAt(Node^.Left, Key);
   ELSIF Compare(Key, Node^.Key) = 1 THEN
       InsertIntoTreeAt(Node^.Right, Key);
   END;
END InsertIntoTreeAt;

PROCEDURE Find(S : StringSet; Key : String) : BOOLEAN;
BEGIN
    RETURN FindInTreeAt(S.Root, Key);
END Find;

PROCEDURE FindInTreeAt(Node : SetNodePtr; Key : String) : BOOLEAN;
BEGIN
    IF Node = NIL THEN
        RETURN FALSE;
    ELSIF Compare(Key, Node^.Key) = 0 THEN
        RETURN TRUE;
    ELSIF Compare(Key, Node^.Key) = -1 THEN
        RETURN FindInTreeAt(Node^.Left, Key);
    ELSE
        RETURN FindInTreeAt(Node^.Right, Key);
    END;
END FindInTreeAt;

PROCEDURE Count(S : StringSet) : CARDINAL;
BEGIN
    RETURN CountNodesFrom(S.Root);
END Count;

PROCEDURE CountNodesFrom(Node : SetNodePtr) : CARDINAL;
BEGIN
    IF Node = NIL THEN
        RETURN 0;
    ELSE
        RETURN 1 + CountNodesFrom(Node^.Left) + CountNodesFrom(Node^.Right);
    END;
END CountNodesFrom;

END StringSet.

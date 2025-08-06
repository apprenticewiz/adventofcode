IMPLEMENTATION MODULE Args;

FROM DynamicStrings IMPORT String, InitStringCharStar;
FROM SYSTEM IMPORT ADDRESS;
FROM UnixArgs IMPORT GetArgC, GetArgV;

TYPE ArgvPtr = POINTER TO ARRAY [0..(ArgMax - 1)] OF ADDRESS;

PROCEDURE ArgCount() : CARDINAL;
BEGIN
    RETURN CARDINAL(GetArgC());
END ArgCount;

PROCEDURE GetArgument(N : CARDINAL) : String;
VAR
    Arg  : String;
    Argv : ArgvPtr;
BEGIN
    Argv := ArgvPtr(GetArgV());
    Arg := InitStringCharStar(Argv^[N]);
    RETURN Arg;
END GetArgument;

END Args.

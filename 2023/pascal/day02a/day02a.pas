PROGRAM Day02a(INPUT, OUTPUT);
{$H+}

USES StrUtils, SysUtils;

CONST
    TOTAL_RED   : INTEGER = 12;
    TOTAL_GREEN : INTEGER = 13;
    TOTAL_BLUE  : INTEGER = 14;

VAR
    FileName : STRING;
    Res      : INT32;

PROCEDURE Usage;
BEGIN
    WriteLn('usage: ', ExtractFileName(paramStr(0)), ' <file>');
    Halt(1)
END;

FUNCTION Process(FileName : STRING) : INTEGER;
VAR
    InFile      : TEXT;
    FileLine    : STRING;
    Res         : INT32 = 0;
    Position    : INTEGER;
    GamePart    : STRING;
    GameNumPart : STRING;
    GameNum     : INTEGER;
    DrawsPart   : STRING;
    Valid       : BOOLEAN;
    DrawPart    : STRING;
    ColorAmount : STRING;
    InnerPos    : INTEGER;
    ColorPos    : INTEGER;
    AmountStr   : STRING;
    Color       : STRING;
    Amount      : INTEGER;
BEGIN
    Assign(InFile, FileName);
    Reset(InFile);
    WHILE NOT Eof(InFile) DO
    BEGIN
        ReadLn(InFile, FileLine);
        Position := NPos(': ', FileLine, 1);
        IF Position > 0 THEN
        BEGIN
            GamePart := MidStr(FileLine, 1, Position - 1);
            DrawsPart := MidStr(FileLine, Position + 2, Length(FileLine) - Position - 1);
            Position := NPos(' ', GamePart, 1);
            IF Position > 0 THEN
            BEGIN
                GameNumPart := MidStr(GamePart, Position + 1, Length(GamePart) - Position);
                GameNum := StrToInt(GameNumPart);
                Valid := True;
                REPEAT
                    Position := NPos('; ', DrawsPart, 1);
                    IF Position > 0 THEN
                    BEGIN
                        DrawPart := MidStr(DrawsPart, 1, Position - 1);
                        DrawsPart := MidStr(DrawsPart, Position + 2, Length(DrawsPart) - Position - 1)
                    END
                    ELSE
                    BEGIN
                        DrawPart := DrawsPart;
                    END;
                    REPEAT
                        InnerPos := NPos(', ', DrawPart, 1);
                        IF InnerPos > 0 THEN
                        BEGIN
                            ColorAmount := MidStr(DrawPart, 1, InnerPos - 1);
                            DrawPart := MidStr(DrawPart, InnerPos + 2, Length(DrawPart) - InnerPos - 1);
                        END
                        ELSE
                        BEGIN
                            ColorAmount := DrawPart;
                        END;
                        ColorPos := NPos(' ', ColorAmount, 1);
                        IF ColorPos > 0 THEN
                        BEGIN
                            AmountStr := MidStr(ColorAmount, 1, ColorPos - 1);
                            Amount := StrToInt(AmountStr);
                            Color := MidStr(ColorAmount, ColorPos + 1, Length(ColorAmount) - ColorPos);
                            IF (Color = 'red') AND (Amount > TOTAL_RED) THEN
                                Valid := False
                            ELSE IF (Color = 'green') AND (Amount > TOTAL_GREEN) THEN
                                Valid := False
                            ELSE IF (Color = 'blue') AND (Amount > TOTAL_BLUE) THEN
                                Valid := False
                        END
                    UNTIL InnerPos = 0
                UNTIL Position = 0;
                IF Valid THEN
                    Res := Res + GameNum
            END;
        END;
    END;
    Close(InFile);
    Process := Res
END;

BEGIN
    IF paramCount() < 1 THEN
        Usage;
    FileName := paramStr(1);
    Res := Process(FileName);
    WriteLn('result = ', Res)
END.

with Ada.Command_Line;               use Ada.Command_Line;
with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.Integer_Text_IO;            use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;
with Ada.Text_IO;                    use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;       use Ada.Text_IO.Unbounded_IO;
with GNAT.OS_Lib;

procedure day06a is

RowMax : constant Positive := 1000;
ColMax : constant Positive := 1000;

type Grid_Type is array(1 .. RowMax, 1 .. ColMax) of Boolean;

procedure Usage(Progname : String) is
begin
  Put_Line("usage: " & Progname & " <input file>");
  GNAT.OS_Lib.OS_Exit(1);
end Usage;

procedure InitGrid(Grid : in out Grid_Type) is
begin
  for Row in 1 .. RowMax loop
    for Col in 1 .. ColMax loop
      Grid(Row, Col) := False;
    end loop;
  end loop;
end InitGrid;

procedure Perform(Grid : in out Grid_Type;
                  Action : Unbounded_String;
                  R1, C1, R2, C2 : Positive) is
begin
    for Row in R1 .. R2 loop
        for Col in C1 .. C2 loop
            if Action = "turn on" then
                Grid(Row, Col) := True;
            elsif Action = "turn off" then
                Grid(Row, Col) := False;
            elsif Action = "toggle" then
                Grid(Row, Col) := not Grid(Row, Col);
            end if;
        end loop;
    end loop;
end Perform;

function Count(Grid : Grid_Type) return Integer is
  Total          : Integer := 0;
begin
    for Row in 1 .. RowMax loop
        for Col in 1 .. ColMax loop
            if Grid(Row, Col) then
                Total := Total + 1;
            end if;
        end loop;
    end loop;
    return Total;
end Count;

function Process(Filename : String) return Integer is
  File           : File_Type;
  Line           : Unbounded_String;
  Grid           : Grid_Type;
  Action         : Unbounded_String;
  CoordIdx       : Positive;
  Coords         : Unbounded_String;
  ThroughIdx     : Positive;
  Coord1, Coord2 : Unbounded_String;
  CommaIdx       : Positive;
  Tok            : Unbounded_String;
  R1, C1, R2, C2 : Positive;
begin
  InitGrid(Grid);
  Open(File, In_File, Filename);
  while not End_Of_File(File) loop
    Get_Line(File, Line);
    if Slice(Line, 1, 6) = "toggle" then
       Action := To_Unbounded_String(Slice(Line, 1, 6));
       CoordIdx := 8;
    elsif Slice(Line, 1, 7) = "turn on" then
       Action := To_Unbounded_String(Slice(Line, 1, 7));
       CoordIdx := 9;
    elsif Slice(Line, 1, 8) = "turn off" then
        Action := To_Unbounded_String(Slice(Line, 1, 8));
        CoordIdx := 10;
    else
        raise Program_Error;
    end if;
    Coords := Unbounded_Slice(Line, CoordIdx, Length(Line));
    ThroughIdx := Index(Coords, " through ");
    Coord1 := Unbounded_Slice(Coords, 1, ThroughIdx - 1);
    Coord2 := Unbounded_Slice(Coords, ThroughIdx + 9, Length(Coords));
    CommaIdx := Index(Coord1, ",");
    Tok := Unbounded_Slice(Coord1, 1, CommaIdx - 1);
    R1 := Positive'Value(To_String(Tok)) + 1;
    Tok := Unbounded_Slice(Coord1, CommaIdx + 1, Length(Coord1));
    C1 := Positive'Value(To_String(Tok)) + 1;
    CommaIdx := Index(Coord2, ",");
    Tok := Unbounded_Slice(Coord2, 1, CommaIdx - 1);
    R2 := Positive'Value(To_String(Tok)) + 1;
    Tok := Unbounded_Slice(Coord2, CommaIdx + 1, Length(Coord2));
    C2 := Positive'Value(To_String(Tok)) + 1;
    Perform(Grid, Action, R1, C1, R2, C2);
  end loop;
  return Count(Grid);
exception
  when E : others =>
    Put("error while processing file: " & Filename & " - got exception: ");
    Put_Line(Exception_Information(E));
    return 0;
end Process;

begin
  if Argument_Count < 1 then
    Usage(Command_Name);
  end if;

  declare
    Filename : constant String := Argument(1);
    Result   : constant Integer := Process(Filename);
  begin
    Put("result = ");
    Put(Result, Width => 0);
    New_Line;
  end;
  
end day06a;

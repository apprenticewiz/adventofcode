with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;  use Ada.Strings.Unbounded.Text_IO;
with GNAT.OS_Lib;

procedure day02b is

procedure Usage(Progname : String) is
begin
  Put_Line("usage: " & Progname & " <input file>");
  GNAT.OS_Lib.OS_Exit(1);
end Usage;

function Process(Filename : String) return Integer is
  Total_Len    : Integer := 0;
  L, W, H      : Integer;
  P1, P2, P3   : Integer;
  Present_Len  : Integer;
  Bow_Len      : Integer;
  File         : File_Type;
  Line         : Unbounded_String;
  Token        : Unbounded_String;
  Pos1, Pos2   : Integer;
begin
  Open(File, In_File, Filename);
  while not End_Of_File(File) loop
    Line := Get_Line(File);
    Pos1 := Index(Line, "x", 1);
    Token := Unbounded_Slice(Line, 1, Pos1 - 1);
    L := Integer'Value(To_String(Token));
    Pos2 := Index(Line, "x", Pos1 + 1);
    Token := Unbounded_Slice(Line, Pos1 + 1, Pos2 - 1);
    W := Integer'Value(To_String(Token));
    Token := Unbounded_Slice(Line, Pos2 + 1, Length(Line));
    H := Integer'Value(To_String(Token));
    P1 := 2 * (L + W);
    P2 := 2 * (L + H);
    P3 := 2 * (W + H);
    Present_Len := Integer'Min(P1, Integer'Min(P2, P3));
    Bow_Len := L * W * H;
    Total_Len := Total_Len + Present_Len + Bow_Len;
  end loop;
  Close(File);
  return Total_Len;
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
  
end day02b;

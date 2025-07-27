with Ada.Command_Line;               use Ada.Command_Line;
with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.Integer_Text_IO;            use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;
with Ada.Text_IO;                    use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;       use Ada.Text_IO.Unbounded_IO;
with GNAT.OS_Lib;

procedure day05a is

procedure Usage(Progname : String) is
begin
  Put_Line("usage: " & Progname & " <input file>");
  GNAT.OS_Lib.OS_Exit(1);
end Usage;

function Prop1(Str : Unbounded_String) return Boolean is
  Vowels       : Integer := 0;
begin
  for I in 1 .. Length(Str) loop
    if (Element(Str, I) in 'a' | 'e' | 'i' | 'o' | 'u') then
      Vowels := Vowels + 1;
    end if;
  end loop;
  return Vowels >= 3;
end Prop1;

function Prop2(Str : Unbounded_String) return Boolean is
begin
  for I in 2 .. Length(Str) loop
    if Element(Str, I) = Element(Str, I - 1) then
      return True;
    end if;
  end loop;
  return False;
end Prop2;

function Prop3(Str : Unbounded_String) return Boolean is
begin
  return (Index(Str, "ab") = 0 and Index(Str, "cd") = 0
    and Index(Str, "pq") = 0 and Index(Str, "xy") = 0);
end Prop3;

function Process(Filename : String) return Integer is
  Count        : Integer := 0;
  File         : File_Type;
  Line         : Unbounded_String;
begin
  Open(File, In_File, Filename);
  while not End_Of_File(File) loop
    Get_Line(File, Line);
    if Prop1(Line) and Prop2(Line) and prop3(Line) then
      Count := Count + 1;
    end if;
  end loop;
  return Count;
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
  
end day05a;

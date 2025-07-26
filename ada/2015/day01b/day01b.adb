with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with GNAT.OS_Lib;

procedure day01b is

procedure Usage(Progname : String) is
begin
  Put_Line("usage: " & Progname & " <input file>");
  GNAT.OS_Lib.OS_Exit(1);
end Usage;

function Process(Filename : String) return Integer is
  Counter : Integer := 0;
  I       : Integer := 0;
  Ch      : Character;
  File    : File_Type;
begin
  Open(File, In_File, Filename);
  while not End_Of_File(File) loop
    Get(File, Ch);
    I := I + 1;
    case Ch is
      when '(' =>
        Counter := Counter + 1;
      when ')' =>
        Counter := Counter - 1;
      when others =>
        null;
    end case;
    if Counter < 0 then
      return I;
    end if;
  end loop;
  Close(File);
  return 0;
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
  
end day01b;

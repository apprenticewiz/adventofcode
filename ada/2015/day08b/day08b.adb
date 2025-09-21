with Ada.Command_Line;               use Ada.Command_Line;
with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.Integer_Text_IO;            use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;
with Ada.Text_IO;                    use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;       use Ada.Text_IO.Unbounded_IO;
with GNAT.OS_Lib;

procedure day08a is

procedure Usage(Progname : String) is
begin
  Put_Line("usage: " & Progname & " <input file>");
  GNAT.OS_Lib.OS_Exit(1);
end Usage;

function Process(Filename : String) return Integer is
  Result            : Integer := 0;
  File              : File_Type;
  Line              : Unbounded_String;
  Code_Len, Enc_Len : Integer;
begin
  Open(File, In_File, Filename);
  while not End_Of_File(File) loop
    Get_Line(File, Line);
    Code_Len := Length(Line);
    Enc_Len := 0;
    for I in 1 .. Length(Line) loop
      case Element(Line, I) is
        when '\' => 
          Enc_Len := Enc_Len + 2;
        when '"' =>
          Enc_Len := Enc_Len + 2;
        when others =>
          Enc_Len := Enc_Len + 1;
      end case;
    end loop;
    Result := Result + 2 + (Enc_Len - Code_Len);
  end loop;
  return Result;
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
  
end day08a;

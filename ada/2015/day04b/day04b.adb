with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with GNAT.MD5;
with GNAT.OS_Lib;

procedure day04b is

procedure Usage(Progname : String) is
begin
  Put_Line("usage: " & Progname & " <key>");
  GNAT.OS_Lib.OS_Exit(1);
end Usage;

function Process(Key : String) return Integer is
  N             : Integer := 1;
  Try_Key, Temp : Unbounded_String;
  Hash_Str      : String(1 .. 32);
begin
  loop
    Temp := To_Unbounded_String(Integer'Image(N));
    Try_Key := To_Unbounded_String(Key) & Slice(Temp, 2, Length(Temp));
    Hash_Str := GNAT.MD5.Digest(To_String(Try_Key));
    exit when Hash_Str(Hash_Str'First .. Hash_Str'First + 5) = "000000";
    N := N + 1;
  end loop;
  return N;
end Process;

begin
  if Argument_Count < 1 then
    Usage(Command_Name);
  end if;

  declare
    Key      : constant String := Argument(1);
    Result   : constant Integer := Process(Key);
  begin
    Put("result = ");
    Put(Result, Width => 0);
    New_Line;
  end;
  
end day04b;

with Ada.Command_Line;               use Ada.Command_Line;
with Ada.Containers;                 use Ada.Containers;
with Ada.Containers.Hashed_Sets;
with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.Integer_Text_IO;            use Ada.Integer_Text_IO;
with Ada.Text_IO;                    use Ada.Text_IO;
with GNAT.OS_Lib;

procedure day03a is

type Position2D is record
  X : Integer;
  Y : Integer;
end record;

function "=" (A, B : Position2D) return Boolean is
begin
  return A.X = B.X and A.Y = B.Y;
end "=";

function Hash(P : Position2D) return Ada.Containers.Hash_Type is
begin
  return Ada.Containers.Hash_Type(P.X + 65536) * 4 +
    Ada.Containers.Hash_Type(P.Y + 65536);
end Hash;

procedure Usage(Progname : String) is
begin
  Put_Line("usage: " & Progname & " <input file>");
  GNAT.OS_Lib.OS_Exit(1);
end Usage;

package Position2D_Sets is new Ada.Containers.Hashed_Sets
  (Element_Type        => Position2D,
   Hash                => Hash,
   Equivalent_Elements => "=");
  
use Position2D_Sets;

function Process(Filename : String) return Integer is
  File         : File_Type;
  Ch           : Character;
  Santa        : Position2D := (X => 0, Y => 0);
  Positions    : Position2D_Sets.Set;
begin
  Open(File, In_File, Filename);
  while not End_Of_File(File) loop
    Get(File, Ch);
    case Ch is
      when '^' =>
        Santa.Y := Santa.Y + 1;
      when 'v' =>
        Santa.Y := Santa.Y - 1;
      when '<' =>
        Santa.X := Santa.X - 1;
      when '>' =>
        Santa.X := Santa.X + 1;
      when others =>
        null;
    end case;
    if not Contains(Positions, Santa) then
      Insert(Positions, Santa);
    end if;
  end loop;
  return Integer(Length(Positions));
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
  
end day03a;

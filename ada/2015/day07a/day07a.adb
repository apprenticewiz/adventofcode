with Ada.Command_Line;               use Ada.Command_Line;
with Ada.Containers.Ordered_Maps;
with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.Integer_Text_IO;            use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;
with Ada.Text_IO;                    use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;       use Ada.Text_IO.Unbounded_IO;
with GNAT.OS_Lib;
with GNAT.RegPat;                    use GNAT.RegPat;

procedure day07a is

type UInt16 is mod 2 ** 16;

type Operator_Type is (AssignOp, NotOp, AndOp, OrOp, LeftShiftOp, RightShiftOp);

type Operation is record
  Operator : Operator_Type;
  Source1  : Unbounded_String;
  Source2  : Unbounded_String;
  Amount   : Integer;
end record;

package Operation_Maps is new Ada.Containers.Ordered_Maps
    (Key_Type      => Unbounded_String,
     Element_Type  => Operation,
     "<"           => "<");

package Cache_Maps is new Ada.Containers.Ordered_Maps
    (Key_Type      => Unbounded_String,
     Element_Type  => UInt16,
     "<"           => "<");

use Cache_Maps;

procedure Usage(Progname : String) is
begin
  Put_Line("usage: " & Progname & " <input file>");
  GNAT.OS_Lib.OS_Exit(1);
end Usage;

function AllDigits(US : Unbounded_String) return Boolean is
  S           : constant String := To_String(US);
begin
  for C of S loop
    if not (C in '0'..'9') then
      return False;
    end if;
  end loop;
  return True;
end AllDigits;

function Eval(Ops : Operation_Maps.Map; Cache : in out Cache_Maps.Map; Expr : Unbounded_String) return UInt16 is
  Op              : Operation;
  A, B, R         : UInt16;
begin
  if AllDigits(Expr) then
    return UInt16(Integer'Value(To_String(Expr)));
  elsif Cache_Maps.Find(Cache, Expr) /= Cache_Maps.No_Element then
    return Cache_Maps.Element(Cache_Maps.Find(Cache, Expr));
  else
    Op := Operation_Maps.Element(Operation_Maps.Find(Ops, Expr));
    case Op.Operator is
      when AssignOp =>
        R := Eval(Ops, Cache, Op.Source1);
      when NotOp =>
        A := Eval(Ops, Cache, Op.Source1);
        R := not A;
      when AndOp =>
        A := Eval(Ops, Cache, Op.Source1);
        B := Eval(Ops, Cache, Op.Source2);
        R := A and B;
      when OrOp =>
        A := Eval(Ops, Cache, Op.Source1);
        B := Eval(Ops, Cache, Op.Source2);
        R := A or B;
      when LeftShiftOp =>
        A := Eval(Ops, Cache, Op.Source1);
        R := A * (2 ** Op.Amount);
      when RightShiftOp =>
        A := Eval(Ops, Cache, Op.Source1);
        R := A / (2 ** Op.Amount);
    end case;
  end if;
  Cache_Maps.Insert(Cache, Expr, R);
  return R;
end Eval;

function Process(Filename : String) return Integer is
  File        : File_Type;
  Line        : Unbounded_String;
  Operations  : Operation_Maps.Map;
  Cache       : Cache_Maps.Map;
  P1          : constant Pattern_Matcher := Compile("^(\d+|\w+) -> (\w+)$");
  P2          : constant Pattern_Matcher := Compile("NOT (\d+|\w+) -> (\w+)");
  P3          : constant Pattern_Matcher := Compile("(\d+|\w+) (AND|OR) (\d+|\w+) -> (\w+)");
  P4          : constant Pattern_Matcher := Compile("(\d+|\w+) (LSHIFT|RSHIFT) (\d+) -> (\w+)");
  Matches     : Match_Array (0..4);
  Op          : Operation;
  OpStr, Dest : Unbounded_String;
begin
  Open(File, In_File, Filename);
  while not End_Of_File(File) loop
    Get_Line(File, Line);
    Match(P1, To_String(Line), Matches);
    if Matches(0) /= No_Match then
        Op.Operator := AssignOp;
        Op.Source1 := Unbounded_Slice(Line, Matches(1).First, Matches(1).Last);
        Dest := Unbounded_Slice(Line, Matches(2).First, Matches(2).Last);
        Operation_Maps.Insert(Operations, Dest, Op);
    end if;
    Match(P2, To_String(Line), Matches);
    if Matches(0) /= No_Match then
        Op.Operator := NotOp;
        Op.Source1 := Unbounded_Slice(Line, Matches(1).First, Matches(1).Last);
        Dest := Unbounded_Slice(Line, Matches(2).First, Matches(2).Last);
        Operation_Maps.Insert(Operations, Dest, Op);
    end if;
    Match(P3, To_String(Line), Matches);
    if Matches(0) /= No_Match then
        Op.Source1 := Unbounded_Slice(Line, Matches(1).First, Matches(1).Last);
        OpStr := Unbounded_Slice(Line, Matches(2).First, Matches(2).Last);
        if OpStr = "AND" then
            Op.Operator := AndOp;
        else
            Op.Operator := OrOp;
        end if;
        Op.Source2 := Unbounded_Slice(Line, Matches(3).First, Matches(3).Last);
        Dest := Unbounded_Slice(Line, Matches(4).First, Matches(4).Last);
        Operation_Maps.Insert(Operations, Dest, Op);
    end if;
    Match(P4, To_String(Line), Matches);
    if Matches(0) /= No_Match then
        Op.Source1 := Unbounded_Slice(Line, Matches(1).First, Matches(1).Last);
        OpStr := Unbounded_Slice(Line, Matches(2).First, Matches(2).Last);
        if OpStr = "LSHIFT" then
            Op.Operator := LeftShiftOp;
        else
            Op.Operator := RightShiftOp;
        end if;
        Op.Amount := Integer'Value(To_String(Unbounded_Slice(Line, Matches(3).First, Matches(3).Last)));
        Dest := Unbounded_Slice(Line, Matches(4).First, Matches(4).Last);
        Operation_Maps.Insert(Operations, Dest, Op);
    end if;
  end loop;
  return Integer(Eval(Operations, Cache, To_Unbounded_String("a")));
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
  
end day07a;

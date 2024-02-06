with Ada.Command_Line;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.OS_Lib;

procedure Day04a is
   package Natural_Sets is new Ada.Containers.Ordered_Sets (Element_Type => Natural);

   procedure Usage is
      Prog_Name : constant String := Ada.Command_Line.Command_Name;
   begin
      Ada.Text_IO.Put_Line("usage: " & Prog_Name & " <file>");
      GNAT.OS_Lib.OS_Exit(1);
   end Usage;

   function IsDigit(Ch : Character) return Boolean is
   begin
      return (Ch >= '0') and (Ch <= '9');
   end IsDigit;

   procedure ScanNumbers (InputStr  : in Ada.Strings.Unbounded.Unbounded_String;
                          NumberSet : in out Natural_Sets.Set) is
      Pos, StartPos  : Natural := 1;
      NumStr         : Ada.Strings.Unbounded.Unbounded_String;
      ScanningNumber : Boolean := false;
   begin
      loop
         if ScanningNumber then
            if not IsDigit(Ada.Strings.Unbounded.Element(InputStr, Pos)) then
               NumStr := Ada.Strings.Unbounded.Unbounded_Slice(Source => InputStr,
                                                               Low => StartPos,
                                                               High => Pos - 1);
               Natural_Sets.Insert(NumberSet, Natural'Value(Ada.Strings.Unbounded.To_String(NumStr)));
               ScanningNumber := false;
            end if;
         else
            if IsDigit(Ada.Strings.Unbounded.Element(InputStr, Pos)) then
               StartPos := Pos;
               ScanningNumber := true;
            end if;
         end if;
         Pos := Pos + 1;
         if Pos > Ada.Strings.Unbounded.Length(InputStr) then
            exit;
         end if;
      end loop;
      if ScanningNumber then
         NumStr := Ada.Strings.Unbounded.Unbounded_Slice(Source => InputStr,
                                                         Low => StartPos,
                                                         High => Ada.Strings.Unbounded.Length(InputStr));
          Natural_Sets.Insert(NumberSet, Natural'Value(Ada.Strings.Unbounded.To_String(NumStr)));
      end if;
   end ScanNumbers;

   function Process(Filename : String) return Natural is
      Input_File                        : Ada.Text_IO.File_Type;
      Line                              : Ada.Strings.Unbounded.Unbounded_String;
      Rest, WinningStr, HandStr, NumStr : Ada.Strings.Unbounded.Unbounded_String;
      Result                            : Natural := 0;
      Pos                               : Natural;
      WinningSet, HandSet, Intersection : Natural_Sets.Set;
      Count                             : Ada.Containers.Count_Type;
   begin
      Ada.Text_IO.Open(File => Input_File,
                       Mode => Ada.Text_IO.In_File,
                       Name => Filename);
      while not (Ada.Text_IO.End_Of_File(Input_File)) loop
         Line := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line(Input_File));
         Pos := Ada.Strings.Unbounded.Index(Source => Line,
                                            Pattern => ": ",
                                            From => 1);
         Rest := Ada.Strings.Unbounded.Unbounded_Slice(Source => Line,
                                                       Low => Pos + 2,
                                                       High => Ada.Strings.Unbounded.Length(Line));
         Pos := Ada.Strings.Unbounded.Index(Source => Rest,
                                            Pattern => " | ",
                                            From => 1);
         WinningStr := Ada.Strings.Unbounded.Unbounded_Slice(Source => Rest,
                                                             Low => 1,
                                                             High => Pos - 1);
         HandStr := Ada.Strings.Unbounded.Unbounded_Slice(Source => Rest, 
                                                          Low => Pos + 3,
                                                          High => Ada.Strings.Unbounded.Length(Rest));
         Natural_Sets.Clear(WinningSet);
         ScanNumbers(WinningStr, WinningSet);
         Natural_Sets.Clear(HandSet);
         ScanNumbers(HandStr, HandSet);
         Intersection := Natural_Sets.Intersection(WinningSet, HandSet);
         Count := Natural_Sets.Length(Intersection);
         if Count > 0 then
            Result := Result + 2 ** (Integer(Count) - 1);
         end if;
      end loop;
      Ada.Text_IO.Close(Input_File);
      return Result;
   end Process;

   Filename : constant String := Ada.Command_Line.Argument(1);
   Contents : Ada.Strings.Unbounded.Unbounded_String;
   Result  : Natural := 0;
begin
   if Ada.Command_Line.Argument_Count < 1 then
      Usage;
   end if;
   Result := Process(Filename);
   Ada.Text_IO.Put_Line("result = " & Natural'Image(Result));
end Day04a;

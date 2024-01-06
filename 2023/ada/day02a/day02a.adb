with Ada.Command_Line;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.OS_Lib;

procedure Day02a is
   Total_Red : constant Positive := 12;
   Total_Green : constant Positive := 13;
   Total_Blue : constant Positive := 14;

   procedure Usage is
      Prog_Name : constant String := Ada.Command_Line.Command_Name;
   begin
      Ada.Text_IO.Put_Line("usage: " & Prog_Name & " <file>");
      GNAT.OS_Lib.OS_Exit(1);
   end Usage;

   function Process(Filename : String) return Natural is
      Input_File : Ada.Text_IO.File_Type;
      Line : Ada.Strings.Unbounded.Unbounded_String;
      Game_Part, Draws_Part : Ada.Strings.Unbounded.Unbounded_String;
      Game_Num_Part : Ada.Strings.Unbounded.Unbounded_String;
      Draw_Part, Color_Amount : Ada.Strings.Unbounded.Unbounded_String;
      Color, Amount_Str : Ada.Strings.Unbounded.Unbounded_String;
      Result : Natural := 0;
      Pos, Inner_Pos, Color_Pos : Natural := 0;
      Game_Num : Natural := 0;
      Amount : Natural := 0;
      Valid : Boolean := True;
   begin
      Ada.Text_IO.Open(File => Input_File,
                       Mode => Ada.Text_IO.In_File,
                       Name => Filename);
      while not (Ada.Text_IO.End_Of_File(Input_File)) loop
         Line := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line(Input_File));
         Pos := Ada.Strings.Unbounded.Index(Source => Line,
                                            Pattern => ": ",
                                            From => 1);
         Game_Part := Ada.Strings.Unbounded.Unbounded_Slice(Source => Line,
                                                            Low => 1,
                                                            High => Pos - 1);
         Draws_Part := Ada.Strings.Unbounded.Unbounded_Slice(Source => Line,
                                                             Low => Pos + 2,
                                                             High => Ada.Strings.Unbounded.Length(Line));
         Pos := Ada.Strings.Unbounded.Index(Source => Game_Part,
                                            Pattern => " ",
                                            From => 1);
         Game_Num_Part := Ada.Strings.Unbounded.Unbounded_Slice(Source => Game_Part,
                                                                Low => Pos + 1,
                                                                High => Ada.Strings.Unbounded.Length(Game_Part));
         Game_Num := Natural'Value(Ada.Strings.Unbounded.To_String(Game_Num_Part));
         Valid := True;
         loop
            Pos := Ada.Strings.Unbounded.Index(Source => Draws_Part,
                                               Pattern => "; ",
                                               From => 1);
            if Pos /= 0 then
               Draw_Part := Ada.Strings.Unbounded.Unbounded_Slice(Source => Draws_Part,
                                                                  Low => 1,
                                                                  High => Pos - 1);
               Draws_Part := Ada.Strings.Unbounded.Unbounded_Slice(Source => Draws_Part,
                                                                   Low => Pos + 2,
                                                                   High => Ada.Strings.Unbounded.Length(Draws_Part));
            else
               Draw_Part := Draws_Part;
            end if;
            loop
               Inner_Pos := Ada.Strings.Unbounded.Index(Source => Draw_Part,
                                                        Pattern => ", ",
                                                        From => 1);
               if Inner_Pos /= 0 then
                  Color_Amount := Ada.Strings.Unbounded.Unbounded_Slice(Source => Draw_Part,
                                                                        Low => 1,
                                                                        High => Inner_Pos - 1);
                  Draw_Part := Ada.Strings.Unbounded.Unbounded_Slice(Source => Draw_Part,
                                                                     Low => Inner_Pos + 2,
                                                                     High => Ada.Strings.Unbounded.Length(Draw_Part));
               else
                  Color_Amount := Draw_Part;
               end if;
               Color_Pos := Ada.Strings.Unbounded.Index(Source => Color_Amount,
                                                        Pattern => " ",
                                                        From => 1);
               Amount_Str := Ada.Strings.Unbounded.Unbounded_Slice(Source => Color_Amount,
                                                                   Low => 1,
                                                                   High => Color_Pos - 1);
               Color := Ada.Strings.Unbounded.Unbounded_Slice(Source => Color_Amount,
                                                              Low => Color_Pos + 1,
                                                              High => Ada.Strings.Unbounded.Length(Color_Amount));
               Amount := Natural'Value(Ada.Strings.Unbounded.To_String(Amount_Str));
               if Ada.Strings.Unbounded.To_String(Color) = "red" and Amount > Total_Red then
                  Valid := False;
               elsif Ada.Strings.Unbounded.To_String(Color) = "green" and Amount > Total_Green then
                  Valid := False;
               elsif Ada.Strings.Unbounded.To_String(Color) = "blue" and Amount > Total_Blue then
                  Valid := False;
               end if;
               exit when Inner_Pos = 0;
            end loop;
            exit when Pos = 0;
         end loop;
         if Valid then
            Result := Result + Game_Num;
         end if;
      end loop;
      Ada.Text_IO.Close(Input_File);
      return Result;
   end Process;

   Filename : constant String := Ada.Command_Line.Argument(1);
   Contents : Ada.Strings.Unbounded.Unbounded_String;
   Result : Natural := 0;
begin
   if Ada.Command_Line.Argument_Count < 1 then
      Usage;
   end if;
   Result := Process(Filename);
   Ada.Text_IO.Put_Line("result = " & Natural'Image(Result));
end Day02a;

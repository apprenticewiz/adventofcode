with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;
with GNAT.OS_Lib;
use type Ada.Containers.Count_Type;

procedure Day03b is
    type Position is record
        Row : Integer;
        Col : Integer;
    end record;

    type NumberLocation is record
        Number : Ada.Strings.Unbounded.Unbounded_String;
        Pos    : Position;
    end record;

    package Number_Location_Vectors is new Ada.Containers.Vectors
        (Index_Type => Natural,
         Element_Type => NumberLocation);

    type GearLocation is record
        Gear : Character;
        Pos  : Position;
    end record;

    package Gear_Location_Vectors is new Ada.Containers.Vectors
        (Index_Type => Natural,
         Element_Type => GearLocation);

    package Natural_Vectors is new Ada.Containers.Vectors
        (Index_Type => Natural,
         Element_Type => Natural);

    procedure Usage is
        Prog_Name : constant String := Ada.Command_Line.Command_Name;
    begin
        Ada.Text_IO.Put_Line("usage: " & Prog_Name & " <file>");
        GNAT.OS_Lib.OS_Exit(1);
    end Usage;

    function BuildNumbers(Filename : in String) return Number_Location_Vectors.Vector is
        Numbers : Number_Location_Vectors.Vector;
        NumberLoc : NumberLocation;
        Input_File : Ada.Text_IO.File_Type;
        Line : Ada.Strings.Unbounded.Unbounded_String;
        Row : Integer;
        Ch : Character;
        ReadingNumber : Boolean := False;
    begin
        Ada.Text_IO.Open
          (File => Input_File,
           Mode => Ada.Text_IO.In_File,
           Name => Filename);
        Row := 1;
        while not (Ada.Text_IO.End_Of_File(Input_File)) loop
            Line := Ada.Strings.Unbounded.Text_IO.Get_Line(Input_File);
            for Col in 1..Ada.Strings.Unbounded.Length(Line) loop
                Ch := Ada.Strings.Unbounded.Element(Line, Col);
                if ReadingNumber then
                    if Ada.Characters.Handling.Is_Digit(Ch) then
                        Ada.Strings.Unbounded.Append(NumberLoc.Number, Ch);
                    else
                        Number_Location_Vectors.Append(Numbers, NumberLoc);
                        NumberLoc.Number := Ada.Strings.Unbounded.To_Unbounded_String(0);
                        ReadingNumber := False;
                    end if;
                else
                    if Ada.Characters.Handling.Is_Digit(Ch) then
                        Ada.Strings.Unbounded.Append(NumberLoc.Number, Ch);
                        NumberLoc.Pos.Row := Row;
                        NumberLoc.Pos.Col := Col;
                        ReadingNumber := True;
                    end if;
                end if;
            end loop;
            if ReadingNumber then
                Number_Location_Vectors.Append(Numbers, NumberLoc);
                NumberLoc.Number := Ada.Strings.Unbounded.To_Unbounded_String(0);
                ReadingNumber := False;
            end if;
            Row := Row + 1;
        end loop;
        Ada.Text_IO.Close(Input_File);
        return Numbers;
    end BuildNumbers;

    function BuildGears(Filename : in String) return Gear_Location_Vectors.Vector is
        Gears : Gear_Location_Vectors.Vector;
        GearLoc : GearLocation;
        Input_File : Ada.Text_IO.File_Type;
        Line : Ada.Strings.Unbounded.Unbounded_String;
        Row : Integer;
        Ch : Character;
    begin
        Ada.Text_IO.Open
          (File => Input_File,
           Mode => Ada.Text_IO.In_File,
           Name => Filename);
        Row := 1;
        while not (Ada.Text_IO.End_Of_File(Input_File)) loop
            Line := Ada.Strings.Unbounded.Text_IO.Get_Line(Input_File);
            for Col in 1..Ada.Strings.Unbounded.Length(Line) loop
                Ch := Ada.Strings.Unbounded.Element(Line, Col);
                if Ch = '*' then
                    GearLoc.Pos.Row := Row;
                    GearLoc.Pos.Col := Col;
                    GearLoc.Gear := Ch;
                    Gear_Location_Vectors.Append(Gears, GearLoc);
                end if;
            end loop;
            Row := Row + 1;
        end loop;
        Ada.Text_IO.Close(Input_File);
        return Gears;
    end BuildGears;

    function CheckGears(Numbers : in Number_Location_Vectors.Vector;
                        Gears : in Gear_Location_Vectors.Vector) return Natural is
        Result : Natural := 0;
        Prod : Natural;
        Adjacents : Natural_Vectors.Vector;
        AdjRow, AdjCol : Integer;
        Skip : Boolean;
    begin
        for GearLoc of Gears loop
            Adjacents := Natural_Vectors.Empty_Vector;
            for NumberLoc of Numbers loop
                Skip := False;
                for DeltaRow in -1..1 loop
                    AdjRow := GearLoc.Pos.Row + DeltaRow;
                    for DeltaCol in -1..1 loop
                        AdjCol := GearLoc.Pos.Col + DeltaCol;
                        for NumberCol in NumberLoc.Pos.Col..(NumberLoc.Pos.Col + Ada.Strings.Unbounded.Length(NumberLoc.Number) - 1) loop
                            if AdjRow = NumberLoc.Pos.Row and AdjCol = NumberCol then
                                Natural_Vectors.Append(Adjacents, Natural'Value(Ada.Strings.Unbounded.To_String(NumberLoc.Number)));
                                Skip := True;
                                exit;
                            end if;
                        end loop;
                        if Skip then
                            exit;
                        end if;
                    end loop;
                    if Skip then
                        exit;
                    end if;
                end loop;
            end loop;
            if Adjacents.Length = 2 then
                Prod := 1;
                for Adjacent of Adjacents loop
                    Prod := Prod * Adjacent;
                end loop;
                Result := Result + Prod;
            end if;
        end loop;
        return Result;
    end CheckGears;

    function Process(Filename : in String) return Natural is
        Numbers : Number_Location_Vectors.Vector := BuildNumbers(Filename);
        Gears : Gear_Location_Vectors.Vector := BuildGears(Filename);
    begin
        return CheckGears(Numbers, Gears);
    end Process;

    Filename : constant String := Ada.Command_Line.Argument(1);
    Contents : Ada.Strings.Unbounded.Unbounded_String;
    Result : Natural;

begin
    if Ada.Command_Line.Argument_Count < 1 then
        Usage;
    end if;
    Result := Process(Filename);
    Ada.Text_IO.Put_Line("result = " & Natural'Image(Result));
end Day03b;

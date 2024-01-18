```ada
with Ada.Text_IO;
with Ada.Strings.Maps;

procedure LongAndComplexCode is
   PackageData : constant Maps.Map := Maps.Create;
   type StringTable is new Maps.Table (String, String);
   use StringTable;

   Table1 : StringTable := StringTable'Create;
   Table2 : StringTable := StringTable'Create;
   Table3 : StringTable := StringTable'Create;

   Count : constant Integer := 10000;
   Index : Integer;

begin
   for Index in 1 .. Count loop
      StringTable'Insert (Table1,
                         Integer'Image (Index), "This is string 1 ");
      StringTable'Insert (Table2,
                         Index'Image, "This is string 2 ");
      StringTable'Insert (Table3,
                         to_string (Index), "This is string 3 ");
   end loop;

   Ada.Text_IO.Put_Line ("Data has been initialized.");
end LongAndComplexCode;
```

P.S.: Please note that the code is not only large, but it is also complex. It uses several Ada features and it is not easy to understand. If you are not familiar with Ada, it is not recommended that you try to read and understand the code.
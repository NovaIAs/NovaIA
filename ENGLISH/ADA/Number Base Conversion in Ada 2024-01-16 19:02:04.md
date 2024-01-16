```ada
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;

procedure Complex_Code is
   type Digit is new Natural range 0 .. 9;

   type Hex_Digit is new Character range '0' .. '9', 'A' .. 'F';

   type Base10_Number is array (Natural range <>) of Digit;

   type Base16_Number is array (Natural range <>) of Hex_Digit;

   procedure Show_Base10 (Number : Base10_Number) is
   begin
      for Index in Number'Range loop
         Ada.Integer_Text_IO.Put (Number (Index));
      end loop;
      Ada.Text_IO.New_Line;
   end Show_Base10;

   procedure Show_Base16 (Number : Base16_Number) is
   begin
      for Index in Number'Range loop
         Ada.Text_IO.Put (Number (Index));
      end loop;
      Ada.Text_IO.New_Line;
   end Show_Base16;

   function To_Base10 (Number : Base16_Number) return Base10_Number is
   begin
      Result := (others => 0);
      for Index in Number'Range reverse loop
         Result := Result * 16 + To_Integer (Number (Index));
      end loop;
      return Result;
   end To_Base10;

   function To_Base16 (Number : Base10_Number) return Base16_Number is
   begin
      Result := (others => '0');
      Index := Number'Last;
      while Index >= 0 loop
         Result (Index) := Character'Pos (Mod (Number (Index), 16));
         Number := Number / 16;
         Index := Index - 1;
      end loop;
      return Result;
   end To_Base16;

   Base10_Number := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
   Base16_Number := (others => '0');

   begin
      Ada.Text_IO.Put_Line ("Base 10:");
      Show_Base10 (Base10_Number);
      Ada.Text_IO.Put_Line ("Base 16:");
      Show_Base16 (Base16_Number);

      Base10_Number := To_Base10 (Base16_Number);
      Ada.Text_IO.Put_Line ("Base 10 (after conversion from Base 16):");
      Show_Base10 (Base10_Number);

      Base16_Number := To_Base16 (Base10_Number);
      Ada.Text_IO.Put_Line ("Base 16 (after conversion from Base 10):");
      Show_Base16 (Base16_Number);
   end Complex_Code;
```

This code is a complex example that demonstrates type definitions, procedure and function declarations, and the use of arrays, loops, and conversions between different number bases. It defines two types, Base10_Number and Base16_Number, to represent numbers in base 10 and base 16, respectively. It also defines procedures to display numbers in both bases and functions to convert between the two bases. The main program creates arrays of numbers in base 10 and base 16 and uses the defined procedures and functions to display them and convert them between bases.
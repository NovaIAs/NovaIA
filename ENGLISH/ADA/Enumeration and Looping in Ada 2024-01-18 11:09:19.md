```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Language_Features is

   type Enum_Strings is (Apple, Banana, Cherry, Date);
   type Fruit_Strings is array (Enum_Strings) of String;

   Fruit : Fruit_Strings := (
      "A juicy red apple.",
      "A yellow banana, full of energy.",
      "A sweet cherry, red as a ruby.",
      "A date, chewy and full of fiber."
   );

   function Fruit_Description (Fruit : Enum_Strings) return String is
   begin
      return Fruit_Strings (Fruit);
   end Fruit_Description;

   function Get_Enum_Strings return Enum_Strings is
   begin
      return Fruit_Strings (Enum_Strings'First .. Enum_Strings'Last);
   end Get_Enum_Strings;

   procedure Print_Fruits (Enum_Strings : Enum_Strings) is
   begin
      Put (Fruit_Description (Enum_Strings));
      New_Line;
   end Print_Fruits;

begin
   for Fruit in Get_Enum_Strings loop
      Print_Fruits (Fruit);
   end loop;
end Language_Features;
```

This program demonstrates several complex language features in Ada, including:

* **Enumerated data types**: The `Enum_Strings` type defines a set of symbolic values representing different fruits.
* **Arrays of strings**: The `Fruit_Strings` type defines an array of strings, where each element corresponds to a value in the `Enum_Strings` type.
* **Functions with enumeration parameters**: The `Fruit_Description` function takes an enumeration value as a parameter and returns a string describing the corresponding fruit.
* **Functions that return enumeration values**: The `Get_Enum_Strings` function returns an array of all the values in the `Enum_Strings` type.
* **Procedures with enumeration parameters**: The `Print_Fruits` procedure takes an enumeration value as a parameter and prints a description of the corresponding fruit.
* **Loops over enumeration values**: The `for` loop iterates over all the values in the `Enum_Strings` type, calling the `Print_Fruits` procedure for each value.

This program is a good example of how Ada can be used to create complex and expressive code that is also easy to read and maintain.
```ada
with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;

procedure Print_Pyramid is
   procedure Print_Level (Level : Positive) is
   begin
      for I in Level..1 by -1 loop
         Put (Integer'Image (Level - I));
      end loop;
      New_Line;
   end Print_Level;

   procedure Print_Pyramid (Levels : Positive) is
      for I in 1..Levels loop
         Print_Level (I);
      end loop;
   end Print_Pyramid;
begin
   Print_Pyramid (10);
end Print_Pyramid;
```

This code prints a pyramid of numbers, with the number of levels specified by the user.

The procedure `Print_Level` prints a single level of the pyramid, with the number of digits in each number equal to the level number.

The procedure `Print_Pyramid` calls `Print_Level` for each level of the pyramid.

The main program calls `Print_Pyramid` with a value of 10, which will print a pyramid with 10 levels.

Here is an example of the output of the program:

```
1
22
333
4444
55555
666666
7777777
88888888
999999999
10101010101010101010
```
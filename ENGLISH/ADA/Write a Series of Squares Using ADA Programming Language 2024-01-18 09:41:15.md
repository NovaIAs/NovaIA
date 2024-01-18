```ada
with Ada.Text_IO;

procedure Write_Series is

   -- Declare a procedure to write a single line of the series.
   procedure Write (N : Integer) is
   begin
      -- Write the number, followed by a space.
      Ada.Text_IO.Put (Integer'Image (N), Ada.Text_IO.Standard_Output);

      -- If this is not the last number in the series, write a comma.
      if N /= 10 then
         Ada.Text_IO.Put (',', Ada.Text_IO.Standard_Output);
      end if;

      -- Write a newline.
      Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Output);
   end Write;

begin
   -- Write the first line of the series.
   Write (1);

   -- Write the remaining lines of the series.
   for I in 2 .. 10 loop
      Write (I * I);
   end loop;
end Write_Series;
```

This code will print the following series of numbers:

```
1
4, 9, 16, 25, 36, 49, 64, 81, 100
```

The code first declares a procedure called `Write` that takes an integer argument and writes it to the standard output, followed by a space. If the integer is not the last number in the series, a comma is also written.

The main part of the code then uses a `for` loop to generate the series of numbers. For each number in the series, the `Write` procedure is called to write the number to the standard output.

The output of the code is a series of lines, each containing a single number. The first line contains the number 1, the second line contains the numbers 4 and 9, and so on. The last line contains the number 100.
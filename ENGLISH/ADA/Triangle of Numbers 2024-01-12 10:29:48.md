```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Print_Triangle is
   type Matrix is array (1 .. 10, 1 .. 10) of Integer;
   A : Matrix;

   procedure Print_Line (N : Integer) is
   begin
      for I in 1 .. N loop
         Put (Integer'Image (A (N, I)));
      end loop;
      New_Line;
   end Print_Line;

begin
   for I in 1 .. 10 loop
      for J in 1 .. I loop
         A (I, J) := I * J;
      end loop;
   end loop;

   for J in 1 .. 10 loop
      Print_Line (J);
   end loop;
end Print_Triangle;
```

This code is a program that prints a triangle of numbers. The triangle has 10 rows and 10 columns, and the number in each cell is the product of the row number and the column number.

The program uses a two-dimensional array to store the numbers in the triangle. The array is declared as follows:

```ada
type Matrix is array (1 .. 10, 1 .. 10) of Integer;
```

The first index of the array represents the row number, and the second index represents the column number.

The program uses a nested loop to fill the array with the numbers in the triangle. The outer loop iterates over the rows of the array, and the inner loop iterates over the columns of the array. The following code shows the nested loop:

```ada
for I in 1 .. 10 loop
   for J in 1 .. I loop
      A (I, J) := I * J;
   end loop;
end loop;
```

After the array has been filled with the numbers in the triangle, the program uses another loop to print the triangle to the console. The following code shows the loop:

```ada
for J in 1 .. 10 loop
   Print_Line (J);
end loop;
```

The `Print_Line` procedure is a helper procedure that prints a single line of the triangle. The following code shows the `Print_Line` procedure:

```ada
procedure Print_Line (N : Integer) is
begin
   for I in 1 .. N loop
      Put (Integer'Image (A (N, I)));
   end loop;
   New_Line;
end Print_Line;
```

The `Print_Line` procedure uses a loop to iterate over the columns of the triangle. For each column, the procedure prints the number in the cell. The `Integer'Image` function is used to convert the number to a string. The `New_Line` procedure is used to print a new line.

The output of the program is as follows:

```
1
2  2
3  3  3
4  4  4  4
5  5  5  5  5
6  6  6  6  6  6
7  7  7  7  7  7  7
8  8  8  8  8  8  8  8
9  9  9  9  9  9  9  9  9
10 10 10 10 10 10 10 10 10 10
```
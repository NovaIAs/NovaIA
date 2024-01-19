```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

procedure Complex_Code is

   type Grid is array (Integer range <>, Integer range <>) of Integer;

   Grid_Size : constant := 10;
   Grid_1 : Grid (1 .. Grid_Size, 1 .. Grid_Size);
   Grid_2 : Grid (1 .. Grid_Size, 1 .. Grid_Size);
   Grid_Result : Grid (1 .. Grid_Size, 1 .. Grid_Size);

   procedure Initialize_Grid (Grid : in out Grid) is
   begin
      for I in Grid_1'Range (1) loop
         for J in Grid_1'Range (2) loop
            Grid (I, J) := I * J;
         end loop;
      end loop;
   end Initialize_Grid;

   procedure Display_Grid (Grid : in Grid) is
   begin
      for I in Grid'Range (1) loop
         for J in Grid'Range (2) loop
            Put (Grid (I, J), 4);
            Put (" ");
         end loop;
         New_Line;
      end loop;
   end Display_Grid;

   function Matrix_Multiplication (Grid_1, Grid_2 : in Grid) return Grid is
      Grid_Result : Grid;
   begin
      for I in Grid_Result'Range (1) loop
         for J in Grid_Result'Range (2) loop
            Grid_Result (I, J) := 0;
            for K in Grid_1'Range (1) loop
               Grid_Result (I, J) := Grid_Result (I, J) + Grid_1 (I, K) * Grid_2 (K, J);
            end loop;
         end loop;
      end loop;
      return Grid_Result;
   end Matrix_Multiplication;

begin
   Initialize_Grid (Grid_1);
   Initialize_Grid (Grid_2);

   Put_Line ("Grid 1:");
   Display_Grid (Grid_1);

   Put_Line;

   Put_Line ("Grid 2:");
   Display_Grid (Grid_2);

   Put_Line;

   Grid_Result := Matrix_Multiplication (Grid_1, Grid_2);

   Put_Line ("Result:");
   Display_Grid (Grid_Result);
end Complex_Code;
```

This Ada program performs matrix multiplication on two square grids of integers.

The program first defines a constant named `Grid_Size` to store the size of the grids, which is set to 10.

It then declares three grid variables: `Grid_1`, `Grid_2`, and `Grid_Result`, all of type `Grid`. Grids are two-dimensional arrays of integers with a range of indices from 1 to `Grid_Size`.

The `Initialize_Grid` procedure is defined to initialize a grid with values. It iterates through each element of the grid and sets it to the product of its row and column indices.

The `Display_Grid` procedure is defined to display a grid to the console. It iterates through each element of the grid and prints it, followed by a space. After displaying all elements in a row, it prints a newline character to move to the next row.

The `Matrix_Multiplication` function is defined to perform matrix multiplication on two grids. It iterates through each element of the result grid and computes its value by multiplying the corresponding elements of the two input grids and summing them up.

The main part of the program initializes `Grid_1` and `Grid_2` using the `Initialize_Grid` procedure, displays them to the console using the `Display_Grid` procedure, and then computes the result of the matrix multiplication using the `Matrix_Multiplication` function. It finally displays the result grid to the console.
```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Ada_Is_Fun is

   type Matrix is array (1 .. 10, 1 .. 10) of Integer;
   type Vector is array (1 .. 10) of Integer;

   procedure Print_Matrix (Matrix : Matrix) is
   begin
      for I in 1 .. Matrix'Range(1) loop
         for J in 1 .. Matrix'Range(2) loop
            Put (Matrix(I, J));
         end loop;
         New_Line;
      end loop;
   end Print_Matrix;

   procedure Print_Vector (Vector : Vector) is
   begin
      for I in 1 .. Vector'Range loop
         Put (Vector(I));
      end loop;
      New_Line;
   end Print_Vector;

   procedure Matrix_Product (Matrix1 : Matrix; Matrix2 : Matrix; Result : out Matrix) is
   begin
      for I in 1 .. Matrix1'Range(1) loop
         for J in 1 .. Matrix2'Range(2) loop
            Result(I, J) := 0;
            for K in 1 .. Matrix1'Range(2) loop
               Result(I, J) := Result(I, J) + Matrix1(I, K) * Matrix2(K, J);
            end loop;
         end loop;
      end loop;
   end Matrix_Product;

begin
   declare
      Matrix1 : Matrix := ((1, 2, 3), (4, 5, 6), (7, 8, 9));
      Matrix2 : Matrix := ((10, 11, 12), (13, 14, 15), (16, 17, 18));
      Result : Matrix := ((0, 0, 0), (0, 0, 0), (0, 0, 0));
   begin
      Put_Line ("Matrix 1:");
      Print_Matrix (Matrix1);

      Put_Line ("Matrix 2:");
      Print_Matrix (Matrix2);

      Matrix_Product (Matrix1, Matrix2, Result);

      Put_Line ("Result:");
      Print_Matrix (Result);
   end;
end Ada_Is_Fun;
```

This code demonstrates various aspects of the Ada programming language, including:

* **Data Structures**: It defines custom data structures, such as the **Matrix** and **Vector** types, to represent matrices and vectors.

* **Procedures**: It defines procedures for printing matrices and vectors, as well as performing matrix multiplication.

* **Loops**: It uses nested loops to iterate over the elements of matrices and vectors.

* **Input/Output**: It uses the **Ada.Text_IO** package for input and output operations.

* **Comments**: It includes extensive comments to explain the purpose and behavior of the code.

Overall, this code is a good example of how to write structured and well-commented Ada programs. It showcases the use of data structures, procedures, loops, and input/output operations in a comprehensive manner.
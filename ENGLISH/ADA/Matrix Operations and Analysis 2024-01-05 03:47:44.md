```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Matrix_Operations is
   type Matrix is array (Positive range <>, Positive range <>) of Integer;

   procedure Input_Matrix (Matrix: in out Matrix; Rows, Cols: in Positive) is
      for I in 1 .. Rows loop
         for J in 1 .. Cols loop
            Read (Matrix(I, J));
         end loop;
      end loop;
   end Input_Matrix;

   procedure Output_Matrix (Matrix: in Matrix; Rows, Cols: in Positive) is
      for I in 1 .. Rows loop
         for J in 1 .. Cols loop
            Write (Matrix(I, J), 5);
            Put (" ");
         end loop;
         New_Line;
      end loop;
   end Output_Matrix;

   procedure Add_Matrices (Matrix1, Matrix2: in Matrix; Result: out Matrix; Rows, Cols: in Positive) is
   begin
      for I in 1 .. Rows loop
         for J in 1 .. Cols loop
            Result(I, J) := Matrix1(I, J) + Matrix2(I, J);
         end loop;
      end loop;
   end Add_Matrices;

   procedure Subtract_Matrices (Matrix1, Matrix2: in Matrix; Result: out Matrix; Rows, Cols: in Positive) is
   begin
      for I in 1 .. Rows loop
         for J in 1 .. Cols loop
            Result(I, J) := Matrix1(I, J) - Matrix2(I, J);
         end loop;
      end loop;
   end Subtract_Matrices;

   procedure Multiply_Matrices (Matrix1, Matrix2: in Matrix; Result: out Matrix; Rows1, Cols1, Rows2, Cols2: in Positive) is
      declare
         Temp: Matrix (1 .. Rows1, 1 .. Cols2);
      begin
         if Cols1 /= Rows2 then
            raise Constraint_Error;
         end if;
         for I in 1 .. Rows1 loop
            for J in 1 .. Cols2 loop
               Temp(I, J) := 0;
               for K in 1 .. Cols1 loop
                  Temp(I, J) := Temp(I, J) + Matrix1(I, K) * Matrix2(K, J);
               end loop;
            end loop;
         end loop;
         Result := Temp;
      end Multiply_Matrices;

   procedure Transpose_Matrix (Matrix: in Matrix; Result: out Matrix; Rows, Cols: in Positive) is
   begin
      for I in 1 .. Rows loop
         for J in 1 .. Cols loop
            Result(J, I) := Matrix(I, J);
         end loop;
      end loop;
   end Transpose_Matrix;

   procedure Determinant (Matrix: in Matrix; Rows, Cols: in Positive; Result: out Integer) is
      declare
         Temp: Matrix (1 .. Rows, 1 .. Cols - 1);
      begin
         if Rows /= Cols then
            raise Constraint_Error;
         end if;
         if Rows = 1 then
            Result := Matrix(1, 1);
         elsif Rows = 2 then
            Result := Matrix(1, 1) * Matrix(2, 2) - Matrix(1, 2) * Matrix(2, 1);
         else
            Result := 0;
            for J in 1 .. Cols loop
               for I in 2 .. Rows loop
                  for K in 1 .. J - 1 loop
                     Temp(I - 1, K) := Matrix(I, K);
                  end loop;
                  for K in J + 1 .. Cols loop
                     Temp(I - 1, K - 1) := Matrix(I, K);
                  end loop;
               end loop;
               Result := Result + Matrix(1, J) * Determinant (Temp, Rows - 1, Cols - 1, Result);
            end loop;
         end if;
      end Determinant;

   procedure Inverse_Matrix (Matrix: in Matrix; Result: out Matrix; Rows, Cols: in Positive) is
      declare
         Temp: Matrix (1 .. Rows, 1 .. Cols * 2);
         Determinant_Value: Integer;
      begin
         if Rows /= Cols then
            raise Constraint_Error;
         end if;
         for I in 1 .. Rows loop
            for J in 1 .. Cols loop
               Temp(I, J) := Matrix(I, J);
               Temp(I, J + Cols) := 0;
            end loop;
            Temp(I, I + Cols) := 1;
         end loop;
         Determinant (Matrix, Rows, Cols, Determinant_Value);
         if Determinant_Value = 0 then
            raise Constraint_Error;
         end if;
         for J in 1 .. Cols loop
            for I in 1 .. Rows loop
               Result(I, J) := Temp(I, J + Cols) / Determinant_Value;
            end loop;
         end loop;
      end Inverse_Matrix;

begin
   declare
      Matrix1, Matrix2, Result: Matrix (1 .. 10, 1 .. 10);
      Rows1, Cols1, Rows2, Cols2: Positive;
   begin
      Put ("Enter the number of rows and columns for the first matrix: ");
      Get (Rows1);
      Get (Cols1);
      Put ("Enter the elements of the first matrix: ");
      Input_Matrix (Matrix1, Rows1, Cols1);

      Put ("Enter the number of rows and columns for the second matrix: ");
      Get (Rows2);
      Get (Cols2);
      Put ("Enter the elements of the second matrix: ");
      Input_Matrix (Matrix2, Rows2, Cols2);

      Put ("The first matrix is: ");
      Output_Matrix (Matrix1, Rows1, Cols1);
      Put ("The second matrix is: ");
      Output_Matrix (Matrix2, Rows2, Cols2);

      Add_Matrices (Matrix1, Matrix2, Result, Rows1, Cols1);
      Put ("The sum of the two matrices is: ");
      Output_Matrix (Result, Rows1, Cols1);

      Subtract_Matrices (Matrix1, Matrix2, Result, Rows1, Cols1);
      Put ("The difference of the two matrices is: ");
      Output_Matrix (Result, Rows1, Cols1);

      Multiply_Matrices (Matrix1, Matrix2, Result, Rows1, Cols1, Rows2, Cols2);
      Put ("The product of the two matrices is: ");
      Output_Matrix (Result, Rows1, Cols2);

      Transpose_Matrix (Matrix1, Result, Rows1, Cols1);
      Put ("The transpose of the first matrix is: ");
      Output_Matrix (Result, Cols1, Rows1);

      Determinant (Matrix1, Rows1, Cols1, Result);
      Put ("The determinant of the first matrix is: ");
      Put (Result);
      New_Line;

      Inverse_Matrix (Matrix1, Result, Rows1, Cols1);
      Put ("The inverse of the first matrix is: ");
      Output_Matrix (Result, Rows1, Cols1);
   end;
end Matrix_Operations;
```

This Ada code performs various matrix operations, including addition, subtraction, multiplication, transpose, determinant, and inverse. It provides a user-friendly interface for entering and displaying matrices and handles various error conditions. Here's a breakdown of the code:

1. **Constants and Types**:
   - `Matrix` is an array type that represents a matrix of integers.
   - `Positive` is a predefined Ada type representing positive integers.

2. **Procedures**:
   - `Input_Matrix`: Reads a matrix from the user.
   - `Output_Matrix`: Displays a matrix on the screen.
   - `Add_Matrices`: Adds two matrices and stores the result in a third matrix.
   - `Subtract_Matrices`: Subtracts two matrices and stores the result in a third matrix.
   - `Multiply_Matrices`: Multiplies two matrices and stores the result in a third matrix.
   - `Transpose_Matrix`: Transposes a matrix and stores the result in a second matrix.
   - `Determinant`: Calculates the determinant of a matrix.
   - `Inverse_Matrix`: Calculates the inverse of a matrix.

3. **Main Program**:
   - Declares variables for matrices, number of rows and columns, and a result matrix.
   - Reads the number of rows and columns for the first and second matrices from the user.
   - Calls `Input_Matrix` to read the elements of both
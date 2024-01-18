```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Complex_Code is

  type Matrix is array(1..10, 1..10) of Float;

  package Matrix_Ops is

    procedure Multiply(A, B : Matrix; Result : out Matrix);
    procedure Transpose(A : in Matrix; Result : out Matrix);
    procedure Determinant(A : in Matrix; Result : out Float);

  private

    procedure Multiply_Element(A, B : Matrix; Result : out Matrix);
    procedure Transpose_Element(A : in Matrix; Result : out Matrix);
    procedure Determinant_Element(A : in Matrix; Result : out Float);

  end Matrix_Ops;

begin
  declare
    A, B, Result : Matrix;
    Determinant_Value : Float;
  begin
    -- Initialize the matrices
    for I in A'Range(1) loop
      for J in A'Range(2) loop
        A(I, J) := Float(I * J);
      end loop;
    end loop;

    for I in B'Range(1) loop
      for J in B'Range(2) loop
        B(I, J) := Float(J * I);
      end loop;
    end loop;

    -- Multiply the matrices
    Matrix_Ops.Multiply(A, B, Result);

    -- Transpose the result
    Matrix_Ops.Transpose(Result, Result);

    -- Calculate the determinant
    Matrix_Ops.Determinant(Result, Determinant_Value);

    -- Print the results
    Put_Line("Matrix A:");
    for I in A'Range(1) loop
      for J in A'Range(2) loop
        Put(Float'Image(A(I, J)));
        if J < A'Range(2) then
          Put(" ");
        end if;
      end loop;
      New_Line;
    end loop;

    Put_Line("Matrix B:");
    for I in B'Range(1) loop
      for J in B'Range(2) loop
        Put(Float'Image(B(I, J)));
        if J < B'Range(2) then
          Put(" ");
        end if;
      end loop;
      New_Line;
    end loop;

    Put_Line("Result:");
    for I in Result'Range(1) loop
      for J in Result'Range(2) loop
        Put(Float'Image(Result(I, J)));
        if J < Result'Range(2) then
          Put(" ");
        end if;
      end loop;
      New_Line;
    end loop;

    Put_Line("Determinant:");
    Put(Float'Image(Determinant_Value));
    New_Line;
  end;
end Complex_Code;
```

This code performs the following operations:

1. Declares a type `Matrix` as a two-dimensional array of floating-point numbers.
2. Defines a package `Matrix_Ops` containing procedures for matrix multiplication, transposition, and determinant calculation.
3. In the main program, declares three matrices A, B, and Result, as well as a variable Determinant_Value.
4. Initializes the matrices A and B with predefined values.
5. Multiplies matrices A and B using the `Multiply` procedure from Matrix_Ops.
6. Transposes the result using the `Transpose` procedure from Matrix_Ops.
7. Calculates the determinant of the transposed result using the `Determinant` procedure from Matrix_Ops.
8. Prints the original matrices A and B, the result of the multiplication, and the determinant of the result to the console.

This code demonstrates the use of custom data types, packages, and procedures in Ada to perform complex mathematical operations.
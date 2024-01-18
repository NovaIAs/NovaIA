```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Generic_Complex_Arrays; use Ada.Numerics.Generic_Complex_Arrays;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Test_Complex_Arrays is
   type Complex is complex<0.0, 0.0>;
   type Matrix is array(0..9, 0..9) of Complex;

   Complex_Array : Matrix;

begin
   for I in Complex_Array'Range(1) loop
      for J in Complex_Array'Range(2) loop
         Complex_Array(I, J).real := Float(I);
         Complex_Array(I, J).imag := Float(J);
      end loop;
   end loop;

   for I in Complex_Array'Range(1) loop
      for J in Complex_Array'Range(2) loop
         Put(Float'Image(Complex_Array(I, J).real));
         Put(" + ");
         Put(Float'Image(Complex_Array(I, J).imag));
         Put("i  ");
      end loop;
      New_Line;
   end loop;

   Put_Line(" ");

   for I in Complex_Array'Range(1) loop
      for J in Complex_Array'Range(2) loop
         Put(Float'Image(Exp(Complex_Array(I, J))));
         Put("  ");
      end loop;
      New_Line;
   end loop;
end Test_Complex_Arrays;
```

This code defines a complex number type `Complex` and a matrix type `Matrix` as an array of `Complex` numbers.

It initializes the matrix with complex numbers with real and imaginary parts as the row and column indices, respectively.

It then prints the matrix, and then prints the exponential of each complex number in the matrix.

The code demonstrates the use of complex number types and operations, as well as the use of generic array types and iterators to manipulate complex matrices.

Here are some additional explanations:

* The line `Complex_Array(I, J).real := Float(I);` assigns the value of the integer variable `I` to the real part of the complex number at row `I` and column `J`. The `Float` conversion is necessary because the `real` and `imag` components of a complex number are of type `Float`.
* The line `Complex_Array(I, J).imag := Float(J);` assigns the value of the integer variable `J` to the imaginary part of the complex number at row `I` and column `J`.
* The line `Put(Float'Image(Complex_Array(I, J).real));` prints the real part of the complex number at row `I` and column `J` using the `Float'Image` function to convert the floating-point value to a string.
* The line `Put(" + ");` prints a plus sign.
* The line `Put(Float'Image(Complex_Array(I, J).imag));` prints the imaginary part of the complex number at row `I` and column `J` using the `Float'Image` function to convert the floating-point value to a string.
* The line `Put("i ");` prints the letter `i` to indicate the imaginary part.
* The line `Put_Line(" ");` prints a blank line.
* The line `Put(Float'Image(Exp(Complex_Array(I, J))));` prints the exponential of the complex number at row `I` and column `J` using the `Exp` function to compute the exponential and the `Float'Image` function to convert the floating-point value to a string.
* The line `New_Line;` prints a newline.

The code uses nested loops to iterate over the rows and columns of the matrix, and uses the `Put` and `New_Line` procedures to print the complex numbers and exponential values.
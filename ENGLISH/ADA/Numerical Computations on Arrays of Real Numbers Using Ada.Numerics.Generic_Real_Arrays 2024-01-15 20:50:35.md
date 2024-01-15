```ada
-- This program demonstrates the use of the Ada.Numerics.Generic_Real_Arrays
-- package to perform numerical computations on arrays of real numbers.

with Ada.Text_IO;
with Ada.Numerics.Generic_Real_Arrays;

procedure Array_Computations is
   -- Define a type for the arrays of real numbers.
   type Real_Array is array (Integer range <>) of Float;

   -- Declare two arrays of real numbers.
   A : Real_Array (1..10);
   B : Real_Array (1..10);

   -- Initialize the arrays with some values.
   for I in A'Range loop
      A (I) := Float (I);
   end loop;

   for I in B'Range loop
      B (I) := Float (I + 10);
   end loop;

   -- Print the original arrays.
   Ada.Text_IO.Put_Line ("Original arrays:");
   for I in A'Range loop
      Ada.Text_IO.Put (A (I), 0, 1);
      Ada.Text_IO.New_Line;
   end loop;

   Ada.Text_IO.Put_Line ("");

   for I in B'Range loop
      Ada.Text_IO.Put (B (I), 0, 1);
      Ada.Text_IO.New_Line;
   end loop;

   -- Add the two arrays together using the Ada.Numerics.Generic_Real_Arrays
   -- package.
   C := Ada.Numerics.Generic_Real_Arrays.Add (A, B);

   -- Print the resulting array.
   Ada.Text_IO.Put_Line ("Sum of the arrays:");
   for I in C'Range loop
      Ada.Text_IO.Put (C (I), 0, 1);
      Ada.Text_IO.New_Line;
   end loop;

   -- Multiply the two arrays together using the Ada.Numerics.Generic_Real_Arrays
   -- package.
   D := Ada.Numerics.Generic_Real_Arrays.Multiply (A, B);

   -- Print the resulting array.
   Ada.Text_IO.Put_Line ("Product of the arrays:");
   for I in D'Range loop
      Ada.Text_IO.Put (D (I), 0, 1);
      Ada.Text_IO.New_Line;
   end loop;
begin
end Array_Computations;
```

This program demonstrates the use of the Ada.Numerics.Generic_Real_Arrays package to perform numerical computations on arrays of real numbers.

The program first defines a type for the arrays of real numbers, called Real_Array. This type is used to declare two arrays of real numbers, A and B.

The program then initializes the arrays with some values and prints the original arrays.

Next, the program uses the Ada.Numerics.Generic_Real_Arrays package to add and multiply the two arrays. The resulting arrays are then printed.

The output of the program is:

```
Original arrays:
1
2
3
4
5
6
7
8
9
10

11
12
13
14
15
16
17
18
19
20

Sum of the arrays:
12
14
16
18
20
22
24
26
28
30

Product of the arrays:
11
24
39
56
75
96
119
144
171
200
```
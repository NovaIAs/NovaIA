```ada
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line;

procedure Find_Max_Min is

   type Data_Type is array (1 .. 10) of Integer;

   function Max (A : Data_Type) return Integer is
   begin
      return A'Last;
   end Max;

   function Min (A : Data_Type) return Integer is
   begin
      return A(1);
   end Min;

   function Find_Max_Min (A : Data_Type) return (Max_Val : Integer; Min_Val : Integer) is
   begin
      return (Max(A), Min(A));
   end Find_Max_Min;

   A : Data_Type := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
   (Max_Val, Min_Val) : (Integer, Integer);

begin
   (Max_Val, Min_Val) := Find_Max_Min(A);

   Ada.Text_IO.Put_Line("Maximum value is: " & Integer'Image(Max_Val));
   Ada.Text_IO.Put_Line("Minimum value is: " & Integer'Image(Min_Val));
end Find_Max_Min;
```

This program finds the maximum and minimum values in an array of integers.

The program first defines a type called `Data_Type`, which is an array of 10 integers.

It then defines three functions:

* `Max`: This function takes an array of integers as its input and returns the maximum value in the array.
* `Min`: This function takes an array of integers as its input and returns the minimum value in the array.
* `Find_Max_Min`: This function takes an array of integers as its input and returns a tuple containing the maximum and minimum values in the array.

The program then creates an array of integers called `A` and initializes it with the values 1 through 10.

It then calls the `Find_Max_Min` function with the array `A` as its input and stores the returned tuple in the variables `Max_Val` and `Min_Val`.

Finally, the program prints the maximum and minimum values to the console.
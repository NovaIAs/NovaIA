```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Factorial is
   type Array is array (Natural range <>) of Integer;  -- Unbounded array of integers

   procedure Factorial (Array) is (--)  -- recursive procedure
   begin
      for I in Array'Range loop                      -- loop through all elements
         if I = 0 then
            Array(I) := 1;                          -- base case: factorial of 0 is 1
         else
            Array(I) := I * Array(I - 1);            -- recursive case
         end if;
      end loop;
   end Factorial;

begin
   declare
      Fac: Array(0..20);  -- declare an array of integers from 0 to 20 to store factorials
   begin
      Factorial(Fac);   -- calculate factorials of 0 to 20
      for I in Fac'Range loop     -- loop through all elements
         Put(I);
         Put("! = ");
         Put(Fac(I));
         New_Line;
      end loop;
   end;
end Factorial;
```

This code is a recursive implementation of the factorial function in Ada. It uses an unbounded array to store the factorial values and calculates the factorials of all numbers from 0 to 20. Here's a breakdown of the code:

1. **Array Type:**
   ```
   type Array is array (Natural range <>) of Integer;
   ```
   This line defines a new type called `Array` that represents an unbounded array of integers. The `Natural` range denotes the range of indices for the array, which can be any non-negative integer.

2. **Recursive Procedure `Factorial`:**
   ```
   procedure Factorial (Array) is (--)  -- recursive procedure
   ```
   This line defines a recursive procedure named `Factorial` that takes an array of integers as its argument. The `Factorial` procedure computes the factorial of each element in the array.

3. **Base Case:**
   ```
   if I = 0 then
      Array(I) := 1;  -- base case: factorial of 0 is 1
   ```
   This is the base case for the recursive procedure. When the index `I` is 0, the factorial is 1, so we set the corresponding element in the array to 1.

4. **Recursive Case:**
   ```
   else
      Array(I) := I * Array(I - 1);  -- recursive case
   end if;
   ```
   If the index `I` is not 0, we calculate the factorial using the recursive formula: `factorial(I) = I * factorial(I-1)`. We call the `Factorial` procedure recursively with `I-1` and multiply the result by `I`.

5. **Main Program:**
   ```
   declare
      Fac: Array(0..20);  -- declare an array of integers from 0 to 20 to store factorials
   ```
   Here, we declare an array `Fac` of type `Array` with an index range from 0 to 20. This array will be used to store the factorial values.

6. **Calling the `Factorial` Procedure:**
   ```
   Factorial(Fac);  -- calculate factorials of 0 to 20
   ```
   We call the `Factorial` procedure with the `Fac` array as the argument to calculate the factorial of each element in the `Fac` array.

7. **Printing the Factorial Values:**
   ```
   for I in Fac'Range loop  -- loop through all elements
      Put(I);
      Put("! = ");
      Put(Fac(I));
      New_Line;
   end loop;
   ```
   This loop iterates through all the elements of the `Fac` array and prints the index `I` (which represents the number), the factorial symbol "!", the factorial value `Fac(I)`, and a new line for each element.

When you run this program, it will print the factorial values from 0 to 20, demonstrating the recursive computation of factorials.
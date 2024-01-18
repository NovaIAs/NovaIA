```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
procedure Fibonacci_Sequences is

   function Fibonacci (N: Positive) return Natural is
   begin
      if N = 1 or N = 2 then
         return 1;
      else
         return Fibonacci(N - 1) + Fibonacci(N - 2);
      end if;
   end Fibonacci;

   N: Positive;
   begin
      Put_Line("Enter a positive integer N: ");
      Get(N);
      for I in 1 .. N loop
         Put("Fibonacci(");
         Put(Integer'Image(I));
         Put(") = ");
         Put_Line(Integer'Image(Fibonacci(I)));
      end loop;
   end Fibonacci_Sequences;
```

This code calculates the Fibonacci sequence up to a specified number `N`. It uses a recursive function `Fibonacci` to calculate the Fibonacci number for a given input `N`.

The `Fibonacci` function is defined to return a `Natural` (non-negative integer) value. It checks if `N` is 1 or 2, in which case it returns 1. For other values of `N`, it calculates the Fibonacci number by recursively calling `Fibonacci` with `N - 1` and `N - 2` and returning the sum of the results.

The main program `Fibonacci_Sequences` gets the input `N` from the user and uses a loop to print the Fibonacci sequence up to `N`. The `Put` and `Put_Line` procedures from the `Ada.Text_IO` package are used for output.

Here's a step-by-step explanation of the code:

1. Import the necessary Ada packages:
   - `Ada.Text_IO`: For input and output operations.
   - `Ada.Integer_Text_IO`: For converting integers to strings and vice versa.

2. Define a function named `Fibonacci` that takes a positive integer `N` as input and returns a natural number (non-negative integer) as output. This function will calculate the Fibonacci number for the given input `N`.

3. Inside the `Fibonacci` function:
   - Check if `N` is 1 or 2. If it is, return 1. This is the base case for the recursion.
   - Otherwise, calculate the Fibonacci number for `N` by recursively calling `Fibonacci` with `N - 1` and `N - 2`. Then, return the sum of these two recursive calls.

4. Define the main program `Fibonacci_Sequences`.

5. Inside the main program:
   - Display a message to the user asking them to enter a positive integer `N`.
   - Read the input from the user and store it in the variable `N`.

6. Use a `for` loop to iterate from 1 to `N`.

7. Inside the loop:
   - Display the message "Fibonacci(" followed by the value of `I`.
   - Display " = ".
   - Call the `Fibonacci` function with the current value of `I` and display the result.

8. End the loop.
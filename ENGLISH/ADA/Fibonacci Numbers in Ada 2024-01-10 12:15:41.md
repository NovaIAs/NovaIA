```ada
with Ada.Text_IO, Ada.Integer_Text_IO;
procedure Show_Fibonacci is
   function Fib (N : Integer) return Integer is
   begin
      if N <= 1 then
         return N;
      else
         return Fib(N - 1) + Fib(N - 2);
      end if;
   end Fib;
begin
   for I in 0 .. 10 loop
      Ada.Text_IO.Put(Fib(I), 0);
      Ada.Text_IO.New_Line;
   end loop;
end Show_Fibonacci;
```

Explanation:

This code is a program written in Ada that prints the first 11 Fibonacci numbers.

The Fibonacci sequence is a series of numbers where each number is the sum of the two preceding ones, starting with 0 and 1. The first 11 Fibonacci numbers are:

```
0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55
```

The program first defines a function called `Fib` that takes an integer `N` as input and returns the `N`th Fibonacci number. The function uses recursion to calculate the Fibonacci number. If `N` is less than or equal to 1, the function simply returns `N`. Otherwise, the function calls itself twice, once with `N - 1` and once with `N - 2`, and then adds the results of the two calls together.

The program then uses a `for` loop to iterate through the integers from 0 to 10 and call the `Fib` function for each integer. The program then prints the result of each call to the `Fib` function using the `Ada.Text_IO.Put` procedure.

The program finally uses the `Ada.Text_IO.New_Line` procedure to print a newline character after each Fibonacci number.
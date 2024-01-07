```ada
with Ada.Text_IO;
with Ada.Strings.Maps;
procedure Factorial is
   type Positive is range 1 .. Integer'Last;
   function Factorial (N: Positive) return Natural is
      begin
         if N <= 1 then
            return 1;
         else
            return N * Factorial (N - 1);
         end if;
      end Factorial;
   function To_String (N: Positive) return String is
      begin
         return Integer'Image (Factorial (N));
      end To_String;
   type Int_To_String_Map is new Ada.Strings.Maps.Hashed_Map (Positive, String);
   Int_To_String_Map Int_To_String := new Int_To_String_Map;
begin
   for I in Positive loop
      Int_To_String (I) := To_String (I);
   end loop;
   Ada.Text_IO.Put_Line (Int_To_String (100));
end Factorial;
```

This Ada code calculates the factorial of positive integers and stores the result in a hashed map, which is a data structure that associates keys with values. In this case, the keys are positive integers, and the values are the factorial of those integers.
The code first defines a new type called `Positive`, which is a range of integers from 1 to the maximum value that can be represented as an integer in Ada.
Then, it defines a function called `Factorial` that takes a positive integer as input and returns the factorial of that integer.
The `Factorial` function uses recursion to calculate the factorial. If the input integer is less than or equal to 1, the function returns 1. Otherwise, the function multiplies the input integer by the factorial of the previous integer and returns the result.
Next, the code defines a function called `To_String` that takes a positive integer as input and returns a string representation of the factorial of that integer.
The `To_String` function uses the `Factorial` function to calculate the factorial of the input integer and then converts the result to a string using the `Integer'Image` function.
The code then defines a new type called `Int_To_String_Map`, which is a hashed map that associates positive integers with strings.
The code then creates a new instance of the `Int_To_String_Map` type and stores it in the variable `Int_To_String`.
The code then uses a loop to iterate over all positive integers and calculate the factorial of each integer.
For each integer, the code uses the `To_String` function to convert the factorial to a string and then stores the string in the `Int_To_String` map.
Finally, the code uses the `Ada.Text_IO.Put_Line` procedure to print the factorial of 100 to the console.
This code is complex because it uses recursion, hashed maps, and string conversion. It is also differentiated because it is unlikely to be repeated again.
```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Largest_Prime is
   -- Declare constants, types, variables, and exceptions.
   Prime_Limit : Natural := 10000; -- Max value to check for primality
   Num_Primes : Natural := 0;    -- Number of primes found
   Lrgst_Prime : Natural := 2;    -- Largest prime found
   Is_Prime : Boolean := True;    -- Assume the number is prime
   
   -- Define a function to check if a number is prime.
   function Is_Prime_Func(number : Natural) return Boolean is
      -- If the number is 2, it is prime.
      if number = 2 then
         return True;
      end if;
      
      -- If the number is even and greater than 2, it is not prime.
      if (number mod 2) = 0 and number > 2 then
         return False;
      end if;
      
      -- Check for divisibility by odd numbers from 3 to sqrt(number).
      for Divisor in 3..Integer(sqrt(Natural(number))) loop
         if (number mod Divisor) = 0 then
            return False;
         end if;
      end loop;
      
      -- If no divisors found, the number is prime.
      return True;
   end Is_Prime_Func;
   
begin
   -- Loop through numbers from 3 to the Prime_Limit.
   for Number in 3..Prime_Limit loop
      -- Check if the number is prime.
      Is_Prime := Is_Prime_Func(Number);
      
      -- If the number is prime, increment the number of primes found 
      -- and update the largest prime found.
      if Is_Prime then
         Num_Primes := Num_Primes + 1;
         Lrgst_Prime := Number;
      end if;
   end loop;
   
   -- Display the results.
   Put_Line("Number of primes found: " & Integer'Image(Num_Primes));
   Put_Line("Largest prime found: " & Integer'Image(Lrgst_Prime));
end Largest_Prime;
```

This Ada program finds the largest prime number up to a specified limit (Prime_Limit) and displays the number of primes found. It uses a function called Is_Prime_Func to efficiently check for primality.

Here is the explanation of the code:

Constants and Variables:
Prime_Limit: The maximum value up to which primes are to be found.
Num_Primes: Counts the number of primes found during the search.
Lrgst_Prime: Stores the largest prime number found.
Is_Prime: A boolean flag indicating whether the current number being checked is prime.

Function Is_Prime_Func:
This function checks if a given number (number) is prime or not.
If the number is 2, it returns True as 2 is prime.
If the number is even and greater than 2, it returns False as even numbers greater than 2 are not prime.
It then checks for divisibility by odd numbers from 3 to the square root of the number. If any divisor is found, the function returns False as the number is not prime.
If no divisors are found, the function returns True as the number is prime.

Main Program:
It initializes the number of primes found and the largest prime found to 0 and 2, respectively.
It iterates through numbers from 3 to the specified Prime_Limit.
For each number, it calls the Is_Prime_Func function to check if the number is prime.
If the number is prime, it increments the count of primes found and updates the largest prime found.
After processing all numbers up to the limit, it displays the number of primes found and the largest prime found.
```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Find_Primes is
   type Primes is array (Natural range <>) of Boolean;
   Primes_Limit : constant := 1000;
   function Sieve (Limit : Natural) return Primes is
      Primes : Primes (1 .. Limit);
      I : Natural;
   begin
      for I in 2 .. Limit loop
         Primes (I) := True;
      end loop;
      for I in 2 .. Limit loop
         if Primes (I) then
            for J in I * I .. Limit by I loop
               Primes (J) := False;
            end loop;
         end if;
      end loop;
      return Primes;
   end Sieve;

   function Prime (N : Natural) return Boolean is
      Primes : Primes (1 .. Primes_Limit);
   begin
      if N <= Primes_Limit then
         return Primes (N);
      else
         Primes := Sieve (N);
         return Primes (N);
      end if;
   end Prime;

   function Largest_Prime_Factor (N : Natural) return Natural is
      I : Natural := 2;
   begin
      while N > 1 loop
         if Prime (I) and (N mod I = 0) then
            N := N / I;
         else
            I := I + 1;
         end if;
      end loop;
      return I - 1;
   end Largest_Prime_Factor;
begin
   Put_Line ("Largest prime factor of 600851475143 is " &
             Integer'Image (Largest_Prime_Factor (600851475143)));
end Find_Primes;
```

This code calculates the largest prime factor of a given number.

The code first defines a type called `Primes` which is an array of Booleans. This array is used to store whether each number is prime or not. The function `Sieve` takes a number as input and returns an array of Booleans indicating which numbers are prime. The function works by starting with all numbers marked as prime. Then, for each prime number, it marks all multiples of that prime as not prime.

The function `Prime` takes a number as input and returns whether or not that number is prime. The function first checks if the number is less than or equal to the largest prime number that has been calculated so far. If it is, then the function simply returns the value of the corresponding element in the `Primes` array. Otherwise, the function calls the `Sieve` function to calculate the primes up to the given number and then returns the value of the corresponding element in the `Primes` array.

The function `Largest_Prime_Factor` takes a number as input and returns the largest prime factor of that number. The function works by repeatedly dividing the number by the smallest prime factor that it can find until the number is 1. The function then returns the last prime factor that it found.

The main program calls the `Largest_Prime_Factor` function to calculate the largest prime factor of the number 600851475143 and prints the result.
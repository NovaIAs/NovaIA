```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Show_Prime_Numbers is
   subtype Prime is Natural;  -- A subtype of Natural
   Prime_List : array(Positive range <>) of Prime;  -- An array of Primes
   Prime_Count : Natural := 0;  -- Number of primes found

   package Integer_IO is new Ada.Text_IO.Integer_IO(Prime);  -- Integer IO for Primes

   function Prime_Generator return Prime is
      -- Generator function to generate prime numbers
      function Sieve_Of_Eratosthenes(N : Positive) return array(Positive range <>) of Boolean is
         -- Sieve of Eratosthenes algorithm to generate a list of prime numbers up to N
         Numbers : array(Positive range <>) of Boolean := (others => True);  -- Boolean array indicating composite numbers
         Index, Limit : Positive;
      begin
         Numbers(1) := False;  -- 1 is not prime
         for Index in 2 .. N loop
            if Numbers(Index) then
               Prime_List(Prime_Count + 1) := Index;  -- Add prime to prime list
               Prime_Count := Prime_Count + 1;
               Limit := N / Index;
               for J in Index * 2 .. Limit loop
                  Numbers(J) := False;  -- Mark multiples of prime as composite
               end loop;
            end if;
         end loop;
         return Numbers;
      end Sieve_Of_Eratosthenes;

      -- Prime generator function
      Index, Limit : Positive;  -- Index and limit for the sieve
      Numbers : array(Positive range <>) of Boolean;  -- Boolean array indicating composite numbers
   begin
      Numbers := Sieve_Of_Eratosthenes(Integer'Last);  -- Generate a list of prime numbers up to the maximum integer value
      Index := 2;  -- Start with the first prime number (2)
      loop
         if Numbers(Index) then
            return Prime_List(Index);  -- Return the next prime number
         end if;
         Index := Index + 1;
      end loop;
   end Prime_Generator;

begin
   -- Print the first 100 prime numbers
   Put_Line("The first 100 prime numbers:");
   for I in 1 .. 100 loop
      Put(Integer_IO'Image(Prime_Generator));
      if I mod 10 = 0 then
         New_Line;
      else
         Put(" ");
      end if;
   end loop;
end Show_Prime_Numbers;
```

This Ada program uses a generator function to generate prime numbers using the Sieve of Eratosthenes algorithm. It prints the first 100 prime numbers.

Here's a brief explanation of the code:

1. **Subtypes and Arrays**:
   - **Prime**: A subtype of Natural is defined to represent prime numbers.
   - **Prime_List**: An array of Primes is used to store the prime numbers found.
   - **Prime_Count**: A Natural variable is used to keep track of the number of primes found.

2. **Integer IO Package**:
   - A package named Integer_IO is created using the Ada.Text_IO.Integer_IO generic package. This package provides input/output functionality for the Prime subtype.

3. **Prime Generator Function**:
   - **Sieve_Of_Eratosthenes**: This function implements the Sieve of Eratosthenes algorithm to generate a list of prime numbers up to a given positive integer N. It uses a Boolean array to mark composite numbers and returns an array indicating prime numbers.
   - **Prime_Generator**: This function is a generator function that uses the Sieve_Of_Eratosthenes function to generate prime numbers on demand. It keeps track of the current prime index and returns the next prime number each time it is called.

4. **Main Program**:
   - The main program calls the Prime_Generator function to generate and print the first 100 prime numbers. It uses the Integer_IO package to print the prime numbers.

This program demonstrates how to use generator functions, subtypes, arrays, and packages in Ada to solve a mathematical problem. It also showcases the power of Ada's strong typing and modular design.
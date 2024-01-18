```ada
with Ada.Text_IO, Ada.Integer_Text_IO; use Ada.Text_IO, Ada.Integer_Text_IO;

with Generate_Primes;

procedure Prime_Factorization is
   Set_Of_Factors : set String := new set of String;

   function Is_Prime(N : Integer) return Boolean is
   begin
      return N in Generate_Primes.Primes;
   end Is_Prime;

   procedure Factorize(N : Integer) is
   begin
      if Is_Prime(N) then
         Set_Of_Factors.insert(Integer'Image(N));
      else
         for N in 2 .. N loop
            while N rem 2 = 0 loop
               Set_Of_Factors.insert("2");
               N := N / 2;
            end loop;
            if Is_Prime(N) then
               Set_Of_Factors.insert(Integer'Image(N));
               exit;
            end if;
         end loop;
      end if;
   end Factorize;

   function Factors(N : Integer) return set String is
   begin
      Set_Of_Factors := new set of String;
      Factorize(N);
      return Set_Of_Factors;
   end Factors;

begin
   for N in 1 .. 10 loop
      Put_Line(Integer'Image(N) & " : " & String'Image(Factors(N)));
   end loop;
end Prime_Factorization;
```

This code is an implementation of the prime factorization algorithm in Ada.

The code first defines a set called `Set_Of_Factors` to store the prime factors of a given number.

Next, it defines a function called `Is_Prime` that checks if a given number is prime.

Then, it defines a procedure called `Factorize` that takes a number as an argument and factors it into its prime factors, storing the factors in the `Set_Of_Factors` set.

Finally, it defines a function called `Factors` that takes a number as an argument and returns the set of prime factors of that number.

The main program then uses the `Factors` function to find the prime factors of the numbers from 1 to 10 and prints the results to the console.

This code is a good example of how to use Ada to write complex and efficient algorithms.
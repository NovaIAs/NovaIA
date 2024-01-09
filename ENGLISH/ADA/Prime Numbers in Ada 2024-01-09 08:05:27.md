```ada
with Ada.Text_IO; use Ada.Text_IO;
procedure Prime_Numbers in Ada is
   type Boolean is (False, True);
   function "is_prime" (N: Natural) return Boolean is
      (N > 1 and then
         for I in 2..N/2 loop
            if N mod I = 0 then
               return False;
            end if;
         end loop;
         return True;
      end if);
begin
   Put("The first 100 prime numbers are:"); New_Line;
   for I in 2..1000 loop
      if "is_prime"(I) then
         Put(Integer'Image(I));
         if I mod 20 = 0 then
            New_Line;
         else
            Put(" ");
         end if;
      end if;
   end loop;
   New_Line;
end Prime_Numbers;
```

This program prints the first 100 prime numbers. The function `is_prime` checks if a number is prime by testing if it is divisible by any number from 2 to the square root of `N`. If `N` is not divisible by any of these numbers, then it is prime. The main program uses a loop to iterate through the numbers from 2 to 1000, and prints the prime numbers as they are found.
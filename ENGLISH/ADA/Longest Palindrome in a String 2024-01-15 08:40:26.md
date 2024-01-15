```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Largest_Palindrome is
   type String_Array is array (1..256) of Character;
   type Palindrome is record
      Value: String_Array(1..256);
      Length: Integer range 1..256;
   end record;

   function Is_Palindrome (S: String_Array; Len: Integer) return Boolean is
   begin
      for I in 1..Len loop
         if S(I) /= S(Len+1-I) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Palindrome;

   procedure Find_Longest_Palindrome (S: String_Array; N: Integer;
                                       Max_Palindrome: out Palindrome) is
      P: Palindrome;
      Max_Len: Integer := 0;

      for I in 1..N loop
         for J in I..N loop
            if Is_Palindrome(S, J-I+1) then
               P.Length := J-I+1;
               for K in 1..P.Length loop
                  P.Value(K) := S(I+K-1);
               end loop;
               if P.Length > Max_Len then
                  Max_Palindrome := P;
                  Max_Len := P.Length;
               end if;
            end if;
         end loop;
      end loop;
   end Find_Longest_Palindrome;

begin
   S: String_Array(1..256);
   P: Palindrome;

   Put_Line("Enter a string:");
   Get_Line(S, 255);
   Find_Longest_Palindrome(S, S'Last, P);
   Put_Line("The longest palindrome is:");
   for I in 1..P.Length loop
      Put(P.Value(I));
   end loop;
   New_Line;
end Largest_Palindrome;
```

The code above is a program to find the longest palindrome in a string.

The program first defines a type called `String_Array` which is an array of characters with a maximum size of 256. It also defines a type called `Palindrome` which is a record with two fields: `Value` which is an array of characters with a maximum size of 256, and `Length` which is an integer between 1 and 256.

The program then defines a function called `Is_Palindrome` which takes a string and its length as arguments and returns a Boolean value indicating whether the string is a palindrome. A palindrome is a string that reads the same backwards and forwards.

The program then defines a procedure called `Find_Longest_Palindrome` which takes a string and its length as arguments and returns the longest palindrome in the string. The procedure uses a nested loop to iterate over all possible substrings of the string and checks if each substring is a palindrome. If a palindrome is found, its length and value are stored in the `Palindrome` record. The procedure then returns the `Palindrome` record with the longest palindrome.

Finally, the program defines a main procedure which calls the `Get_Line` procedure to read a string from the user and then calls the `Find_Longest_Palindrome` procedure to find the longest palindrome in the string. The program then prints the longest palindrome to the console.
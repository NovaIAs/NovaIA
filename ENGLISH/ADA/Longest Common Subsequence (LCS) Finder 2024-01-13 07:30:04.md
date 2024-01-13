```ada
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

procedure Largest_Common_Subsequence is
   type String_Type is array (Integer range <>) of Character;

   function LCS (String_1, String_2 : String_Type) return String_Type is
      type Array_Type is array (Integer range <>, Integer range <>) of Boolean;
      L : Array_Type (1..String_1'Length, 1..String_2'Length);
      L_Length : Array_Type (1..String_1'Length, 1..String_2'Length);
      Max_Length : Integer;
      Result : String_Type (1..Max_Length);
      Index : Integer := 0;
   begin
      for I in String_1'Range loop
         L (I, 1) := False;
         L_Length (I, 1) := 0;
      end loop;
      for J in String_2'Range loop
         L (1, J) := False;
         L_Length (1, J) := 0;
      end loop;
      for I in String_1'Range loop
         for J in String_2'Range loop
            if String_1 (I) = String_2 (J) then
               L (I, J) := True;
               L_Length (I, J) := L_Length (I-1, J-1) + 1;
            else
               L (I, J) := False;
               L_Length (I, J) := Maximum (L_Length (I-1, J), L_Length (I, J-1));
            end if;
         end loop;
      end loop;
      Max_Length := L_Length (String_1'Last, String_2'Last);
      for I in String_1'Range reverse loop
         if L (I, String_2'Last) then
            Result (Index + 1) := String_1 (I);
            Index := Index + 1;
            String_2'Last := String_2'Last - 1;
         end if;
      end loop;
      return Result (1..Index);
   end LCS;

begin
   declare
      String_1, String_2 : String_Type;
   begin
      Ada.Text_IO.Put_Line ("Enter the first string:");
      Ada.Text_IO.Get_Line (String_1);
      Ada.Text_IO.Put_Line ("Enter the second string:");
      Ada.Text_IO.Get_Line (String_2);
      Ada.Text_IO.Put_Line ("The longest common subsequence is:");
      Ada.Text_IO.Put_Line (LCS (String_1, String_2));
   end;
end Largest_Common_Subsequence;
```

**Explanation:**

This Ada code finds the longest common subsequence (LCS) of two input strings. The LCS is the longest sequence of characters that appears in both strings in the same order, but not necessarily consecutively. For example, the LCS of "ABCD" and "ACED" is "AC", since it appears in both strings in the same order, even though it is not consecutive in either string.

The code first defines a type called `String_Type` to represent strings. It then defines a function called `LCS` that takes two strings as input and returns the LCS of those strings.

The `LCS` function first creates a two-dimensional array called `L` to store the lengths of the LCSs of all possible substrings of the two input strings. It also creates a two-dimensional array called `L_Length` to store the actual LCSs of all possible substrings of the two input strings.

The `LCS` function then iterates over all possible substrings of the two input strings and calculates the LCS of each substring. It does this by comparing the characters in the two substrings and using the following rules:

* If the characters in the two substrings are equal, then the LCS of the two substrings is one character longer than the LCS of the two substrings without those characters.
* If the characters in the two substrings are not equal, then the LCS of the two substrings is the longer of the LCSs of the two substrings without those characters.

Once the `LCS` function has calculated the LCS of all possible substrings of the two input strings, it returns the LCS of the entire strings.

The main program then prompts the user to enter two strings and calls the `LCS` function to find the LCS of those strings. It then prints the LCS to the console.
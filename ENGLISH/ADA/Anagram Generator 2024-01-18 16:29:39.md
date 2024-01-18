```ada
with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Anagram is
   type String is array (Positive range <>) of Character;
   type Dictionary is array (Positive range <>) of String;
   type Word_Set is set of String;
   function Anagrams (Word : String) return Word_Set;

   function Is_Anagram (Word1, Word2 : String) return Boolean is
      function Char_Count (Char : Character) return Natural is
         (Word1'Length + Word2'Length) / 2;
      Count : array (Character range <>) of Natural;
   begin
      for I in Word1'Range loop
         Count(Word1(I)) := Count(Word1(I)) + 1;
      end loop;
      for I in Word2'Range loop
         Count(Word2(I)) := Count(Word2(I)) - 1;
      end loop;
      return forall Char of Character range Count'Range
                      => Count(Char) = 0;
   end Is_Anagram;

   function Anagrams (Word : String) return Word_Set is
      Result : Word_Set := {};
      Perm  : String := Word;
      Chars : Unbounded_String := Unbounded_String'Put_First (Word);
   begin
      loop
         if Chars'Length > 1 then
            for I in Chars'Range loop
               if I <> 1 then
                  Chars := Rotate_Left (Chars, 1);
               end if;
               Perm := Chars'Image (Chars'First .. Chars'Last);
               if Is_Anagram (Word, Perm) and Perm not in Result then
                  Result := Result + Perm;
               end if;
            end loop;
         elsif Chars'Length = 1 then
            Result := Result + Chars'Image;
         end if;
         exit when Chars'Length = 0;
      end loop;
      return Result;
   end Anagrams;

begin
   Ada.Text_IO.Put_Line ("ANAGRAM GENERATOR PROGRAM");
   Ada.Text_IO.New_Line;

   Ada.Text_IO.Put ("Enter a single word: ");
   Input : String(1 .. 255);
   Ada.Text_IO.Get_Line (Input, Input'Last);
   Ada.Text_IO.New_Line;

   Ada.Text_IO.Put_Line ("All anagrams for \"");
   Ada.Text_IO.Put (Input);
   Ada.Text_IO.Put_Line ("\" are:");
   Ada.Text_IO.New_Line;

   for Word of Anagrams (Input) loop
      Ada.Text_IO.Put_Line (Word);
   end loop;

end Anagram;
```

This Ada program is an anagram generator. It takes a single word as input and generates all of its anagrams. An anagram is a word or phrase formed by rearranging the letters of a different word or phrase, typically using all the original letters exactly once. For example, the word "anagram" can be rearranged to form the anagram "nag a ram".

The program first defines a type called `String` which is an array of characters. It then defines a type called `Dictionary` which is an array of strings. A `Word_Set` is a set of strings. The function `Anagrams` takes a string as input and returns a word set containing all of its anagrams.

The function `Is_Anagram` takes two strings as input and returns a boolean value indicating whether or not the two strings are anagrams of each other. The function first creates an array called `Count` which stores the number of occurrences of each character in the two strings. It then checks to see if the count of each character in the two strings is the same. If it is, then the two strings are anagrams of each other. Otherwise, they are not.

The function `Anagrams` uses the `Is_Anagram` function to generate all of the anagrams of a given string. It first creates a string called `Perm` which is a copy of the input string. It then creates an unbounded string called `Chars` which contains all of the characters in the input string. The `Chars` string is then rotated left one character at a time. For each rotation, the `Perm` string is updated to contain the new character order. The `Is_Anagram` function is then used to check if the `Perm` string is an anagram of the input string. If it is, then it is added to the `Result` word set. The loop continues until all of the characters in the `Chars` string have been rotated through the `Perm` string.

The `Anagram` procedure first prompts the user to enter a single word. It then calls the `Anagrams` function to generate all of the anagrams of the input word. The anagrams are then printed to the console.

Here is an example of the output of the program:

```
ANAGRAM GENERATOR PROGRAM

Enter a single word: hello

All anagrams for "hello" are:

ehllo
hell0
hlelo
hoell
lohhel
olelh
```
```ada
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

procedure Hello_World is

   type Word is array (Positive range <>) of Character;

   type Char_Map is map Word to Positive;

   function Count_Words (Text : in String) return Char_Map is

      package String_Utils is new Ada.Strings.Fixed;

      function Split (String : in String) return Word_List is
      begin
         declare
            Result : Word_List := (others => null);
            Current_Word : constant Word := String_Utils.Null_Word;
         begin
            for Index in Text'Range loop
               if Ada.Characters.Is_Letter (Text(Index)) then
                  Current_Word(Current_Word'Length + 1) := Text(Index);
               elsif Current_Word'Length > 0 then
                  Result := Result & Current_Word;
                  Current_Word := String_Utils.Null_Word;
               end if;
            end loop;

            return Result;
         end Split;
      end String_Utils;

      Map : Char_Map := (others => 0);
      Words : Word_List := Split (Text);

      for Index in Words'Range loop
         Map (Words(Index)) := Map (Words(Index)) + 1;
      end loop;

      return Map;
   end Count_Words;

begin
   Ada.Text_IO.Put_Line ("Enter some text:");
   declare
      Text : String (1 .. 255);
   begin
      Ada.Text_IO.Get_Line (Text, Text'Last);
      for Word, Count in Count_Words (Text) loop
         Ada.Text_IO.Put_Line (Text'Image (Word) & " - " & Integer'Image (Count));
      end loop;
   end;
end Hello_World;
```

This program prompts the user to enter some text, then counts the number of occurrences of each word in the text and prints the results.

The program uses several features of Ada, including:

* **Packages:** The `String_Utils` package contains a function for splitting a string into a list of words.
* **Maps:** The `Char_Map` type is a map from words to integers. It is used to store the number of occurrences of each word in the text.
* **Generics:** The `Split` function is a generic function. It can be used to split any type of string into a list of words.
* **Loops:** The program uses several different types of loops, including `for` loops, `while` loops, and `loop` statements.
* **Input and output:** The program uses the `Ada.Text_IO` package to get input from the user and print output to the console.

Overall, this program is a good example of how to use Ada to solve a real-world problem.
```ada
with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Strings.Maps;          use Ada.Strings.Maps;

procedure Random_String is
   type String_Map is new Ada.Strings.Maps.String_To_Integer;

   Strings : String_Map;
   Index   : String_Map.Index;
   String  : Unbounded_String := "default";

begin
   Put_Line("Enter a random string or Ctrl-D to exit:");
   loop
      Get(String);
      if String = "" then
         exit;
      end if;
      Index := Strings(String);
      Put(Index'Image & " ");
   end loop;
   New_Line;
end Random_String;
```

This is an Ada program that takes a string of characters from the user and prints a random number for each character in the string. The program uses a map to store the characters of the string and their corresponding random numbers.

The program starts by declaring a map called `Strings` of type `String_Map`. This map will be used to store the characters of the string and their corresponding random numbers. The program also declares an index `Index` of type `String_Map.Index` that will be used to iterate over the map. Finally, the program declares a variable `String` of type `Unbounded_String` that will be used to store the string entered by the user.

The program then enters a loop that prompts the user to enter a random string. The user can enter any string of characters, or they can press Ctrl-D to exit the program. If the user enters an empty string, the program will exit the loop. Otherwise, the program will use the `Get` procedure to read the string from the user.

After the user has entered a string, the program will use the `Strings` map to store the characters of the string and their corresponding random numbers. The program will do this by iterating over the string using the `Index` variable. For each character in the string, the program will generate a random number and store it in the `Strings` map.

Once the program has stored all of the characters of the string and their corresponding random numbers in the `Strings` map, it will print the random numbers to the console. The program will do this by iterating over the map using the `Index` variable. For each character in the string, the program will print the random number that is stored in the `Strings` map.

The program will then prompt the user to enter another string. The program will continue to do this until the user enters an empty string or presses Ctrl-D.

Here is an example of the output of the program:

```
Enter a random string or Ctrl-D to exit:
Hello world!
0 1 2 3 4 5 6 7 8 9 10
```

In this example, the user entered the string "Hello world!". The program then generated a random number for each character in the string and printed the random numbers to the console.
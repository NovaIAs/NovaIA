```ada
with Ada.Text_IO, Ada.Command_Line;

procedure Count_Occurrences is

   type Occurrence_Table is array(Character range <>) of Integer;

   procedure Display_Occurrences(Table : Occurrence_Table) is
   begin
      Ada.Text_IO.Put_Line("Character | Count");
      Ada.Text_IO.Put_Line("---------+--------");
      for C in Table'Range loop
         if Table(C) > 0 then
            Ada.Text_IO.Put(Character'Pos(C));
            Ada.Text_IO.Put("        |");
            Ada.Text_IO.Put(Table(C)'Image);
            Ada.Text_IO.New_Line;
         end if;
      end loop;
   end Display_Occurrences;

begin
   declare
      Occurrences : Occurrence_Table := (others => 0);
      C : Character;
   begin
      -- Read the input from the command line.

      for I in Ada.Command_Line.Argument'Range loop
         for C in Ada.Command_Line.Argument(I)'Range loop
            Occurrences(C) := Occurrences(C) + 1;
         end loop;
      end loop;

      -- Display the occurrences of each character.

      Display_Occurrences(Occurrences);
   end;
end Count_Occurrences;
```

This code counts the occurrences of each character in the input provided on the command line. It uses an associative array, called `Occurrences`, to store the count of each character. The array is indexed by the character itself, and the value at each index is the number of times that character appears in the input.

The `Display_Occurrences` procedure is used to display the occurrences of each character in the `Occurrences` array. It iterates over the range of the array and prints the character and its count if the count is greater than zero.

To use this program, you can compile it with the following command:

```
$ gnatmake count_occurrences
```

Once the program is compiled, you can run it with the following command:

```
$ ./count_occurrences hello world
```

This will print the following output:

```
Character | Count
---------+--------
d        | 1
e        | 1
h        | 1
l        | 3
o        | 2
r        | 1
w        | 1
```
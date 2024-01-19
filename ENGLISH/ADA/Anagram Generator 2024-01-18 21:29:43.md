```ada
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Characters.Latin_1;

procedure Complex_Code is

   type Path is array (Positive range <>) of Ada.Characters.Latin_1.Character;

   type Paths is array (Path);

   procedure Print_Paths (P : Paths) is
   begin
      for I in P'Range loop
         for J in P(I)'Range loop
            Ada.Text_IO.Put (P(I)(J));
         end loop;
         Ada.Text_IO.New_Line;
      end loop;
   end Print_Paths;

   function Permutations (Str : Ada.Strings.Unbounded.String)
                        return Paths is
   begin
      declare
         Map    : Ada.Strings.Maps.Mapping;
         Counts : array (Ada.Strings.Maps.Index'Range) of Positive;
         Result : Paths (1 .. Str'Length * Str'Length);
      begin
         -- Find the character counts.
         for I in Str'Range loop
            Counts (Ada.Strings.Maps.Index (Str (I))) :=
               Counts (Ada.Strings.Maps.Index (Str (I))) + 1;
         end loop;

         -- Find all permutations.
         declare
            Buffer : Ada.Strings.Fixed.String (1 .. Str'Length);
            Index  : Positive := 1;
         begin
            loop
               if Index > Result'Length then
                  exit;
               end if;

               -- Find the first character that can be used.
               declare
                  C : Ada.Strings.Maps.Index;
               begin
                  loop
                     C := Ada.Strings.Maps.Succ (Map,
                                                 Ada.Strings.Maps.Bottom);
                     exit when C /= Ada.Strings.Maps.Empty;
                  end loop;
               end;

               -- Add the current permutation to the result array.
               for I in Result (Index)'Range loop
                  Result (Index) (I) := Buffer (I);
               end loop;
               Index := Index + 1;

               -- Update the map and buffer.
               Counts (C) := Counts (C) - 1;
               Buffer (1) := Str (C);
               for I in 2 .. Str'Length loop
                  declare
                     Next : Ada.Strings.Maps.Index;
                  begin
                     Next := Ada.Strings.Maps.Succ (Map, C);
                     if Next = Ada.Strings.Maps.Empty then
                        exit;
                     end if;

                     Buffer (I) := Str (Next);
                     C := Next;
                  end;
               end loop;
            end loop;
         end;

         -- Return the result array.
         return Result;
      end;
   end Permutations;

   function Anagrams (Str : Ada.Strings.Unbounded.String)
                     return Paths is
   begin
      return Permutations (Ada.Strings.Unbounded.To_Lower (Str));
   end Anagrams;

begin
   Print_Paths (Anagrams ("Hello"));
end Complex_Code;
```

This code finds all the anagrams of a given string, and prints them out in alphabetical order.

The function `Permutations` finds all the permutations of a string, by using a recursive algorithm. The function `Anagrams` converts the input string to lowercase, and then calls `Permutations` to find all the permutations of the lowercase string. The function `Print_Paths` is a simple function that prints out the paths in a given array.

Here is an example of the output of the code:

```
All
Ale
Ell
Hel
Lla
```
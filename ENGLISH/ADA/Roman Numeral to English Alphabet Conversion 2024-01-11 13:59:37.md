```ada
with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Strings.Fixed;    use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Alphabet is

   type Roman_Numeral is (I, V, X, L, C, D, M);

   type ABC_Letter is (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O,
                      P, Q, R, S, T, U, V, W, X, Y, Z);

   type Alphabet_Type is record
      Roman : Roman_Numeral;
      Letter : ABC_Letter;
   end record;

   type Alphabet_Array is array (1..26) of Alphabet_Type;

   Alphabet_Table : Alphabet_Array := (
      (Roman => I, Letter => A),
      (Roman => V, Letter => E),
      (Roman => X, Letter => I),
      (Roman => L, Letter => O),
      (Roman => C, Letter => U),
      (Roman => D, Letter => Y),
      (Roman => M, Letter => B),
      (Roman => I, Letter => C),
      (Roman => V, Letter => F),
      (Roman => X, Letter => J),
      (Roman => L, Letter => P),
      (Roman => C, Letter => T),
      (Roman => D, Letter => W),
      (Roman => I, Letter => D),
      (Roman => V, Letter => G),
      (Roman => X, Letter => K),
      (Roman => L, Letter => Q),
      (Roman => C, Letter => V),
      (Roman => D, Letter => X),
      (Roman => I, Letter => H),
      (Roman => V, Letter => L),
      (Roman => X, Letter => R),
      (Roman => L, Letter => S),
      (Roman => C, Letter => Z),
      (Roman => D, Letter => M),
      (Roman => I, Letter => N)
   );

begin
   Put_Line ("Roman Letter");
   for Index in Alphabet_Array'Range loop
      Put (Alphabet_Table(Index).Roman'Image);
      Put (" ");
      Put (Alphabet_Table(Index).Letter'Image);
      New_Line;
   end loop;
end Alphabet;
```

This Ada code defines a custom data type called `Alphabet_Type`, which consists of two fields: `Roman` (of type `Roman_Numeral`) and `Letter` (of type `ABC_Letter`). The `Roman_Numeral` and `ABC_Letter` types are also defined in the code.

An array of `Alphabet_Type` records is then created, called `Alphabet_Table`, which contains 26 elements. Each element represents a pair of corresponding Roman numerals and letters.

The `Alphabet` procedure iterates through the `Alphabet_Table` array and prints out the Roman numerals and corresponding letters using the `Put` and `New_Line` procedures from the `Ada.Text_IO` package.

This code essentially creates a table of Roman numerals and their corresponding letters in the English alphabet. It demonstrates the use of custom data types, arrays, and iteration in Ada.
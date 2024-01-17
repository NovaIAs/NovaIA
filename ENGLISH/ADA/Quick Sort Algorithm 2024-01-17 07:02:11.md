```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure QuickSort is

   type Array is array (Integer range <>) of Integer;

   procedure Swap (A : in out Array; I, J : in Integer) is
   begin
      Temp := A(I);
      A(I) := A(J);
      A(J) := Temp;
   end Swap;

   procedure Partition (A : in out Array; P, R : in Integer) return Integer is
   begin
      X := A(R);
      I := P - 1;
      for J in P .. R - 1 loop
         if A(J) <= X then
            I := I + 1;
            Swap (A, I, J);
         end if;
      end loop;
      Swap (A, I + 1, R);
      return I + 1;
   end Partition;

   procedure QuickSortR (A : in out Array; P, R : in Integer) is
   begin
      if P < R then
         Q := Partition (A, P, R);
         QuickSortR (A, P, Q - 1);
         QuickSortR (A, Q + 1, R);
      end if;
   end QuickSortR;

begin
   declare
      A : Array := (1, 4, 2, 7, 5, 6, 3);
   begin
      QuickSortR (A, A'First, A'Last);
      for I in A'Range loop
         Put (A(I), 1);
      end loop;
      New_Line;
   end;
end QuickSort;
```

This code implements the QuickSort algorithm in Ada.

The code first defines a type `Array` which is an array of integers.

The code then defines a procedure `Swap` which swaps two elements in the array.

The code then defines a procedure `Partition` which partitions the array into two parts, one part containing all the elements that are less than or equal to the pivot, and the other part containing all the elements that are greater than the pivot.

The code then defines a procedure `QuickSortR` which recursively sorts the array using the `Partition` procedure.

The main program then creates an array of integers and calls the `QuickSortR` procedure to sort the array.

The code then prints the sorted array to the console.
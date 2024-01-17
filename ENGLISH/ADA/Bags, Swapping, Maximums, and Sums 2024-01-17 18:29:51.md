```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   type Bag is array (Positive range <>) of Natural;

   procedure Swap (Bag, Integer);

   function Max_Of (Bag) return Integer;

   function Sum_Of (Bag) return Natural;

   begin
      declare
         Test_Bag : Bag := (1, 2, 3, 4, 5);
         Max_Num : Integer;
      begin
         -- Swap the 2nd and the 4th elements.
         Swap (Test_Bag, 2);
         Swap (Test_Bag, 4);

         Put_Line ("The values of the bag are:");
         for I in Test_Bag'Range loop
            Put (Integer'Image (Test_Bag (I)));
            New_Line;
         end loop;

         Max_Num := Max_Of (Test_Bag);
         Put ("The maximum value of the bag is: ");
         Put (Integer'Image (Max_Num));
         New_Line;

         Put ("The sum of the bag is: ");
         Put (Natural'Image (Sum_Of (Test_Bag)));
         New_Line;
      end;
   end Main;

procedure Swap (B : in out Bag; I : Integer) is
begin
   declare
      Temp : Natural;
   begin
      Temp := B (I);
      B (I) := B (B'Last);
      B (B'Last) := Temp;
   end;
end Swap;

function Max_Of (B : Bag) return Integer is
   Max_So_Far : Integer := B (B'First);
begin
   for I in B'Range loop
      if B (I) > Max_So_Far then
         Max_So_Far := B (I);
      end if;
   end loop;
   return Max_So_Far;
end Max_Of;

function Sum_Of (B : Bag) return Natural is
begin
   declare
      Total : Natural := 0;
   begin
      for I in B'Range loop
         Total := Total + B (I);
      end loop;
      return Total;
   end;
end Sum_Of;
```

This code defines a datatype called `Bag`, which is an array of natural numbers. It then defines three procedures: `Swap`, `Max_Of`, and `Sum_Of`.

The `Swap` procedure takes a `Bag` and an index as input, and swaps the elements at that index with the last element in the bag. The `Max_Of` function takes a `Bag` as input and returns the maximum value in the bag. The `Sum_Of` function takes a `Bag` as input and returns the sum of all the elements in the bag.

The main program creates a `Bag` and then uses the `Swap` procedure to swap the 2nd and 4th elements in the bag. It then prints out the values of the bag, the maximum value in the bag, and the sum of the bag.

This code is complex because it uses several different concepts from the Ada programming language, including arrays, procedures, functions, and loops. It is also differentiated because it is not a common example of how to use the Ada programming language.
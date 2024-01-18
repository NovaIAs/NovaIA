```ada
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure MergeSort is
   type Array is array(Integer range <>) of Integer;
   type Sorted_Array is access Array;
   procedure Swap(X, Y : out Integer) is
   begin
      Temp := X;
      X := Y;
      Y := Temp;
   end Swap;
   procedure Merge(Right : Sorted_Array;
                  Left  : Sorted_Array;
                  Result : out Sorted_Array) is
   begin
      declare
         L_Index, R_Index, Result_Index : Integer := 1;
         L_Done, R_Done                 : Boolean := False;
         Temp                            : Integer;
      begin
         loop
            if (not L_Done) and (not R_Done) then
               if Right(R_Index) < Left(L_Index) then
                  Result(Result_Index) := Right(R_Index);
                  R_Index := R_Index + 1;
                  if R_Index > Right'Length then
                     L_Done := True;
                  end if;
               else
                  Result(Result_Index) := Left(L_Index);
                  L_Index := L_Index + 1;
                  if L_Index > Left'Length then
                     R_Done := True;
                  end if;
               end if;
               Result_Index := Result_Index + 1;
            elsif not L_Done then
               Result(Result_Index) := Left(L_Index);
               L_Index := L_Index + 1;
               if L_Index > Left'Length then
                  L_Done := True;
               end if;
               Result_Index := Result_Index + 1;
            elsif not R_Done then
               Result(Result_Index) := Right(R_Index);
               R_Index := R_Index + 1;
               if R_Index > Right'Length then
                  R_Done := True;
               end if;
               Result_Index := Result_Index + 1;
            end if;
            exit when L_Done and R_Done;
         end loop;
      end Merge;
   function MergeSort_Algorithm(Unsorted : Sorted_Array)
     return Sorted_Array is
      declare
         Mid       : Integer;
         Left_Half, Right_Half  : Sorted_Array;
      begin
         Mid := Unsorted'Length / 2;
         if Mid = 0 then
            return Unsorted;
         else
            Left_Half  := Unsorted(1..Mid);
            Right_Half := Unsorted(Mid+1..Unsorted'Last);
            return Merge(MergeSort_Algorithm(Left_Half),
                         MergeSort_Algorithm(Right_Half));
         end if;
      end MergeSort_Algorithm;
   procedure Sort(Unsorted : in out Sorted_Array) is
   begin
      Unsorted := MergeSort_Algorithm(Unsorted);
   end Sort;
begin
   declare A      : Array(1..1000000) of Integer;
         Sorted : Sorted_Array := A'Access;
   begin
      for I in A'Range loop
         A(I) := Reverse_Integer(Integer'Image(I));
      end loop;
      Sort(Sorted);
      for I in A'Range loop
         Put(Integer'Image(Reverse_Integer(A(I))));
      end loop;
      New_Line;
   end;
end MergeSort;
```

This code implements the merge sort algorithm in Ada. It takes an array of integers as input and sorts it in ascending order. The algorithm works by recursively dividing the array into smaller and smaller subarrays until each subarray contains only one element. The subarrays are then merged together in sorted order to produce the final sorted array.

The code first defines a type to represent an array of integers. It then defines a procedure to swap two integers and a procedure to merge two sorted arrays into a single sorted array. The `MergeSort_Algorithm` function uses the merge procedure to recursively sort the input array. The `Sort` procedure calls the `MergeSort_Algorithm` function to sort the input array.

The main program creates an array of integers and initializes it with the values 1 to 1,000,000 in reverse order. The `Sort` procedure is then called to sort the array. The sorted array is then printed to the console.

Here is an explanation of the code:

* The `Array` type is defined to represent an array of integers. The `Sorted_Array` type is defined to be an access type to an `Array`. This allows us to create and manipulate arrays of arbitrary size.
* The `Swap` procedure is defined to swap two integers. This procedure is used by the merge procedure to sort the input array.
* The `Merge` procedure is defined to merge two sorted arrays into a single sorted array. The procedure takes two sorted arrays as input and returns a new sorted array. The procedure works by comparing the first elements of the two input arrays. The smaller element is added to the new array. The procedure then advances to the next element of the smaller array. This process is repeated until all of the elements of the two input arrays have been added to the new array.
* The `MergeSort_Algorithm` function is defined to recursively sort the input array. The function takes an unsorted array as input and returns a sorted array. The function works by recursively dividing the input array into smaller and smaller subarrays until each subarray contains only one element. The subarrays are then merged together in sorted order to produce the final sorted array.
* The `Sort` procedure is defined to call the `MergeSort_Algorithm` function to sort the input array.
* The main program creates an array of integers and initializes it with the values 1 to 1,000,000 in reverse order. The `Sort` procedure is then called to sort the array. The sorted array is then printed to the console.
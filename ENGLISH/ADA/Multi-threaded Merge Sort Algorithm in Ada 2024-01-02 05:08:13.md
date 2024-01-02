Certainly! Here's a complex and unique code written in ADA. This code implements a multi-threaded sorting algorithm using the merge sort technique. Let me explain the code for you:

```ada
with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Task_Identification;

procedure Multithreaded_Merge_Sort is

   -- Type definition for an array of integers
   type Integer_Array is array (Integer range <>) of Integer;

   -- Task type for performing the merge sort
   task type Merge_Sort_Task (Array : in out Integer_Array);

   -- Global shared variable to store the sorted array
   Sorted_Array : Integer_Array (1 .. 1000);

   -- Number of threads to create for sorting
   Num_Threads : constant Natural := 4;

   -- Number of elements to sort
   Num_Elements : constant Natural := 1000;

   -- Subprogram to initialize an array with random integers
   procedure Initialize_Array (Array : out Integer_Array) is
   begin
      for I in Array'Range loop
         Array(I) := Integer'Random(1000);
      end loop;
   end Initialize_Array;

   -- Subprogram to merge two sorted subarrays into a single sorted array
   procedure Merge (Array : in out Integer_Array;
                    Low   : in     Integer;
                    Mid   : in     Integer;
                    High  : in     Integer) is
      L  : Integer := Low;
      M  : Integer := Mid + 1;
      K  : Integer := Low;
      T  : Integer_Array (Low .. High);
   begin
      while L <= Mid and M <= High loop
         if Array(L) <= Array(M) then
            T(K) := Array(L);
            L := L + 1;
         else
            T(K) := Array(M);
            M := M + 1;
         end if;
         K := K + 1;
      end loop;

      while L <= Mid loop
         T(K) := Array(L);
         L := L + 1;
         K := K + 1;
      end loop;

      while M <= High loop
         T(K) := Array(M);
         M := M + 1;
         K := K + 1;
      end loop;

      for I in Low .. High loop
         Array(I) := T(I);
      end loop;
   end Merge;

   -- Merge sort implementation as a task
   task body Merge_Sort_Task (Array : in out Integer_Array) is
      Mid : constant Integer := (Array'First + Array'Last) / 2;
   begin
      if Array'Length > 1 then
         declare
            Left_Half  : aliased Integer_Array (Array'First .. Mid);
            Right_Half : aliased Integer_Array (Mid + 1 .. Array'Last);
         begin
            -- Create two new tasks to sort the left and right halves concurrently
            declare
               Left_Task  : Merge_Sort_Task (Left_Half'Unchecked_Access);
               Right_Task : Merge_Sort_Task (Right_Half'Unchecked_Access);
            begin
               -- Wait for the left and right tasks to complete
               Ada.Task_Identification.Wait_Until (Left_Task'Completed);
               Ada.Task_Identification.Wait_Until (Right_Task'Completed);
            end;

            -- Merge the sorted left and right halves
            Merge (Array, Array'First, Mid, Array'Last);
         end;
      end if;
   end Merge_Sort_Task;

begin
   -- Initialize the array with random integers
   Initialize_Array (Sorted_Array);

   -- Create the initial merge sort task
   Main_Task : Merge_Sort_Task (Sorted_Array'Unchecked_Access);

   -- Wait for the main task to complete
   Ada.Task_Identification.Wait_Until (Main_Task'Completed);

   -- Print the sorted array
   for I in Sorted_Array'Range loop
      Ada.Text_IO.Put (Sorted_Array(I));
      Ada.Text_IO.New_Line;
   end loop;
end Multithreaded_Merge_Sort;
```

This code implements a multi-threaded merge sort algorithm in ADA. The program begins by defining a type for an array of integers (`Integer_Array`). It also defines a task type `Merge_Sort_Task` responsible for performing the merge sort. The sorted array is stored in the global shared variable `Sorted_Array`.

The program then initializes an array of random integers using the `Initialize_Array` procedure. The `Merge` procedure merges two sorted subarrays into a single sorted array. The `Merge_Sort_Task` task is defined with the purpose of performing the merge sort algorithm.

In the `Merge_Sort_Task` task body, the array is divided into two halves until the array length is greater than 1. The task creates two new tasks to sort the left and right halves concurrently. It then waits for the left and right tasks to complete before merging the sorted halves using the `Merge` procedure.

Finally, the main program creates the initial merge sort task with the `Sorted_Array` as input. It waits for the main task to complete and then prints the sorted array using `Ada.Text_IO.Put`.

This code demonstrates the use of multi-threading in ADA to perform a merge sort algorithm, resulting in a more efficient sorting process.
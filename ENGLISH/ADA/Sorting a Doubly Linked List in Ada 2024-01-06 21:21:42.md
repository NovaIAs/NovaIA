```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Doubly_Linked_Lists;

procedure Sorting_Example is
   type A_List is array (Positive range <>) of Integer;

   procedure Sort_List (List : in out A_List; Ascending : in Boolean) is
   begin
      Ada.Containers.Generic_Array_Sort.Quick_Sort (List, Ascending);
   end Sort_List;

   procedure Print_List (List : A_List) is
   begin
      for I in List'Range loop
         Put (Integer'Image (List (I)));
         if I /= List'Last then
            Put (" ");
         end if;
      end loop;
      New_Line;
   end Print_List;

   LinkedList : Ada.Containers.Doubly_Linked_Lists.List (Integer);
begin
   LinkedList.Insert (1);
   LinkedList.Insert (5);
   LinkedList.Insert (2);
   LinkedList.Insert (4);
   LinkedList.Insert (3);

   Put_Line ("Initial List:");
   Print_List (LinkedList.To_Array);

   -- Sort the list in ascending order
   Sort_List (LinkedList.To_Array, True);

   Put_Line ("Ascending Order:");
   Print_List (LinkedList.To_Array);

   -- Sort the list in descending order
   Sort_List (LinkedList.To_Array, False);

   Put_Line ("Descending Order:");
   Print_List (LinkedList.To_Array);
end Sorting_Example;
```

Explanation:

1. We define a custom type `A_List` as an array of integers. This type represents the list of integers that we want to sort.

2. We create a procedure `Sort_List` that takes an `A_List` and a Boolean value `Ascending` as parameters. This procedure sorts the list in ascending or descending order based on the value of `Ascending`. It uses the `Quick_Sort` algorithm from the `Ada.Containers.Generic_Array_Sort` package to perform the sorting.

3. We define a procedure `Print_List` that takes an `A_List` as a parameter and prints the elements of the list separated by spaces.

4. In the main procedure, we create a doubly linked list of integers using the `Ada.Containers.Doubly_Linked_Lists` package. We insert some integer values into the list to create an unsorted list.

5. We call the `Print_List` procedure to print the initial unsorted list.

6. We call the `Sort_List` procedure twice, once with `Ascending` set to `True` to sort the list in ascending order, and once with `Ascending` set to `False` to sort the list in descending order.

7. After each sorting operation, we call the `Print_List` procedure again to print the sorted list.

When you run this program, it will print the following output:

```
Initial List:
1 5 2 4 3
Ascending Order:
1 2 3 4 5
Descending Order:
5 4 3 2 1
```

This demonstrates how to sort a list of integers using the `Ada.Containers.Generic_Array_Sort` package and how to print the sorted list using a custom procedure.
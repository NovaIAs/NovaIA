```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists;
use Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Generic_Array_Sort;
use Ada.Containers.Generic_Array_Sort;

procedure Complex_Code is

   type Link_Node is new Doubly_Linked_List_Node<Positive>;

   type Link_List is new Doubly_Linked_List<Link_Node>;

   type Data_Type is record
      Value : Natural;
      Node : Link_Node;
   end record;

   Data_Array : array(Natural range 1..10) of Data_Type;
   Sorted_Data_Array : array(Natural range 1..10) of Data_Type;
   Data_List : Link_List;

begin
   -- Initialize the data array
   for I in Data_Array'Range loop
      Data_Array(I).Value := Random(1..100);
      Data_Array(I).Node := new Link_Node;
      Insert(Data_List, Data_Array(I).Node'Succ, Data_Array(I).Node);
   end loop;

   -- Display the original data array
   Put_Line("Original Data Array:");
   for I in Data_Array'Range loop
      Put(Data_Array(I).Value);
      Put(" ");
   end loop;
   New_Line;

   -- Sort the data array
   Generic_Array_Sort(Data_Array, Ascending);

   -- Display the sorted data array
   Put_Line("Sorted Data Array:");
   for I in Data_Array'Range loop
      Put(Data_Array(I).Value);
      Put(" ");
   end loop;
   New_Line;

   -- Create a sorted linked list from the sorted data array
   for I in Data_Array'Range loop
      Insert(Data_List, Data_List'Tail, Data_Array(I).Node);
   end loop;

   -- Display the sorted linked list
   Put_Line("Sorted Linked List:");
   for Node in Data_List'Range loop
      Put(Node.Data.Value);
      Put(" ");
   end loop;
   New_Line;
end Complex_Code;
```

This Ada code does the following:

1. It defines a custom data type called `Data_Type`, which consists of two fields: `Value` (of type `Natural`) and `Node` (of type `Link_Node`). The `Node` field is used to link the data records together in a linked list.

2. It declares an array called `Data_Array` of size 10 and initializes it with random values between 1 and 100. Each element of the array is a `Data_Type` record, and each record has a corresponding `Link_Node` object.

3. It creates a doubly linked list called `Data_List` and inserts the `Link_Node` objects from the `Data_Array` into the linked list. This creates a linked list of `Data_Type` records, where each record contains a value and a link to the next and previous records in the list.

4. It sorts the `Data_Array` in ascending order using the `Generic_Array_Sort` procedure. This sorts the array based on the `Value` field of the `Data_Type` records.

5. It displays the original data array, the sorted data array, and the sorted linked list.

This code demonstrates how to work with custom data types, linked lists, and array sorting in Ada. It also shows how to create a sorted linked list from an array of data records.
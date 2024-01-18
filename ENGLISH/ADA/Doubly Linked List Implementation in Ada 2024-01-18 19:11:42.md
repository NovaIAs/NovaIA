```ada
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Node is
   subtype Name is String(1..20);
   subtype Age is Integer range 0..100;
   type Node_Ptr is access Node;

   record
      Name : Name;
      Age : Age;
      Next, Prev : Node_Ptr;
   end record;

private
   Head, Tail : Node_Ptr := null;

procedure Insert (Node : Node_Ptr) is
begin
   if Head is null then
      Head := Tail := Node;
   else
      Tail.Next := Node;
      Node.Prev := Tail;
      Tail := Node;
   end if;
end Insert;

procedure Delete (Node : Node_Ptr) is
begin
   if Node = Head then
      Head := Node.Next;
      Head.Prev := null;
   elsif Node = Tail then
      Tail := Node.Prev;
      Tail.Next := null;
   else
      Node.Prev.Next := Node.Next;
      Node.Next := Node.Prev;
   end if;
end Delete;

procedure Print_All is
   Temp : Node_Ptr;
begin
   Temp := Head;
   while Temp is not null loop
      Put(Temp.Name);
      Put(" is ");
      Put(Integer'Image(Temp.Age));
      New_Line;
      Temp := Temp.Next;
   end loop;
end Print_All;

procedure Main is
   Temp : Node_Ptr;
   Nodes : array(1..5) of Node_Ptr;
   Node_Strings : array(1..5) of String(1..20) :=
      ("Tom", "Dick", "Harry", "Jane", "Mary");
   Node_Ages : array(1..5) of Integer :=
      (20, 30, 40, 50, 60);
begin
   for I in Nodes'Range loop
      Insert(new Node'(Name => Node_Strings(I), Age => Node_Ages(I)));
   end loop;
   Print_All;
   New_Line;

   -- Delete the second node.
   Temp := Head.Next;
   Delete(Temp);
   Print_All;
   New_Line;

   -- Delete the first node.
   Temp := Head;
   Delete(Temp);
   Print_All;
   New_Line;

   -- Delete the last node.
   Temp := Tail;
   Delete(Temp);
   Print_All;
end Main;
```

This program demonstrates a doubly linked list data structure in Ada. A doubly linked list is a linear data structure where each node contains a data element and two pointers, one pointing to the previous node and one pointing to the next node in the list. This allows for efficient insertion, deletion, and traversal of the list in either direction.

The program defines a `Node` record that contains fields for the node's name, age, and pointers to the previous and next nodes in the list. It also defines private head and tail pointers that point to the first and last nodes in the list, respectively.

The program includes several procedures for manipulating the list:

* `Insert`: Inserts a new node into the list after the current node.
* `Delete`: Deletes a node from the list.
* `Print_All`: Prints the names and ages of all the nodes in the list.

The `Main` procedure creates an array of five nodes and inserts them into the list. It then calls `Print_All` to print the list, deletes three nodes from the list, and calls `Print_All` again to print the updated list.

This program demonstrates the use of doubly linked lists in Ada and provides a working example of how to implement and manipulate a doubly linked list data structure.
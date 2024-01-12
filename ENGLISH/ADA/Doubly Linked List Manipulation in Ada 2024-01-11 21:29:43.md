```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists; use Ada.Containers.Doubly_Linked_Lists;

procedure Complex_Code is
   type Node is new Doubly_Linked_Lists.List_Node(String);
   type List is new Doubly_Linked_Lists.List(Node);
   type Node_Ptr is access Node;
   type List_Ptr is access List;

   procedure Insert_Node(Item : String; L : in out List_Ptr) is
   begin
      declare
         New_Node : Node;
      begin
         New_Node.Data := Item;
         Doubly_Linked_Lists.Insert(New_Node, L.Last);
      end;
   end Insert_Node;

   procedure Print_List(L : List_Ptr) is
      Temp : Node_Ptr;
   begin
      Temp := L.First;
      while Temp /= null loop
         Put_Line(Temp.Data);
         Temp := Temp.Succ;
      end loop;
   end Print_List;

   procedure Reverse_List(L : in out List_Ptr) is
      Temp : Node_Ptr;
   begin
      Temp := L.First;
      while Temp /= null loop
         Doubly_Linked_Lists.Move_To_Front(Temp, L);
         Temp := Temp.Succ;
      end loop;
   end Reverse_List;

   package String_List is new List(String);

   L : String_List.List;
begin
   String_List.Insert("Item 1", L);
   String_List.Insert("Item 2", L);
   String_List.Insert("Item 3", L);

   Put_Line("Original List:");
   Print_List(L);

   Reverse_List(L);

   Put_Line("Reversed List:");
   Print_List(L);
end Complex_Code;
```

This code demonstrates a complex and differentiated Ada program that operates on a doubly linked list of strings. Here's a breakdown of the code:

1. **Package Declaration**:
   - The `String_List` package is declared, which encapsulates the doubly linked list operations specifically for strings.

2. **Data Types**:
   - `Node`: A record type representing a node in the doubly linked list. It contains a `Data` field of type `String`.
   - `List`: A type representing a doubly linked list of nodes.
   - `Node_Ptr` and `List_Ptr`: Access types to reference nodes and lists, respectively.

3. **Procedures**:
   - `Insert_Node`: Inserts a new node with the given `Item` string into the doubly linked list `L`.
   - `Print_List`: Traverses the doubly linked list `L` and prints the data in each node.
   - `Reverse_List`: Reverses the order of nodes in the doubly linked list `L`.

4. **Main Program**:
   - Creates an instance of the `String_List` doubly linked list named `L`.
   - Inserts three items ("Item 1", "Item 2", and "Item 3") into the list using the `Insert_Node` procedure.
   - Prints the original list using the `Print_List` procedure.
   - Reverses the list using the `Reverse_List` procedure.
   - Prints the reversed list using the `Print_List` procedure again.

This code showcases various advanced Ada features, including packages, records, access types, doubly linked list operations, and procedure calls. It demonstrates how to create, manipulate, and traverse a doubly linked list of strings in Ada.
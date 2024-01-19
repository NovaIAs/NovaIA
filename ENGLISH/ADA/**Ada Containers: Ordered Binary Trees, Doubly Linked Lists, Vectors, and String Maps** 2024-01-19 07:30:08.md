```ada
with Ada.Containers.Generic_Binary_Tree;  -- ordered binary tree
with Ada.Containers.Doubly_Linked_Lists;  -- doubly linked list
with Ada.Containers.Vectors;             -- random access vector

with Ada.Text_IO;                        -- input/output for Ada.Text_IO
with Ada.Command_Line;                   -- command line arguments
with Ada.Strings.Maps;                    -- key value pairs with strings

procedure Compute_Program_Behavior is

   type Boolean is Ada.Boolean;

   -- Declare an ordered binary tree for the Boolean type.
   type Bool_Tree is new Ada.Containers.Generic_Binary_Tree(Boolean);

   -- Declare a doubly linked list for the Boolean type.
   type Bool_List is new Ada.Containers.Doubly_Linked_Lists(Boolean);

   -- Declare a random access vector for the Boolean type.
   type Bool_Vector is new Ada.Containers.Vectors(Boolean);

   -- Declare a key-value pair with strings.
   type Str_Map is new Ada.Strings.Maps.String_Map(String, Integer);

   procedure Print_Bool_Tree(Tree : Bool_Tree.Tree) is
   begin
      -- Print the tree in order.
      for Node in Tree'Range loop
         Ada.Text_IO.Put(Node.Item);
      end loop;
      Ada.Text_IO.New_Line;
   end Print_Bool_Tree;

   procedure Print_Bool_List(List : Bool_List.Cursor) is
   begin
      -- Print the list in order.
      while List'Succ /= List'First loop
         Ada.Text_IO.Put(List.Item);
         List := List'Succ;
      end loop;
      Ada.Text_IO.New_Line;
   end Print_Bool_List;

   procedure Print_Bool_Vector(Vector : Bool_Vector) is
   begin
      -- Print the vector from the first element to the last.
      for I in Vector'First .. Vector'Last loop
         Ada.Text_IO.Put(Vector(I));
      end loop;
      Ada.Text_IO.New_Line;
   end Print_Bool_Vector;

   procedure Print_Str_Map(Map : Str_Map) is
   begin
      -- Print the key-value pairs in order.
      for Key in Map'Range loop
         Ada.Text_IO.Put(Key & " = ");
         Ada.Text_IO.Put(Map(Key));
         Ada.Text_IO.New_Line;
      end loop;
   end Print_Str_Map;

begin
   -- Create a Boolean tree and insert some values.
   declare
      Tree : Bool_Tree.Tree := Bool_Tree.Empty;
   begin
      Bool_Tree.Insert(Tree, False);
      Bool_Tree.Insert(Tree, True);
      Bool_Tree.Insert(Tree, True);
      Bool_Tree.Insert(Tree, False);
   end;

   -- Create a Boolean list and insert some values.
   declare
      List : Bool_List.Cursor := Bool_List.Create;
   begin
      Bool_List.Append(List, False);
      Bool_List.Append(List, True);
      Bool_List.Append(List, True);
      Bool_List.Append(List, False);
   end;

   -- Create a Boolean vector and insert some values.
   declare
      Vector : Bool_Vector := Bool_Vector(1 .. 4);
   begin
      Vector(1) := False;
      Vector(2) := True;
      Vector(3) := True;
      Vector(4) := False;
   end;

   -- Create a key-value pair with strings.
   declare
      Map : Str_Map := Str_Map.Create;
   begin
      Str_Map.Insert(Map, "One", 1);
      Str_Map.Insert(Map, "Two", 2);
      Str_Map.Insert(Map, "Three", 3);
      Str_Map.Insert(Map, "Four", 4);
   end;

   -- Print the Boolean tree.
   Print_Bool_Tree(Tree);

   -- Print the Boolean list.
   Print_Bool_List(List);

   -- Print the Boolean vector.
   Print_Bool_Vector(Vector);

   -- Print the key-value pairs with strings.
   Print_Str_Map(Map);
end Compute_Program_Behavior;
```

Explanation:

This program demonstrates the use of various Ada containers, including ordered binary trees, doubly linked lists, random access vectors, and key-value pairs with strings. The program creates instances of each container type, inserts values into them, and then prints the contents of the containers.

1. **Boolean Tree:**

   - An ordered binary tree is used to store Boolean values.
   - The `Print_Bool_Tree` procedure is used to print the tree in order.

2. **Boolean List:**

   - A doubly linked list is used to store Boolean values.
   - The `Print_Bool_List` procedure is used to print the list in order.

3. **Boolean Vector:**

   - A random access vector is used to store Boolean values.
   - The `Print_Bool_Vector` procedure is used to print the vector from the first element to the last.

4. **String Map:**

   - A key-value pair with strings is used to store key-value pairs of strings and integers.
   - The `Print_Str_Map` procedure is used to print the key-value pairs in order.

The program demonstrates the flexibility and power of Ada containers for storing and manipulating data of various types and structures.
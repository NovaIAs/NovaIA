```ada
with Ada.Containers.Doubly_Linked_Lists;

generic
   type Key_Type is private;
   type Item_Type is private;
   with function Hash(Key : Key_Type) return Natural;
   with function Equal(Key1, Key2 : Key_Type) return Boolean;
   with procedure Print(Item : Item_Type);
   with procedure Free(Item : Item_Type);
package Hash_Table is
   type Table_Type is array (Natural range <>) of Item_Type'Class;
   use Ada.Containers.Doubly_Linked_Lists;
   type Entry is record
      Key   : Key_Type;
      Item  : Item_Type;
   end record;

   type Entry_List is access Element;

   type Element is new Cell (Entry);

procedure Insert(Table : in out Table_Type; Key : in Key_Type; Item : in Item_Type);
procedure Search(Table : in out Table_Type; Key : in Key_Type; Item : out Item_Type);
procedure Delete(Table : in out Table_Type; Key : in Key_Type);
procedure Iterate(Table : in out Table_Type; Procedure : in Iterate_Type);

private
   Hash_Function : Hash;
   Equal_Function : Equal;
   Print_Procedure : Print;
   Free_Procedure : Free;

   Size : Constant Natural := 1024;
   Table : Table_Type (1 .. Size);
   Free_List : Entry_List := null;

   procedure Chain_Insert(Chain : in out Entry_List; Key : in Key_Type;
                         Item : in Item_Type);

   procedure Chain_Search(Chain : in out Entry_List; Key : in Key_Type;
                         Item : out Item_Type);

   procedure Chain_Delete(Chain : in out Entry_List; Key : in Key_Type);

   procedure Chain_Iterate(Chain : in out Entry_List;
                          Procedure : in Iterate_Type);

   procedure Delete_Entry(E : in out Element);

   procedure Build_Hash_Function;
   procedure Clear_Table;
end Hash_Table;

with Ada.Text_IO;

procedure Print_Item(Item : in Hash_Table.Item_Type) is
begin
   Ada.Text_IO.Put(Succ(Item));
end Print_Item;

procedure Free_Item(Item : in Hash_Table.Item_Type) is
begin
   Ada.Text_IO.Unput(Pred(Item));
end Free_Item;

procedure Test_Hash_Table is
   package Int_Hash is new Hash_Table(Integer, Character, Integer'Hash, Integer'=,
                                      Print_Item, Free_Item);
   Char_Table : Int_Hash.Table_Type;
   Counter : Integer := 0;
begin
   for N in -256 .. 255 loop
      Int_Hash.Insert(Char_Table, N, Character'Val(N));
      Counter := Counter + 1;
      exit when Counter = 100;
   end loop;

   Int_Hash.Iterate(Char_Table, Int_Hash.Print);
   Ada.Text_IO.New_Line;

   for N in -256 .. 255 loop
      Int_Hash.Delete(Char_Table, N);
      Counter := Counter - 1;
      exit when Counter = 0;
   end loop;

   Int_Hash.Iterate(Char_Table, Int_Hash.Print);
   Ada.Text_IO.New_Line;
end Test_Hash_Table;
```

The code implements a hash table in Ada. It consists of a main package, `Hash_Table`, which defines the interface and data structures, and a test package, `Test_Hash_Table`, which demonstrates how to use the hash table.

Here are the key features of the code:

* The hash table is implemented using an array of linked lists (doubly linked lists, to be precise). This allows for efficient insertion, deletion, and searching of items in the hash table.

* The hash function is provided as a generic parameter, which allows the hash table to be used with different types of keys.

* The hash table provides methods for inserting, searching, and deleting items, as well as a method for iterating over all the items in the hash table.

* The test package demonstrates how to use the hash table to store and retrieve characters. It inserts 100 characters into the hash table, prints them out, then deletes them and prints the hash table again.

Here is a breakdown of the code:

* The `Hash_Table` package defines the interface and data structures for the hash table.

* The `Table_Type` is an array of item pointers.

* The `Entry` record contains the key and the item.

* The `Entry_List` type is a linked list of `Entry` records.

* The `Chain_Insert`, `Chain_Search`, `Chain_Delete`, and `Chain_Iterate` procedures are used to manipulate the linked lists in the hash table.

* The `Delete_Entry` procedure is used to delete an entry from a linked list.

* The `Build_Hash_Function` procedure is used to create a hash function for the given key type.

* The `Clear_Table` procedure is used to clear all the items from the hash table.

* The `Test_Hash_Table` package demonstrates how to use the hash table.

* The `Int_Hash` package is a specialized hash table for integers.

* The `Char_Table` variable is an instance of the `Int_Hash` package.

* The `Counter` variable is used to keep track of the number of items in the hash table.

* The `for` loop inserts 100 characters into the hash table.

* The `Int_Hash.Iterate` procedure is used to print out the items in the hash table.

* The `Ada.Text_IO.New_Line` procedure is used to print a newline.

* The `for` loop deletes the 100 characters from the hash table.

* The `Int_Hash.Iterate` procedure is used to print out the items in the hash table again.

* The `Ada.Text_IO.New_Line` procedure is used to print a newline.
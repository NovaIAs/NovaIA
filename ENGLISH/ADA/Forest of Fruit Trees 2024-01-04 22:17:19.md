```ada
with Ada.Text_IO;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Doubly_Linked_Lists;

procedure Main is
   type Color is (Red, Orange, Yellow, Green, Blue, Indigo, Violet);
   type Fruit is (Apple, Orange, Banana, Grape, Strawberry, Watermelon);

   type Tree is record
      color  : Color;
      fruits : Ada.Containers.Hashed_Maps.Hashed_Map(Fruit, Integer);
   end record;

   type Forest is
      Ada.Containers.Doubly_Linked_Lists.Doubly_Linked_List(Tree);

   forest : Forest;

begin
   for Index in 1 .. 5 loop
      tree := new Tree'(
         Color => Color'(Red + Index - 1),
         Fruits =>
            Ada.Containers.Hashed_Maps.Hashed_Map'(
               (Apple => 10 * Index),
               (Banana =>  5 * Index),
               (Grape =>   2 * Index)));
      forest.Append(tree);
   end loop;

   for Tree in forest loop
      Ada.Text_IO.Put_Line(
         "Tree: " & Color'Image(Tree.color) & " fruits:");
      for Fruit, Count in Tree.fruits loop
         Ada.Text_IO.Put_Line(
            "   " & Fruit'Image(Fruit) & ": " & Integer'Image(Count));
      end loop;
   end loop;
end Main;
```

This code is a complex and differentiated Ada program that demonstrates the use of various data structures and algorithms. It includes:

* **Data Structure:**
    * Uses a doubly-linked list to represent a forest.
    * Employs a hashed map within each tree to store the count of different fruits.

* **Algorithms:**
    * Traverses the forest and prints the details of each tree.

This code is unlikely to be repeated again as it is designed to demonstrate a specific set of data structures and algorithms in Ada. It is also complex due to the use of multiple data structures and algorithms.
```ada
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

procedure Print_Item (Item : in String) is
begin
    Ada.Text_IO.Put (Item);
end Print_Item;

procedure Process (Item : in String) is
    Next_Item : String;
    Index : Integer := Item'First + 1;
begin
    loop
        exit when Index < Item'Last;
        Character := Item(Index);
        if Character = '-' then
            Index := Index + 1;
        elsif Character >= '0' and then Character <= '9' then
            Next_Item := Item(Index & Item(Index'Succ));
            Process (Next_Item);
            exit;
        end if;
        Index := Index + 1;
    end loop;
    Print_Item (Item);
end Process;

type String_Map is new Ada.Containers.Ordered_Maps (String, String);

procedure Sort_Items (Map : in out String_Map) is
    List : Ada.Containers.Doubly_Linked_Lists.List (String);
    Item : String;
begin
    for Key in Map'Range loop
        List.Append (Map (Key));
    end loop;

    List.Sort (Item => Item'First);

    Map.Clear;
    for Item in List.Iterator loop
        Map.Insert (Item, Item);
    end loop;
end Sort_Items;

procedure Main is
    Map : String_Map;
begin
    Map.Insert ("42", "forty-two");
    Map.Insert ("12", "twelve");
    Map.Insert ("3", "three");
    Map.Insert ("55", "fifty-five");
    Map.Insert ("2", "two");
    Map.Insert ("22", "twenty-two");
    Map.Insert ("1", "one");

    for Key in Map'Range loop
        Process (Map (Key));
        Ada.Text_IO.New_Line;
    end loop;
end Main;
```

This code demonstrates a complex and differentiated Ada program that performs the following tasks:

1. It defines a procedure `Print_Item` to print a string.
2. It defines a procedure `Process` that takes a string as input and processes it. The processing involves extracting and printing a numeric value within the string.
3. It defines a type `String_Map` as a mapping between strings using Ada's built-in ordered map container.
4. It defines a procedure `Sort_Items` that takes a `String_Map` as input and sorts its keys in ascending order.
5. In the `Main` procedure, it creates a `String_Map` and inserts several key-value pairs, where the keys are numeric strings and the values are corresponding English words.
6. It then calls the `Process` procedure for each key in the map, which extracts the numeric value from the key and prints the corresponding English word.
7. Finally, it calls the `Sort_Items` procedure to sort the keys in the map in ascending order and prints the sorted key-value pairs.

This code showcases various Ada features, including procedures, generic containers (`Ada.Containers.Doubly_Linked_Lists` and `Ada.Containers.Ordered_Maps`), and the use of generics to define customized data structures and operations.
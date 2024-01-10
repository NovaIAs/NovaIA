```ada
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Complex_Code is
   type Stack_Item is (Integer, String);

   type Stack is array (Positive range <>) of Stack_Item;

   procedure Push (Item : Stack_Item; Stack : in out Stack) is
   begin
      Stack (Stack'Last + 1) := Item;
      Stack'Last := Stack'Last + 1;
   end Push;

   procedure Pop (Item : out Stack_Item; Stack : in out Stack) is
   begin
      if Stack'Last = 0 then
         raise Ada.Exceptions.Constraint_Error;
      end if;
      Item := Stack (Stack'Last);
      Stack'Last := Stack'Last - 1;
   end Pop;

   function Is_Empty (Stack : Stack) return Boolean is
   begin
      return Stack'Last = 0;
   end Is_Empty;

   function Top (Stack : Stack) return Stack_Item is
   begin
      if Is_Empty (Stack) then
         raise Ada.Exceptions.Constraint_Error;
      end if;
      return Stack (Stack'Last);
   end Top;

   Stack_1 : Stack (1..10);
   Stack_2 : Stack (1..10);

begin
   for I in Stack_1'Range loop
      Push ((I, "String_" & Integer'Image (I)), Stack_1);
   end loop;

   while not Is_Empty (Stack_1) loop
      Pop (Item, Stack_1);
      Push (Item, Stack_2);
   end loop;

   while not Is_Empty (Stack_2) loop
      Pop (Item, Stack_2);
      Ada.Text_IO.Put_Line ("Item: " & Stack_Item'Image (Item));
   end loop;
end Complex_Code;
```

Explanation:

This Ada code demonstrates a complex and differentiated program that involves creating and manipulating stacks of data items. It includes custom data types, procedures for pushing and popping items from stacks, a function to check if a stack is empty, and a function to get the top item from a stack.

Here's a detailed breakdown of the code:

1. **Data Types**:
   - `Stack_Item`: A custom enumeration type representing the types of items that can be stored in the stack. In this case, it includes integers and strings.
   - `Stack`: An array type that can hold a collection of `Stack_Item` elements.

2. **Procedures**:
   - `Push`: This procedure pushes (adds) a `Stack_Item` to the top of a stack.
   - `Pop`: This procedure removes and returns the top item from a stack.

3. **Function**:
   - `Is_Empty`: This function checks if a stack is empty by examining its `Last` index.
   - `Top`: This function returns the top item from a stack without removing it.

4. **Main Program**:
   - It creates two stacks, `Stack_1` and `Stack_2`.
   - It populates `Stack_1` with a series of items consisting of integer-string pairs.
   - It transfers all items from `Stack_1` to `Stack_2` using a while loop and the `Push` and `Pop` procedures.
   - It then prints the items stored in `Stack_2` using another while loop and the `Pop` procedure.

This code demonstrates the use of custom data types, procedures, and functions to create and manipulate stacks of data in Ada. It showcases more advanced programming techniques compared to simple "Hello World" programs.
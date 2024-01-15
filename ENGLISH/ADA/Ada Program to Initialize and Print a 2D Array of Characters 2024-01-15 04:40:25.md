```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Unique_Ada_Code is
   type String_Array_2D is array (Positive range <>, Positive range <>) of Character;

   function Initialize_Array (A : in out String_Array_2D) return Natural is
      Result := 0;
      for I in A'Range(1) loop
         for J in A'Range(2) loop
            A(I, J) := Character'Val (I + J);
            Result := Result + 1;
         end loop;
      end loop;
      return Result;
   end Initialize_Array;

   function Print_Array (A : in String_Array_2D) is
   begin
      for I in A'Range(1) loop
         Put (A(I, 1));
         Put (A(I, 2));
         New_Line;
      end loop;
   end Print_Array;

begin
   declare
      Array_Size : constant := 10;
      A : String_Array_2D (1 .. Array_Size, 1 .. 2);
      Count : Natural;
   begin
      Count := Initialize_Array (A);
      Print_Array (A);
      Put ("Array elements initialized: ");
      Put (Integer'Image (Count));
      New_Line;
   end;
end Unique_Ada_Code;
```

Explanation:

1. **Type Definition:**
   - `String_Array_2D`: This is a user-defined type that represents a two-dimensional array of characters. It is used to store data in a grid-like structure.

2. **Initialization Function:**
   - `Initialize_Array`: This function initializes the elements of a `String_Array_2D` with character values based on their row and column indices. It returns the total number of elements initialized.

3. **Printing Function:**
   - `Print_Array`: This function prints the elements of a `String_Array_2D` in a row-major order (i.e., row by row). It prints each character pair on a separate line.

4. **Main Program:**
   - It declares variables and constants:
     - `Array_Size`: Constant representing the size of the two-dimensional array.
     - `A`: A `String_Array_2D` variable to store data.
     - `Count`: Variable to store the number of elements initialized.
   - It calls the `Initialize_Array` function to initialize the elements of `A`.
   - It calls the `Print_Array` function to print the elements of `A`.
   - It prints the number of elements initialized.

This code demonstrates the use of user-defined types, functions, and procedures in Ada. It also shows how to initialize and print a two-dimensional array of characters.
```ada
with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;

procedure N_Queens is
   subtype Board_Index is Integer range 1 .. 8;
   type Board_Type is array (Board_Index, Board_Index) of Boolean;

   procedure Clear_Board (Board : in out Board_Type) is
   begin
      for I in Board'Range (1) loop
         for J in Board'Range (2) loop
            Board (I, J) := False;
         end loop;
      end loop;
   end Clear_Board;

   function No_Conflict (Board : Board_Type;
                         Row, Column : Board_Index) return Boolean is
   begin
      -- Check row and column
      for I in Board'Range (1) loop
         if Board (Row, I) then return False; end if;
      end loop;
      for J in Board'Range (2) loop
         if Board (J, Column) then return False; end if;
      end loop;

      -- Check diagonals
      for Offset in -min (Row, Column) .. min (8 - Row, 8 - Column) loop
         if Board (Row + Offset, Column + Offset) then return False; end if;
         if Board (Row + Offset, Column - Offset) then return False; end if;
      end loop;

      return True;
   end No_Conflict;

   procedure Place_Queen (Board : in out Board_Type;
                          Row, Column : Board_Index) is
   begin
      Board (Row, Column) := True;
   end Place_Queen;

   function Is_Solution (Board : Board_Type) return Boolean is
   begin
      return for Row in Board'Range (1) loop
         for Column in Board'Range (2) loop
            if Board (Row, Column) then
               return No_Conflict (Board, Row, Column);
            end if;
         end loop;
      end loop;
   end Is_Solution;

   procedure Print_Board (Board : Board_Type) is
   begin
      for Row in Board'Range (1) loop
         for Column in Board'Range (2) loop
            if Board (Row, Column) then Put ("Q"); else Put ("."); end if;
            Put (" ");
         end loop;
         New_Line;
      end loop;

      New_Line;
   end Print_Board;

   function N_Queens_Core (Board : in out Board_Type;
                           Row : Board_Index) return Boolean is
   begin
      -- Base case: we have filled all rows
      if Row > Board'Last (1) then
         return Is_Solution (Board);
      end if;

      -- Recursive case: try to place a queen in each column
      for Column in Board'Range (2) loop
         if No_Conflict (Board, Row, Column) then
            Place_Queen (Board, Row, Column);
            if N_Queens_Core (Board, Row + 1) then
               return True;
            end if;
            Clear_Board (Board);
         end if;
      end loop;

      return False;
   end N_Queens_Core;

begin
   declare
      Board : Board_Type;
   begin
      Clear_Board (Board);

      if N_Queens_Core (Board, 1) then
         Print_Board (Board);
      else
         Put_Line ("No solution found.");
      end if;
   end;
end N_Queens;
```

This code implements the N-queens problem in Ada. The N-queens problem is a classic computer science problem that asks how to place N queens on an N×N chessboard such that no two queens threaten each other.

The code begins by defining a subtype `Board_Index` which represents the indices of the rows and columns of the chessboard. It then defines a type `Board_Type` which is an array of booleans that represents the state of the chessboard, with `True` indicating that there is a queen in that cell and `False` indicating that there is no queen in that cell.

The `Clear_Board` procedure clears the chessboard by setting all cells to `False`.

The `No_Conflict` function checks if placing a queen in a given cell would result in a conflict with any other queens on the board. It does this by checking the row, column, and diagonals of the cell.

The `Place_Queen` procedure places a queen in a given cell by setting the corresponding element of the chessboard array to `True`.

The `Is_Solution` function checks if the current state of the chessboard is a solution to the N-queens problem. It does this by checking that there are no conflicts between any of the queens on the board.

The `Print_Board` procedure prints the chessboard to the console.

The `N_Queens_Core` function is the recursive core of the N-queens solver. It tries to place queens on the chessboard one row at a time. If it can find a solution where all queens are placed without any conflicts, it returns `True`. Otherwise, it returns `False`.

The `N_Queens` procedure calls the `N_Queens_Core` function to solve the N-queens problem. If a solution is found, it prints the solution to the console. Otherwise, it prints a message indicating that no solution was found.
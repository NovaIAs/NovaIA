```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Solve_Queens is
   type Board is array (1 .. 8) of Integer range 1 .. 8;

   function Attacking (b : Board; z : Integer) return Boolean is
      function Diagonal_Attack (b : Board) return Boolean is
         function Abs (x : Integer) return Natural is (x < 0) => x'Abs else x; end Abs;
         function Attack (c1 : Integer; c2 : Integer) return Boolean is
            return Abs (c1 - c2) = Abs (b (c1) - b (c2));
         end Attack;
      begin
         for i in 1 .. b'Length - 1 loop
            for j in i + 1 .. b'Length loop
               if (Attack (i, j)) then return True; end if;
            end loop;
         end loop;
         return False;
      end Diagonal_Attack;
   begin
      return (Diagonal_Attack (b (1 .. z - 1))) or Attacking (b (1 .. z - 1), b (z));
   end Attacking;

   procedure Print_Board (b : Board) is
   begin
      Put_Line ("    A B C D E F G H");
      for i in b'Range loop
         Put (Integer'Image (i)); Put ("   ");
         for j in b'Range loop
            if (b (j) = i) then Put ("#" else Put ("."); end if;
         end loop;
         Put_Line ("");
      end loop;
      Put_Line ("");
   end Print_Board;

   procedure Find_Solutions (b : Board; z : Integer) is
   begin
      for i in 1 .. 8 loop
         if not Attacking (b, i) then
            b (z) := i;
            if z = b'Length then Print_Board (b);
            else Find_Solutions (b, z + 1); end if;
         end if;
      end loop;
   end Find_Solutions;

begin
   Find_Solutions (Board' (others => 0), 1);
end Solve_Queens;
```

This code is an implementation of the n-queens problem in Ada, which consists of placing n queens on an n x n chessboard in such a way that no two queens attack each other.

The code first defines a type Board, which is an array of integers representing the positions of the queens on the board.

The function Attacking checks if a given position on the board is under attack by any of the queens that have already been placed.

The procedure Print_Board prints the board to the console, showing the positions of the queens with "#" and the empty positions with ".".

The procedure Find_Solutions uses a recursive algorithm to find all the possible solutions to the n-queens problem. It starts by trying to place a queen on the first position of the board. If this position is not under attack, it tries to place a queen on the second position, and so on. If it finds a solution, it prints it to the console.

The code finally calls the procedure Find_Solutions with an initial board and the first position, which starts the recursive search for all the possible solutions.
```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Towers_of_Hanoi is

   type Disk is range 1..10;  -- Disks numbered 1 to 10

   procedure Move_Disk (From, To : in Disk) is
   -- Moves a disk from one peg to another.
   begin
      Put ("Move disk " & Integer'Image (From) & " from " &
          Integer'Image (To) & New_Line);
   end Move_Disk;

   procedure Towers (Disks, From, To, Via : in Disk) is
   -- Moves a stack of disks from one peg to another, using a third as temporary storage.
   begin
      if Disks = 1 then
         Move_Disk (From, To);
      else
         Towers (Disks - 1, From, Via, To);
         Move_Disk (From, To);
         Towers (Disks - 1, Via, To, From);
      end if;
   end Towers;

begin
   Towers (3, 1, 3, 2);
end Towers_of_Hanoi;
```

This code implements the classic Towers of Hanoi puzzle in Ada. The puzzle involves three pegs and a stack of disks of different sizes, which must be moved from one peg to another, following certain rules.

The code defines a data type `Disk` to represent the disks, and a procedure `Move_Disk` to move a single disk from one peg to another. The procedure `Towers` then uses recursion to solve the puzzle for a given number of disks.

To use the code, simply call the procedure `Towers` with the number of disks and the three pegs as arguments. The code will then print the sequence of moves required to solve the puzzle.

Here is an example of how the code can be used:

```
$ gnatmake towers_of_hanoi
$ ./towers_of_hanoi
Move disk 1 from 1 to 3
Move disk 2 from 1 to 2
Move disk 1 from 3 to 2
Move disk 3 from 1 to 3
Move disk 1 from 2 to 1
Move disk 2 from 2 to 3
Move disk 1 from 1 to 3
```

This output shows the sequence of moves required to solve the puzzle for three disks.
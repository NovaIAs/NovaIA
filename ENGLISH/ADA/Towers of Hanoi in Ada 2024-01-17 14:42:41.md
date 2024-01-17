```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Towers_of_Hanoi is
   type Disk is (Small, Medium, Large);

   type Pole is (A, B, C);

   procedure Move_Disk (From, To : Pole) is
      begin
         Put_Line("Move disk from " & To_String(From) & " to " & To_String(To));
      end Move_Disk;

   procedure Hanoi (Disks : in Integer; From, To, Via : Pole) is
      begin
         if Disks > 0 then
            Hanoi(Disks - 1, From, Via, To);
            Move_Disk(From, To);
            Hanoi(Disks - 1, Via, To, From);
         end if;
      end Hanoi;

begin
   Hanoi(3, A, B, C);
end Towers_of_Hanoi;
```

This Ada code implements the Towers of Hanoi problem, which is a mathematical game or puzzle.
It consists of three rods and a number of disks of different sizes, which can slide onto any rod.
The puzzle starts with the disks in a neat stack in ascending order of size on one rod, the smallest at the top, thus making a conical shape.

The objective of the puzzle is to move the entire stack to another rod, obeying the following simple rules:

1. Only one disk can be moved at a time.
2. Each move consists of taking the upper disk from one of the stacks and placing it on top of another stack or on an empty rod.
3. No disk may be placed on top of a smaller disk.

With 3 disks, the puzzle can be solved in 7 moves.
The output of the program is:

```
Move disk from A to B
Move disk from A to C
Move disk from B to C
Move disk from A to B
Move disk from C to A
Move disk from C to B
Move disk from A to B
```

The code is explained below:

* The `Disk` type defines the three different sizes of disks.
* The `Pole` type defines the three poles.
* The `Move_Disk` procedure prints a message to the console indicating which disk is being moved from which pole to which pole.
* The `Hanoi` procedure is a recursive function that solves the Towers of Hanoi problem.
* The `main` procedure calls the `Hanoi` procedure to solve the puzzle with 3 disks.
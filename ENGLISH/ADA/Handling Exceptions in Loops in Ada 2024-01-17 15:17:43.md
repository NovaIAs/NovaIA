```ada
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Containers.Vectors;  use Ada.Containers.Vectors;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;

procedure Exceptions_In_Loops is

   type Vector_Of_Strings is array (Positive range <>) of String(1..31);
   Vector_Of_Strings := (("Ada", "is", "good"), ("but", "it", "can"), ("be", "confusing"));

   Strings : Vector_Of_Strings;
   Index  : Positive;
   Last   : Positive := Strings'Last;

begin
   for Index in Strings'Range loop
      Put_Line(Strings(Index));
   end loop;
   -- OK.

   Put_Line("Now with an exception");
   for Index in Strings'Range loop
      begin
         Put_Line(Strings(Index));
         if (Index = Last / 2) then
            raise Constraint_Error;  -- raise an exception at index 1
         end if;
      end;
   end loop;
   -- The exception will propagate out of the loop and be handled by the caller.
   -- Statements after the exception here will not be executed.

   Put_Line("Now with an exception handler");
   for Index in Strings'Range loop
      begin
         Put_Line(Strings(Index));
         if (Index = Last / 2) then
            raise Constraint_Error;
         end if;
      exception
         when Constraint_Error =>
            Put_Line("Exception was handled in the loop");
      end; -- loop
   end loop;
   -- The exception is handled locally in the loop.
   -- If no exception was raised, then the loop iterates as before.
   -- Statements after the exception handler here will be executed in any case.

   Put_Line("Now with an exception handler and loop exit");
   for Index in Strings'Range loop
      begin
         Put_Line(Strings(Index));
         if (Index = Last / 2) then
            raise Constraint_Error;
         end if;
      exception
         when Constraint_Error =>
            Put_Line("Exception was handled in the loop");
            exit;  -- exit from the loop
      end; -- loop
   end loop;
   -- The exception is handled locally in the loop.
   -- If an exception was raised, then the loop is exited.
   -- Statements after the exception handler here will be executed only if no exception was raised.

end Exceptions_In_Loops;
```

This code demonstrates how to handle exceptions in loops in Ada. It includes three examples:

1. An example where an exception is raised in the loop and is allowed to propagate out of the loop, terminating the program.
2. An example where an exception is raised in the loop and is handled locally in the loop, allowing the loop to continue iterating.
3. An example where an exception is raised in the loop and is handled locally in the loop, causing the loop to exit immediately.

The code includes comments to explain each part of the code and the expected behavior.
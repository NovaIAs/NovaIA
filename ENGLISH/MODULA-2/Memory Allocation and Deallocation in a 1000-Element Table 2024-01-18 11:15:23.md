```modula-2
MODULE Main;

FROM Terminal IMPORT
   WriteString, WriteLn;

TYPE
   Storage = RECORD
      value: ARRAY 0..1000 OF INTEGER;
      used : ARRAY 0..1000 OF BOOLEAN;
   END;

VAR
   table: Storage;
   entries: INTEGER;

PROCEDURE InitTable;
VAR
   i: INTEGER;
BEGIN
   FOR i := 0 TO 1000 DO
      table.used[i] := FALSE
   END
END InitTable;

PROCEDURE PrintTable;
VAR
   i: INTEGER;
BEGIN
   FOR i := 0 TO 1000 DO
      IF table.used[i] THEN
         WriteString((table.value[i]:2) : 0)
      END
   END;
   WriteLn
END PrintTable;

PROCEDURE Allocate(VAR p: POINTER TO INTEGER): BOOLEAN;
BEGIN
   p := NIL;
   FOR p := @table.value[0] TO @table.value[1000] DO
      IF NOT table.used[p - @table.value[0]] THEN
         table.used[p - @table.value[0]] := TRUE;
         EXIT
      END
   END;
   IF p = NIL THEN RETURN FALSE END
END Allocate;

PROCEDURE Free(p: POINTER TO INTEGER);
BEGIN
   table.used[p - @table.value[0]] := FALSE;
   p := NIL
END Free;

BEGIN
   entries := 0;
   InitTable;
   PrintTable;
   WHILE Allocate(p) DO
      table.value[p - @table.value[0]] := entries;
      entries := entries + 1;
      PrintTable
   END;
   PrintTable;
   Free(p);
   PrintTable;
   Free(p);
   Free(p)
END Main.
```

This code creates a table of 1000 integers, initially all set to 0. It then repeatedly allocates blocks of 100 integers from the table, setting the values in each block to a unique number. It prints the table after each allocation, showing the allocated blocks (non-zero values) and the free blocks (zero values).

It then frees the last three allocated blocks, printing the table after each free operation.

The code uses a custom data structure called `Storage` to store the table. The `Storage` record contains two arrays: `value` and `used`. The `value` array stores the integer values in the table, and the `used` array indicates which values are currently allocated.

The code also uses several procedures to manipulate the table. The `InitTable` procedure initializes the table to all 0s. The `PrintTable` procedure prints the table, showing the allocated blocks and the free blocks.

The `Allocate` procedure allocates a block of 100 integers from the table. It returns a pointer to the first integer in the block, or NIL if there are no free blocks.

The `Free` procedure frees a block of 100 integers from the table. It takes a pointer to the first integer in the block as an argument.

The main program first initializes the table, then repeatedly allocates blocks of 100 integers from the table until there are no more free blocks. It then prints the table, frees the last three allocated blocks, and prints the table again.
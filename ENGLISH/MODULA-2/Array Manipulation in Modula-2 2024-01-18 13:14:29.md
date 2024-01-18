```modula-2
MODULE Main;
FROM Terminal IMPORT WriteLn;

TYPE
  SomeType = ARRAY 0 .. 10 OF INTEGER;

PROCEDURE InitArray(VAR array: SomeType);
VAR
  index: CARDINAL;
BEGIN
  FOR index := 0 TO HIGH(array) DO
    array[index] := 0;
  END;
END InitArray;

PROCEDURE PrintArray(VAR array: SomeType);
VAR
  index: CARDINAL;
BEGIN
  FOR index := 0 TO HIGH(array) DO
    WriteLn(array[index]);
  END;
END PrintArray;

PROCEDURE Main;
VAR
  array: SomeType;
BEGIN
  InitArray(array);
  PrintArray(array);
END Main.
```
This code declares a module named `Main` that defines a data type `SomeType` that is an array of 11 integers.
It then defines two procedures `InitArray` and `PrintArray`.
The first procedure initialises the array with all zeros, while the second prints the contents of the array to the terminal.
The main program `Main` creates an instance of `SomeType` and calls `InitArray` to initialise it with zeros, and then calls `PrintArray` to print the contents of the array.
When running the program, the output will be:

```
0
0
0
0
0
0
0
0
0
0
0
```
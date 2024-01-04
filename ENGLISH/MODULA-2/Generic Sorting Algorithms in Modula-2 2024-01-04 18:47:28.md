```modula-2
MODULE GenericSort;
FROM FormatString IMPORT WriteF, WriteLn;
FROM Terminal IMPORT WriteString;
FROM Terminal IMPORT ReadChar;
FROM Strings IMPORT String, Chr, Length, Concat, SubString;
FROM Strings IMPORT Index, Size, CopyString, MakeLower, MakeUpper;
FROM Strings IMPORT CompareStrings, CompareStringsI;
FROM Sequences IMPORT Array;
FROM Sequences IMPORT Sort;
FROM Sequences IMPORT Select;

PROCEDURE PrintElements[T:ARRAY OF ANY];
VAR i:CARDINAL;
BEGIN
  WriteString("[");
  FOR i:=LOW(T) TO HIGH(T) DO
    WriteF("%g, ", T[i]);
  WriteString("]");
  WriteLn;
END PrintElements;

PROCEDURE IsEven(x:INTEGER):BOOLEAN;
BEGIN
  RETURN (x MOD 2) = 0;
END IsEven;

PROCEDURE CompareArrays(a,b:ARRAY OF INTEGER):INTEGER;
VAR i:CARDINAL;
BEGIN
  FOR i:=LOW(a) TO HIGH(a) DO
    IF a[i] < b[i] THEN
      RETURN -1;
    ELSIF a[i] > b[i] THEN
      RETURN 1;
  END;
  RETURN 0;
END CompareArrays;

PROCEDURE Example1;
VAR a:ARRAY[1..5] OF INTEGER;
BEGIN
  a[1]:=1; a[2]:=3; a[3]:=4; a[4]:=2; a[5]:=5;
  WriteString("Original array: ");
  PrintElements(a);
  Sort(a);
  WriteString("Sorted array: ");
  PrintElements(a);
END Example1;

PROCEDURE Example2;
VAR a:ARRAY[1..5] OF INTEGER;
BEGIN
  a[1]:=1; a[2]:=4; a[3]:=2; a[4]:=3; a[5]:=5;
  WriteString("Original array: ");
  PrintElements(a);
  Sort(a,CompareArrays);
  WriteString("Sorted array: ");
  PrintElements(a);
END Example2;

PROCEDURE Example3;
VAR a:ARRAY[1..5] OF STRING;
BEGIN
  a[1]:="Alice"; a[2]:="Bob"; a[3]:="Eve"; a[4]:="Charlie"; a[5]:="David";
  WriteString("Original array: ");
  PrintElements(a);
  Sort(a);
  WriteString("Sorted array: ");
  PrintElements(a);
END Example3;

PROCEDURE Example4;
VAR a:ARRAY[1..5] OF STRING;
BEGIN
  a[1]:="Alice"; a[2]:="Bob"; a[3]:="Eve"; a[4]:="Charlie"; a[5]:="David";
  WriteString("Original array: ");
  PrintElements(a);
  Sort(a,CompareStrings);
  WriteString("Sorted array: ");
  PrintElements(a);
END Example4;

PROCEDURE Example5;
TYPE Person = RECORD name:STRING; age:INTEGER; END;
VAR a:ARRAY[1..5] OF Person;
BEGIN
  a[1].name:="Alice"; a[1].age:=20;
  a[2].name:="Bob"; a[2].age:=25;
  a[3].name:="Eve"; a[3].age:=30;
  a[4].name:="Charlie"; a[4].age:=35;
  a[5].name:="David"; a[5].age:=40;
  WriteString("Original array: ");
  PrintElements(a);
  Sort(a,PROCEDURE(a,b:Person):INTEGER;
          BEGIN
            RETURN CompareStrings(a.name,b.name);
          END);
  WriteString("Sorted array: ");
  PrintElements(a);
END Example5;

BEGIN
  Example1;
  Example2;
  Example3;
  Example4;
  Example5;
  ReadChar;
END GenericSort.
```

This Modula-2 code demonstrates generic sorting algorithms and how they can be used to sort different types of data, including arrays of integers, strings, and records.

The code consists of several procedures and functions:

- `PrintElements` is a generic procedure that prints the elements of an array.

- `IsEven` is a function that checks if an integer is even.

- `CompareArrays` is a function that compares two arrays of integers and returns -1, 0, or 1 depending on whether the first array is less than, equal to, or greater than the second array.

- `Example1` demonstrates sorting an array of integers using the built-in `Sort` procedure.

- `Example2` demonstrates sorting an array of integers using a custom comparison function.

- `Example3` demonstrates sorting an array of strings using the built-in `Sort` procedure.

- `Example4` demonstrates sorting an array of strings using a custom comparison function.

- `Example5` demonstrates sorting an array of records using a custom comparison function.

The `Sort` procedure takes two arguments: the array to be sorted and a comparison function. The comparison function takes two elements of the array as arguments and returns -1, 0, or 1 depending on whether the first element is less than, equal to, or greater than the second element.

The code uses the `ReadChar` procedure to pause the program and wait for the user to press a key before exiting.
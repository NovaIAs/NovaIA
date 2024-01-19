```modula-2
MODULE ComplexModule;
FROM FormatString IMPORT FormatString, WriteString, WriteLn;
FROM Strings IMPORT Length,Copy,Equal;

TYPE String = ARRAY[0..80-1] OF CHAR;
TYPE StringList = ARRAY[0..100-1] OF String;

PROCEDURE InitList(VAR List: StringList);
VAR Result: String;
BEGIN
  WriteString("Enter a list of 10 strings (empty string to terminate): ");
  FOR i:= 0 TO 100-1 DO
    ReadString(Result);
    IF Length(Result) = 0 THEN
      List[i] := "";
      LEAVE
    ELSE
      List[i] := Copy(Result,1,Length(Result)-1)
    FI
  OD
END InitList;

PROCEDURE GetLongest(List: StringList; VAR Longest: String; VAR Index: INTEGER);
VAR Len: INTEGER;
BEGIN
  Longest := ""; Index := -1;
  Len := 0;
  FOR i:= 0 TO 100-1 DO
    IF List[i] = "" THEN
      LEAVE
    ELIF Length(List[i]) > Len THEN
      Len := Length(List[i]);
      Longest := List[i];
      Index:= i
    ELIF Length(List[i]) = Len AND
         Equal(List[i],Longest) THEN
      Longest := Longest + ", " + List[i]
      Index:= i
    FI
  OD
END GetLongest;

PROCEDURE FindIndex(List: StringList; Search: String; VAR Index: INTEGER);
VAR Found: BOOLEAN;
BEGIN
  Found := FALSE; Index := -1;
  FOR i:= 0 TO 100-1 DO
    IF List[i] = Search THEN
      Found := TRUE;
      Index:= i;
      LEAVE
    FI
  OD;
  IF NOT Found THEN
    WriteString("String \"");
    WriteString(Search);
    WriteString("\" not found in list.")
  FI
END FindIndex;

PROCEDURE PrintIndex(Index: INTEGER);
BEGIN
  IF Index < 0 THEN
    WriteString("Not in list")
  ELSE
    FormatString(S,"String found at index %d",[Index])
  FI
END PrintIndex;

VAR List: StringList;
VAR Longest: String; Index: INTEGER;
VAR Search: String;

BEGIN
  InitList(List);
  GetLongest(List,Longest,Index);
  WriteString("Longest string(s): \"");
  WriteString(Longest);
  WriteString("\"\n");

  WriteString("Enter a string to search for: ");
  ReadString(Search);
  FindIndex(List,Search,Index);
  PrintIndex(Index);
  WriteString("\n")
END ComplexModule.
```
This program performs a series of complex operations using arrays of strings, string manipulation functions, and conditional statements in MODULA-2. Here's what the code does:

1. It initializes an array of strings (`List`) with a maximum capacity of 100 strings.

2. It uses a loop to prompt the user to enter a list of 10 strings. The user can enter an empty string to terminate the input.

3. The program calls the `GetLongest` procedure to find the longest string or strings (`Longest`) in the list (`List`) and the index (`Index`) of that string in the list. It considers strings with equal lengths to be equally long.

4. It displays the longest string(s) and their index.

5. The program prompts the user to enter a string (`Search`) to search for in the list.

6. It calls the `FindIndex` procedure to search for the user's input (`Search`) in the list (`List`) and returns the index (`Index`) of the first occurrence of the string.

7. Finally, it uses the `PrintIndex` procedure to display the index where the searched string is found in the list. If the string is not found, it notifies the user.

This program demonstrates the use of various string manipulation functions, looping constructs, conditional statements, and procedures in MODULA-2 to perform a series of operations on a list of strings.
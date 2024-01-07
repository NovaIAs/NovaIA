```modula-2
MODULE Main;
FROM Terminal IMPORT WriteString, WriteLn, ReadChar;

TYPE
  Table = ARRAY [1..20] OF INTEGER;
  List = POINTER TO Record
    next: List;
    value: INTEGER;
  END;

VAR
  table: Table;
  listHead: List;

PROCEDURE InitTable;
VAR
  i: CARDINAL;
BEGIN
  FOR i := 0 TO HIGH(table) DO
    table[i] := 0;
  END;
END InitTable;

PROCEDURE InitList;
VAR
  i: CARDINAL;
  p: List;
BEGIN
  listHead := NIL;
  FOR i := 0 TO HIGH(table) DO
    p := List(NEW(Record
      next: NIL,
      value: i
    ));
    p^.next := listHead;
    listHead := p;
  END;
END InitList;

PROCEDURE PrintTable;
VAR
  i: CARDINAL;
BEGIN
  FOR i := 0 TO HIGH(table) DO
    WriteString(IntToStr(table[i], 10));
    WriteString(", ");
  END;
  WriteLn;
END PrintTable;

PROCEDURE PrintList;
VAR
  p: List;
BEGIN
  p := listHead;
  WHILE p # NIL DO
    WriteString(IntToStr(p^.value, 10));
    WriteString(", ");
    p := p^.next;
  END;
  WriteLn;
END PrintList;

PROCEDURE IncrementTable;
VAR
  i: CARDINAL;
BEGIN
  FOR i := 0 TO HIGH(table) DO
    table[i] := table[i] + 1;
  END;
END IncrementTable;

PROCEDURE IncrementList;
VAR
  p: List;
BEGIN
  p := listHead;
  WHILE p # NIL DO
    p^.value := p^.value + 1;
    p := p^.next;
  END;
END IncrementList;

BEGIN
  InitTable;
  InitList;
  PrintTable;
  PrintList;
  IncrementTable;
  IncrementList;
  PrintTable;
  PrintList;

  ReadChar;
END Main.
```

This code initializes a table and a linked list, prints their values, increments them, and prints them again.

The table is declared as a static array of integers. The linked list is declared as a pointer to a record that contains a pointer to the next element in the list and an integer value.

The `InitTable` procedure initializes the table to all zeros.

The `InitList` procedure initializes the linked list by creating a new node for each element in the table and linking them together.

The `PrintTable` procedure prints the values of the table.

The `PrintList` procedure prints the values of the linked list.

The `IncrementTable` procedure increments the value of each element in the table by one.

The `IncrementList` procedure increments the value of each element in the linked list by one.

The `Main` procedure calls the initialization procedures, prints the table and linked list, increments them, and prints them again.

Finally, the `ReadChar` procedure waits for the user to press a key before terminating the program.
```modula-2

MODULE ComplexCode;
FROM Terminal IMPORT WriteString, WriteLn;

TYPE
  IntSet = ARRAY[0..100] OF BOOLEAN;

  TYPE CharSet = ARRAY[CHAR] OF BOOLEAN;

PROCEDURE PrintIntSet(s: IntSet);
VAR
  i: CARDINAL;
BEGIN
  WriteString("IntSet: ");
  FOR i := 0 TO 100 DO
    IF s[i] THEN
      WriteString(IntToString(i), " ");
    END;
  WriteLn;
END PrintIntSet;

PROCEDURE PrintCharSet(s: CharSet);
VAR
  c: CHAR;
BEGIN
  WriteString("CharSet: ");
  FOR c := 'A' TO 'Z' DO
    IF s[c] THEN
      WriteChar(c);
    END;
  WriteLn;
END PrintCharSet;

PROCEDURE Main;
VAR
  intSet: IntSet;
  charSet: CharSet;
BEGIN
  intSet := [];
  FOR i := 0 TO 100 DO
    intSet[i] := (i MOD 2 = 0);
  END;
  PrintIntSet(intSet);

  charSet := [];
  FOR c := 'A' TO 'Z' DO
    charSet[c] := (c >= 'M');
  END;
  PrintCharSet(charSet);
END Main.

```

This code defines two data types, IntSet and CharSet, which are arrays of booleans. The arrays are indexed by integers in the range 0 to 100 for IntSet and by characters in the range 'A' to 'Z' for CharSet. The values in the arrays indicate whether the corresponding integer or character is included in the set.

The code also defines two procedures, PrintIntSet and PrintCharSet, which are used to print the contents of IntSet and CharSet arrays, respectively. The procedures iterate over the arrays and print the indices or characters of the elements that are included in the sets.

Finally, the code defines a procedure, Main, which is the entry point for the program. The procedure creates instances of IntSet and CharSet and initializes them with some values. It then calls the PrintIntSet and PrintCharSet procedures to print the contents of the sets.

When the program is run, it will print the following output:

```
IntSet: 0 2 4 6 8 10 12 14 16 18 20 ... 98 100
CharSet: M N O P Q R S T U V W X Y Z
```

This output shows that the IntSet array contains all the even integers from 0 to 100, and the CharSet array contains all the characters from 'M' to 'Z'.
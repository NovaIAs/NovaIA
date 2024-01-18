```modula-2
MODULE FruitSort;
FROM Terminal IMPORT WriteString, WriteLn, ReadChar;

PROCEDURE FruitSortProcedure(IFruitBox: ARRAY OF ARRAY OF STRING);
VAR
  i, j, k: CARDINAL;
  Fruit, Temp: STRING;
BEGIN
  FOR i := 1 TO HIGH(IFruitBox) DO
    FOR j := 1 TO HIGH(IFruitBox[i]) DO
      FOR k := 1 TO HIGH(IFruitBox[i]) - 1 DO
        IF IFruitBox[i][k] > IFruitBox[i][k + 1] THEN
          Temp := IFruitBox[i][k];
          IFruitBox[i][k] := IFruitBox[i][k + 1];
          IFruitBox[i][k + 1] := Temp;
        END;
      END;
    END;
  END;
END FruitSortProcedure;

PROCEDURE PrintFruitBox(IFruitBox: ARRAY OF ARRAY OF STRING);
VAR
  i, j: CARDINAL;
BEGIN
  FOR i := 1 TO HIGH(IFruitBox) DO
    FOR j := 1 TO HIGH(IFruitBox[i]) DO
      WriteString(IFruitBox[i][j]);
      WriteString(", ");
    END;
    WriteLn;
  END;
END PrintFruitBox;

VAR
  FruitBox: ARRAY[1 .. 3] OF ARRAY[1 .. 5] OF STRING;
  Answer: CHAR;
BEGIN
  WriteString("Enter the fruits (separated by commas): ");
  ReadString(FruitBox[1]);
  WriteString("Enter the fruits (separated by commas): ");
  ReadString(FruitBox[2]);
  WriteString("Enter the fruits (separated by commas): ");
  ReadString(FruitBox[3]);

  FruitSortProcedure(FruitBox);

  WriteString("Sorted fruits:");
  WriteLn;
  PrintFruitBox(FruitBox);

  ReadChar(Answer);
END FruitSort.
```

This Modula-2 program sorts the fruits entered by the user into three different boxes and then prints the sorted fruits.

The program first defines a procedure `FruitSortProcedure` which takes an array of arrays of strings as its parameter. This procedure sorts the fruits in each box in alphabetical order.

The program then defines a procedure `PrintFruitBox` which takes an array of arrays of strings as its parameter. This procedure prints the fruits in each box, separated by commas.

The program then declares a variable `FruitBox` which is an array of three arrays of strings. Each array represents a box of fruits.

The program then prompts the user to enter the fruits for each box. The user enters the fruits separated by commas.

The program then calls the `FruitSortProcedure` procedure to sort the fruits in each box.

The program then calls the `PrintFruitBox` procedure to print the sorted fruits.

Finally, the program prompts the user to press a key to exit.
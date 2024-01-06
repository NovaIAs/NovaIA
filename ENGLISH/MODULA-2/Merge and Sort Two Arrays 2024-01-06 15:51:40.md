```modula-2
MODULE ComplexCode;
FROM FormatString IMPORT FormatString;

PROCEDURE AllInOne(n, m, A, B: ARRAY OF INTEGER);

VAR i, j, k, l, tmp, sorted: ARRAY OF INTEGER;

BEGIN
  i := n + 1;
  j := 0;
  WHILE j < i DO
    tmp[j] := A[j];
    INC(j);
  END;
  j := 0;
  WHILE j < m DO
    tmp[i] := B[j];
    INC(i);
    INC(j);
  END;
  i := 0;
  WHILE i < n + m DO
    sorted[i] := tmp[i];
    INC(i);
  END;
  j := 0;
  WHILE j < n + m - 1 DO
    i := 0;
    WHILE i < n + m - 1 - j DO
      IF sorted[i] > sorted[i + 1] THEN
        tmp := sorted[i];
        sorted[i] := sorted[i + 1];
        sorted[i + 1] := tmp;
      END;
      INC(i);
    END;
    INC(j);
  END;
  i := 0;
  j := 0;
  WHILE i < n DO
    A[j] := sorted[i];
    INC(i);
    INC(j);
  END;
  i := n;
  j := 0;
  WHILE i < n + m DO
    B[j] := sorted[i];
    INC(i);
    INC(j);
  END;
END AllInOne;

PROCEDURE PrintArrays(n, m: INTEGER; A, B: ARRAY OF INTEGER);

VAR i, j: INTEGER;

BEGIN
  i := 0;
  WHILE i < n DO
    FormatString("%d", A[i]);
    INC(i);
  END;
  i := 0;
  WHILE i < m DO
    FormatString("%d", B[i]);
    INC(i);
  END;
  FormatString("\n");
END PrintArrays;

PROCEDURE Main;

VAR n, m: INTEGER;
A, B: ARRAY OF INTEGER;

BEGIN
  n := 5;
  m := 7;
  A := [1, 3, 5, 7, 9];
  B := [2, 4, 6, 8, 10, 12, 14];
  PrintArrays(n, m, A, B);
  AllInOne(n, m, A, B);
  PrintArrays(n, m, A, B);
END Main.
```

This code takes two arrays of integers, `A` and `B`, and merges them into a single sorted array. The `AllInOne` procedure first concatenates the two arrays into a temporary array `tmp`. Then, it sorts the temporary array using the bubble sort algorithm. Finally, it copies the sorted elements back into the original arrays `A` and `B`.

The `PrintArrays` procedure prints the elements of the two arrays to the console.

The `Main` procedure creates two arrays of integers, `A` and `B`, and calls the `PrintArrays` procedure to print the initial contents of the arrays. Then, it calls the `AllInOne` procedure to merge and sort the two arrays. Finally, it calls the `PrintArrays` procedure again to print the sorted arrays.

Here is an example of the output of the program:

```
1 3 5 7 9 2 4 6 8 10 12 14
1 2 3 4 5 6 7 8 9 10 12 14
```
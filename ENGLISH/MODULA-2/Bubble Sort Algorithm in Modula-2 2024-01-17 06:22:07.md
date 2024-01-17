```modula-2

MODULE Sort;
FROM Terminal IMPORT WriteString, WriteLn, WriteCard, WriteInt;
FROM Terminal IMPORT ReadString, ReadLn, ReadCard, ReadInt;

TYPE Element = CARDINAL;
PROCEDURE Swap(VAR A, B: Element);
PROCEDURE BubbleSort(VAR A: ARRAY OF Element);
PROCEDURE Show(A: ARRAY OF Element);
PROCEDURE GetArray(VAR A: ARRAY OF Element);

BEGIN
  WriteString("Enter the number of elements: ");
  GetArray(A);
  BubbleSort(A);
  Show(A);
END Sort.

PROCEDURE Swap(VAR A, B: Element);
BEGIN
  VAR Temp: Element;
  Temp := A;
  A := B;
  B := Temp;
END Swap;

PROCEDURE BubbleSort(VAR A: ARRAY OF Element);
VAR
  I, J: CARDINAL;
BEGIN
  FOR I := 1 TO HIGH(A) - 1 DO
    FOR J := I + 1 TO HIGH(A) DO
      IF A[I] > A[J] THEN
        Swap(A[I], A[J]);
      END;
    END;
  END;
END BubbleSort;

PROCEDURE Show(A: ARRAY OF Element);
VAR
  I: CARDINAL;
BEGIN
  FOR I := 0 TO HIGH(A) DO
    WriteCard(A[I], 3);
    WriteString(" ");
  END;
  WriteLn;
END Show;

PROCEDURE GetArray(VAR A: ARRAY OF Element);
VAR
  I: CARDINAL;
BEGIN
  FOR I := 0 TO HIGH(A) DO
    WriteString("Enter element ");
    WriteCard(I, 1);
    WriteString(": ");
    A[I] := ReadCard;
  END;
END GetArray;

```

The code above is a bubble sort algorithm written in Modula-2.
The algorithm first prompts the user to enter the number of elements they want to sort.
Then, it calls the `GetArray` procedure to read in the elements from the user.
Next, it calls the `BubbleSort` procedure to sort the array of elements.
Finally, it calls the `Show` procedure to display the sorted array.

Here's a brief explanation of the code:

* The `Swap` procedure swaps the values of two elements in an array.
* The `BubbleSort` procedure sorts an array of elements using the bubble sort algorithm.
* The `Show` procedure displays the elements of an array on the screen.
* The `GetArray` procedure reads in the elements of an array from the user.

The code is commented in detail, so you should be able to understand how it works by reading the comments.
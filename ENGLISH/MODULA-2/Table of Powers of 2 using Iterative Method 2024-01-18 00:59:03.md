```modula-2

MODULE POWER;
FROM Terminal IMPORT WriteString, WriteLn, ReadInt, WriteCardinal;
FROM FormatStrings IMPORT "%d";

TYPE Table = ARRAY [1..10] OF INTEGER; (* table of size 10 *)

PROCEDURE Tweak(p: Table);
  VAR temp: INTEGER;
  BEGIN
    FOR i := 1 TO HIGH(p) - 1 DO
      temp := p[i];
      p[i] := p[HIGH(p) - i];
      p[HIGH(p) - i] := temp;
    END;
  END Tweak;

PROCEDURE PrintArray(a: Table);
  VAR i: INTEGER;
  BEGIN
    FOR i := 1 TO HIGH(a) DO
      WriteCardinal(a[i], "%d");
      WriteString(", ");
    END;
    WriteLn;
  END PrintArray;

VAR test1, test2: Table;
VAR power: INTEGER;

BEGIN
  WriteString("Enter a power: ");
  power := ReadInt;

  (* initialize first table *)
  FOR i := 1 TO HIGH(test1) DO
    test1[i] := i;
  END;

  PrintArray(test1);

  (* result is stored in test2 *)
  Tweak(test2);

  FOR i := 1 TO power DO
    Tweak(test1);
    Tweak(test2);
  END;

  PrintArray(test2);
END POWER.

```

This Modula-2 program takes a number as input and generates a table of powers of 2. It utilizes an iterative method to construct the table of powers, and then applies a transformation function called "Tweak" to modify the table. The function Tweak reverses the order of elements in the table.

Here's a breakdown of the code:

1. Modules and Imports:

   - The program starts by importing necessary modules:
     - Terminal: Contains functions for input and output operations.
     - FormatStrings: Provides formatted output.

2. Data Types:

   - Table: An array type representing a table of integers with a size of 10.

3. Procedures:

   - Tweak: This procedure takes a table as input and reverses the order of its elements.

   - PrintArray: Prints the elements of a table to the console.

4. Main Execution:

   a. Variable Declarations:
      - test1 and test2: Two table variables to store different versions of the table.
      - power: An integer variable to store the exponent.

   b. User Input:
      - The user is prompted to enter a power using ReadInt.

   c. Table Initialization:
      - test1 is initialized with values from 1 to 10.

   d. Printing the Original Table:
      - The initial table test1 is printed using PrintArray.

   e. Applying Tweak:
      - The Tweak procedure is applied to initialize test2.

   f. Calculating Powers:
      - Using a loop, the Tweak procedure is applied to test1 and test2 repeatedly, effectively generating the table of powers.

   g. Printing the Final Table:
      - The final version of the table, stored in test2, is printed using PrintArray.
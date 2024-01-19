```modula-2
MODULE Fibonacci;
FROM Terminal IMPORT WriteCard, WriteString, WriteLn;
FROM Terminal IMPORT ReadChar;

PROCEDURE Fibonacci(n: INTEGER): INTEGER;
VAR
  end: ARRAY [0..1] OF INTEGER;
BEGIN
  end[0] := 0;
  end[1] := 1;
  FOR i := 2 TO n DO
    end[i] := end[i-1] + end[i-2]
  END;
  RETURN end[n]
END Fibonacci;

PROCEDURE Main;
VAR
  number: INTEGER;
BEGIN
  WriteString("Enter the number to calculate the Fibonacci series: ");
  ReadCard(number);
  WriteLn("The Fibonacci number at position ", number, " is ", Fibonacci(number));
END Main.
```

Explanation:

- The module 'Fibonacci' is defined, which encapsulates the functionality for calculating Fibonacci numbers and the main program.

- The 'Fibonacci' procedure is defined, which takes an integer 'n' as input and returns the nth Fibonacci number.

- Inside the 'Fibonacci' procedure, an array 'end' is defined to store the Fibonacci numbers for positions 0 and 1. The Fibonacci series starts with 0 and 1, so these values are initialized.

- A 'FOR' loop is used to calculate the Fibonacci numbers for positions 2 to 'n'. Each Fibonacci number is calculated by adding the previous two Fibonacci numbers.

- The final Fibonacci number at position 'n' is returned.

- The 'Main' procedure is defined, which is the entry point of the program.

- In the 'Main' procedure, the user is prompted to enter an integer number using 'WriteString' and 'ReadCard'.

- The 'Fibonacci' procedure is called with the entered number to calculate the nth Fibonacci number.

- The result is displayed using 'WriteLn'.
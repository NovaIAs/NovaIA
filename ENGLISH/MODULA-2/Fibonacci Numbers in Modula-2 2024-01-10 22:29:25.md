```modula-2
MODULE Fib;
FROM FormatString IMPORT FormatString;

VAR
  fib : ARRAY 100 OF CARDINAL;

PROCEDURE InitFib;
VAR
  i : CARDINAL;
BEGIN
  fib[0] := 0;
  fib[1] := 1;
  FOR i := 2 TO 99 DO
    fib[i] := fib[i - 1] + fib[i - 2]
  END;
END InitFib;

PROCEDURE PrintFib;
VAR
  i : CARDINAL;
BEGIN
  FOR i := 0 TO 99 DO
    FormatString("Fibonacci number %2d is %6d", i, fib[i])
  END;
END PrintFib;

BEGIN
  InitFib;
  PrintFib;
END Fib.
```

This Modula-2 code calculates and prints the first 100 Fibonacci numbers.

The Fibonacci sequence is a series of numbers in which each number is the sum of the two preceding ones, typically starting with 0 and 1. The sequence goes like this:

```
0, 1, 1, 2, 3, 5, 8, 13, 21, 34, ...
```

The code first defines an array `fib` of 100 cardinal numbers. Then, the procedure `InitFib` initializes the array with the first two Fibonacci numbers (0 and 1). It then calculates the remaining Fibonacci numbers using a loop that iterates from 2 to 99. In each iteration, the value of `fib[i]` is calculated by adding the values of `fib[i - 1]` and `fib[i - 2]`.

The procedure `PrintFib` prints the first 100 Fibonacci numbers using a loop that iterates from 0 to 99. In each iteration, it uses the `FormatString` procedure to format the Fibonacci number and print it to the console.

The code starts by calling the `InitFib` procedure to initialize the `fib` array. Then, it calls the `PrintFib` procedure to print the first 100 Fibonacci numbers.
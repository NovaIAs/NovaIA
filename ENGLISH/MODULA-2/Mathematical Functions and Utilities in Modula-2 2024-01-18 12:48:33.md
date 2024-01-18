```modula-2
MODULE LargeAndDifferentiatedCode;
FROM Terminal IMPORT WriteString, WriteLn;

PROCEDURE Factorial(n: CARDINAL): CARDINAL;
VAR i, f: CARDINAL;
BEGIN
  f := 1;
  FOR i := 2 TO n DO
    f := f * i
  END;
  RETURN f
END Factorial;

PROCEDURE Fibonacci(n: CARDINAL): CARDINAL;
VAR f0, f1, f: CARDINAL;
BEGIN
  f0 := 0;
  f1 := 1;
  FOR i := 3 TO n DO
    f := f0 + f1;
    f0 := f1;
    f1 := f
  END;
  RETURN f
END Fibonacci;

PROCEDURE PrintFactors(n: CARDINAL);
VAR i: CARDINAL;
BEGIN
  WriteString("Factors of ");
  WriteCard(n, 0);
  WriteString(": ");
  FOR i := 1 TO n DIV 2 DO
    IF n MOD i = 0 THEN
      WriteCard(i, 0);
      WriteString(" ")
    END
  END;
  WriteString(n);
  WriteLn
END PrintFactors;

PROCEDURE PrintPrimes(n: CARDINAL);
VAR i, j, isPrime: BOOLEAN;
BEGIN
  WriteString("Primes up to ");
  WriteCard(n, 0);
  WriteString(": ");
  FOR i := 2 TO n DO
    isPrime := TRUE;
    FOR j := 2 TO i DIV 2 DO
      IF i MOD j = 0 THEN
        isPrime := FALSE
      END
    END;
    IF isPrime THEN
      WriteCard(i, 0);
      WriteString(" ")
    END
  END;
  WriteLn
END PrintPrimes;

PROCEDURE PrintPerfectNumbers(n: CARDINAL);
VAR i, sum, j: CARDINAL;
BEGIN
  WriteString("Perfect numbers up to ");
  WriteCard(n, 0);
  WriteString(": ");
  FOR i := 2 TO n DO
    sum := 0;
    FOR j := 1 TO i DIV 2 DO
      IF i MOD j = 0 THEN
        sum := sum + j
      END
    END;
    IF sum = i THEN
      WriteCard(i, 0);
      WriteString(" ")
    END
  END;
  WriteLn
END PrintPerfectNumbers;

PROCEDURE Main;
VAR choice, n: CARDINAL;
BEGIN
  REPEAT
    WriteString("Enter a number (0 to exit): ");
    ReadCard(n);
    IF n > 0 THEN
      WriteString("1. Factorial\n");
      WriteString("2. Fibonacci\n");
      WriteString("3. Factors\n");
      WriteString("4. Primes\n");
      WriteString("5. Perfect numbers\n");
      WriteString("Choice: ");
      ReadCard(choice);
      CASE choice OF
        1: WriteString("Factorial of ");
            WriteCard(n, 0);
            WriteString(" is ");
            WriteCard(Factorial(n), 0);
            WriteLn;
        2: WriteString("Fibonacci of ");
            WriteCard(n, 0);
            WriteString(" is ");
            WriteCard(Fibonacci(n), 0);
            WriteLn;
        3: PrintFactors(n);
        4: PrintPrimes(n);
        5: PrintPerfectNumbers(n);
        ELSE
          WriteString("Invalid choice");
          WriteLn
      END
    END
  UNTIL n = 0
END Main.
```

This code is a collection of mathematical functions and utilities that demonstrate a variety of programming concepts in Modula-2. It includes functions for calculating factorials, Fibonacci numbers, finding factors of a number, generating prime numbers, and finding perfect numbers. The program also includes a simple command-line interface that allows the user to select which function to execute and provides the results.

Here is a breakdown of the code:

1. **Factorial Function:** The `Factorial` function calculates the factorial of a given number `n` by multiplying all positive integers up to `n`.

2. **Fibonacci Function:** The `Fibonacci` function calculates the `n`th Fibonacci number using a recursive approach.

3. **PrintFactors Procedure:** The `PrintFactors` procedure prints all the factors of a given number `n`.

4. **PrintPrimes Procedure:** The `PrintPrimes` procedure generates and prints all prime numbers up to a given number `n`.

5. **PrintPerfectNumbers Procedure:** The `PrintPerfectNumbers` procedure finds and prints all perfect numbers up to a given number `n`. A perfect number is a positive integer that is equal to the sum of its proper divisors (excluding the number itself).

6. **Main Procedure:** The `Main` procedure is the entry point of the program. It displays a menu of options to the user, allowing them to choose which function to execute. Based on the user's choice, it calls the appropriate function and displays the results.

This code demonstrates several programming techniques, including:

- Functions with parameters and return values
- Loops (FOR and WHILE)
- Conditional statements (IF-THEN-ELSE)
- CASE statements for multiple choices
- Input and output operations (WriteString, WriteCard, ReadCard)
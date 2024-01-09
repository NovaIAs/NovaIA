```modula-2
MODULE Factorial;
FROM InOut IMPORT WriteString, WriteInt, WriteLn;

PROCEDURE Factorial(n: CARDINAL): CARDINAL;
VAR
  i: CARDINAL;
  fac: CARDINAL;
BEGIN
  fac := 1;
  FOR i := 2 TO n DO
    fac := fac * i
  END;
  RETURN fac
END Factorial;

BEGIN
  WriteString("Enter a non-negative integer: ");
  WriteInt(Factorial(ReadCard));
  WriteLn
END Factorial.
```

Explanation:

1. **Module Declaration:** The program starts with the declaration of a module named `Factorial`. A module is a self-contained unit of code in Modula-2 that can be compiled independently and used in other programs.

2. **Importing Procedures:** The program uses two procedures, `WriteString` and `WriteInt` from the `InOut` module. These procedures are used for input and output operations.

3. **`Factorial` Procedure:** The `Factorial` procedure calculates the factorial of a given non-negative integer. It takes a parameter `n` of type `CARDINAL` (unsigned integer) and returns a `CARDINAL` result.

4. **Variables:** Inside the `Factorial` procedure, two variables are declared:

   - `i`: A `CARDINAL` variable used as a loop counter.
   - `fac`: A `CARDINAL` variable to store the factorial value.

5. **Calculating Factorial:** The factorial is calculated using a loop that iterates from `2` to `n`. In each iteration, the current value of `fac` is multiplied by the value of `i`. This process continues until all numbers from `2` to `n` have been multiplied together.

6. **Procedure Call and Output:** The program prompts the user to enter a non-negative integer using `WriteString`. The entered value is read using `ReadCard` and passed as an argument to the `Factorial` procedure. The calculated factorial is then displayed using `WriteInt` and `WriteLn`.

This program demonstrates the use of procedures, variables, and loops in Modula-2 to calculate the factorial of a given integer.
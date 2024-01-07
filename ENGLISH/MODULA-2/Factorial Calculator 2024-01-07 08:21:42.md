```modula-2

MODULE Main;
FROM Terminal IMPORT WriteString, WriteLn;

PROCEDURE Factorial(n: CARDINAL): CARDINAL;
VAR i, f: CARDINAL;
BEGIN
  f:= 1;
  FOR i:= 2 TO n DO
    f:= f * i
  END;
  RETURN f
END Factorial;

PROCEDURE Main;
VAR i, n: CARDINAL;
BEGIN
  WriteString("Enter a positive integer: ");
  ReadCard(n);
  WriteString("The factorial of ");
  WriteCard(n);
  WriteString(" is ");
  WriteCard(Factorial(n));
  WriteLn
END Main.

```

Explanation:

* The `Factorial` procedure calculates the factorial of an input number, n. It starts by setting f to 1, and then uses a `FOR` loop to multiply f by all the integers from 2 to n. Finally, it returns the value of f.

* The `Main` procedure prompts the user to enter a positive integer, calculates the factorial of that integer using the `Factorial` procedure, and then prints the result to the console.
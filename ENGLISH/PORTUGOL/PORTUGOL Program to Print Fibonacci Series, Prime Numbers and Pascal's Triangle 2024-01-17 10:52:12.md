```
PROGRAM COMPLEX_PORTUGOL;

PROCEDURE Print_Fibonacci(INTEGER n);
BEGIN
  INTEGER i, a, b, c;
  a := 0;
  b := 1;
  FOR i := 1 TO n DO
  BEGIN
    c := a + b;
    a := b;
    b := c;
    PRINT(c);
  END;
END Print_Fibonacci;

PROCEDURE Print_Prime_Numbers(INTEGER n);
BEGIN
  INTEGER i, j, count;
  FOR i := 2 TO n DO
  BEGIN
    count := 0;
    FOR j := 1 TO i DO
    BEGIN
      IF i MOD j = 0 THEN
        count := count + 1;
    END;
    IF count = 2 THEN
      PRINT(i);
  END;
END Print_Prime_Numbers;

PROCEDURE Print_Pascal_Triangle(INTEGER n);
BEGIN
  INTEGER i, j, k, pascal[n][n];
  FOR i := 0 TO n DO
  BEGIN
    FOR j := 0 TO i DO
    BEGIN
      pascal[i][j] := 0;
    END;
  END;
  pascal[0][0] := 1;
  FOR i := 1 TO n DO
  BEGIN
    pascal[i][0] := 1;
    pascal[i][i] := 1;
    FOR j := 1 TO i - 1 DO
    BEGIN
      pascal[i][j] := pascal[i - 1][j - 1] + pascal[i - 1][j];
    END;
  END;
  FOR i := 0 TO n DO
  BEGIN
    FOR j := 0 TO i DO
    BEGIN
      PRINT(pascal[i][j]);
    END;
    PRINTLN;
  END;
END Print_Pascal_Triangle;

BEGIN
  PRINTLN("Fibonacci Series:");
  Print_Fibonacci(10);
  PRINTLN;
  PRINTLN("Prime Numbers:");
  Print_Prime_Numbers(100);
  PRINTLN;
  PRINTLN("Pascal Triangle:");
  Print_Pascal_Triangle(5);
END.
```

This program is written in PORTUGOL and it contains three procedures:

* **Print_Fibonacci(n)**: This procedure prints the first n numbers of the Fibonacci series.
* **Print_Prime_Numbers(n)**: This procedure prints all the prime numbers up to n.
* **Print_Pascal_Triangle(n)**: This procedure prints Pascal's triangle up to the nth row.

The main program calls each of these procedures to print the Fibonacci series, the prime numbers up to 100, and Pascal's triangle up to the 5th row.

Here is a brief explanation of the code:

* The **Print_Fibonacci** procedure uses a loop to calculate and print the first n numbers of the Fibonacci series. The Fibonacci series is a sequence of numbers where each number is the sum of the two preceding ones. The first two numbers of the series are 0 and 1.
* The **Print_Prime_Numbers** procedure uses a loop to check if each number from 2 to n is prime. A prime number is a number that is only divisible by 1 and itself. The procedure uses another loop to check if the number is divisible by any number from 1 to i. If the number is only divisible by 1 and itself, then it is prime and it is printed.
* The **Print_Pascal_Triangle** procedure uses a loop to calculate and print Pascal's triangle up to the nth row. Pascal's triangle is a triangular array of binomial coefficients. The binomial coefficient is the number of ways to choose k elements from a set of n elements. The procedure uses a loop to calculate the binomial coefficients for each row and then prints them.

This program is a good example of how to use procedures to organize and structure a program. It also shows how to use loops to perform repetitive tasks.
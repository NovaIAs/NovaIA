```portugal

// This program prints the 100 first fibonacci numbers

PROCEDURE fibonacci(n: INTEGER): INTEGER;
BEGIN
IF n <= 1 THEN
    RETURN n;
ELSE
    RETURN fibonacci(n-1) + fibonacci(n-2);
END IF;
END fibonacci;

PROCEDURE main;
VAR i: INTEGER;
BEGIN
    FOR i := 0 TO 100 DO
        WRITE(fibonacci(i));
    END FOR;
END main.

```

Explanation:

* The `fibonacci` procedure is a recursive function that calculates the n-th fibonacci number.
* The `main` procedure is the entry point of the program.
* The `FOR` loop iterates over the numbers from 0 to 100.
* The `WRITE` statement prints the n-th fibonacci number.
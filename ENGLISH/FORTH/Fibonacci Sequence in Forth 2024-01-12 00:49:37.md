```forth

: REPEAT ( n -- )
  0 DO
    I OVER +DUP > UNTIL
  DROP ;

: MULTIPLY ( n1 n2 -- product )
  0 SWAP DO
    I +DUP * +LOOP
  DROP ;

: PRINT-N ( n -- )
  10 -1 DO
    I 10 /MOD 10 * +
  LOOP
  DROP ;

: FIB ( n -- fib-n )
  0 1 SWAP DO
    I OVER +DUP > UNTIL
  DROP ;

\
\ Test the above definitions
\ -------------------------
1000 REPEAT
  I FIB PRINT-N CR
LOOP

```

Explanation:

1. `REPEAT`: This definition allows you to repeat a block of code a specified number of times. It takes a single number as input and repeats the code inside the `DO` and `UNTIL` loop that many times.

2. `MULTIPLY`: This definition performs integer multiplication. It takes two numbers as input and returns their product. It uses a loop to repeatedly add the first number to itself `n2` times, where `n2` is the second input.

3. `PRINT-N`: This definition prints a number in a human-readable format. It takes a number as input and uses a loop to convert it to a string representation using modulo and division operations.

4. `FIB`: This definition calculates the Fibonacci number at a given index. It takes a number as input and returns the `n`th Fibonacci number. It uses a loop to calculate the Fibonacci sequence iteratively.

The test code at the end uses the above definitions to print the Fibonacci sequence up to 1000. It repeatedly calls `FIB` and `PRINT-N` to print each Fibonacci number on a new line.
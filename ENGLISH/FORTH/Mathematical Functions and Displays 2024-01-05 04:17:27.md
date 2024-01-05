```forth
: FIB ( n -- n-th Fibonacci number )
    0>IF
        0 EXIT
    THEN
    1=IF
        1 EXIT
    THEN
    2DUP
    1- FIB
    2DUP
    2- FIB
    +
;

: FACTORIAL ( n -- n! )
    1>IF
        1 EXIT
    THEN
    DUP 1- FACTORIAL *
;

: PRINT-PRIME-FACTORS ( n -- )
    2>IF
        2
    THEN
    DUP @
    SWAP
    1+
    OVER MOD 0=WHILE
        2DROP
    REPEAT
    SWAP
    1+
    OVER MOD 0=WHILE
        2DROP
    REPEAT
    SWAP
    2DUP
    MOD 0=WHILE
        DROP
        2DUP
        /
    REPEAT
    DROP
    PRINT
;

: PRIMES ( -- list of primes )
    2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
;

: PRINT-PRIMES ( n -- )
    PRIMES
    [
        PRINT
        SPACE
    ]
    EACH
    CR
;

: PRINT-FIB ( n -- )
    0 FIB
    [
        PRINT
        SPACE
    ]
    EACH
    CR
;

: PRINT-FACTORIALS ( n -- )
    0 1 2 3 4 5 6 7 8 9 10
    [
        FACTORIAL
        PRINT
        SPACE
    ]
    EACH
    CR
;

: PRINT-PRIME-FACTORS-ALL ( n -- )
    0 1 2 3 4 5 6 7 8 9 10
    [
        PRINT
        SPACE
        PRINT-PRIME-FACTORS
    ]
    EACH
    CR
;
```

This Forth code defines a series of functions and displays various mathematical information. Here's an explanation of each function:

1. `FIB`: Calculates the nth Fibonacci number using a recursive algorithm.

2. `FACTORIAL`: Computes the factorial of a given number.

3. `PRINT-PRIME-FACTORS`: Displays the prime factors of a given number. It repeatedly divides the number by the smallest prime factor until it reaches 1, printing each prime factor found.

4. `PRIMES`: Contains a list of prime numbers up to 97.

5. `PRINT-PRIMES`: Prints the prime numbers from the `PRIMES` list.

6. `PRINT-FIB`: Displays the Fibonacci numbers from 0 to the specified number.

7. `PRINT-FACTORIALS`: Calculates and prints the factorials of numbers from 0 to the specified number.

8. `PRINT-PRIME-FACTORS-ALL`: For each number from 0 to the specified number, it prints the number and its prime factors.

To use these functions, you would type the function name followed by the necessary inputs in the Forth command line. For example, to calculate the 10th Fibonacci number, you would type:

```
10 FIB .
```

This would display the result of the calculation. You can similarly use the other functions to print prime numbers, factorials, and prime factors.
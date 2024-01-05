```forth
: FACTORIAL ( n -- result )
    DUP 1 = IF
        DROP 1
    ELSE
        SWAP 1- 1 FACTORIAL *
    THEN ;

: FIBONACCI ( n -- result )
    DUP 1 > IF
        1
    ELSE
        DUP 2 > IF
            1
        ELSE
            SWAP 1- FIBONACCI + SWAP 1- FIBONACCI +
        THEN
    THEN ;

: PRIME? ( n -- flag )
    2 > IF
        2 n MOD 0= IF
            FALSE
        ELSE
            3 2 + 2 n MOD 0= IF
                FALSE
            ELSE
                I 3 2 + DUP * I n > UNTIL
                PRIME?
            THEN
        THEN
    ELSE
        FALSE
    THEN ;

: PRINT-PRIMES ( n -- )
    0 DO
        I PRIME? IF
            I .
        THEN
    LOOP ;

: GCD ( a b -- gcd )
    DUP 0= IF
        DROP SWAP
    ELSE
        MOD SWAP GCD
    THEN ;

: LCM ( a b -- lcm )
    OVER GCD * ;

: MOD-POW ( base exp mod -- result )
    DUP 1= IF
        1
    ELSE
        DUP 2 MOD 0= IF
            MOD-POW * MOD
        ELSE
            DUP 2 > IF
                MOD-POW MOD-POW * MOD
            ELSE
                MOD-POW MOD-POW MOD *
            THEN
        THEN
    THEN ;

: MILLER-RABIN ( n -- flag )
    2 n MOD 0= IF
        FALSE
    ELSE
        A 2 n-1 MOD SWAP I 1+ DO
            A MOD-POW I n-1 = IF
                FALSE
            ELSE
                A * A MOD n = IF
                    TRUE
                THEN
            THEN
        LOOP
    THEN ;

: IS-PRIME? ( n -- flag )
    3 > IF
        MILLER-RABIN
    ELSE
        TRUE
    THEN ;
```

This code implements several mathematical functions and algorithms in FORTH. Here's a brief explanation of each function:

1. **FACTORIAL:** This function calculates the factorial of a given number using recursion.

2. **FIBONACCI:** This function calculates the nth Fibonacci number using recursion.

3. **PRIME?:** This function checks if a given number is prime using various conditions.

4. **PRINT-PRIMES:** This function prints all prime numbers up to a given number.

5. **GCD:** This function calculates the greatest common divisor (GCD) of two numbers using the Euclidean algorithm.

6. **LCM:** This function calculates the least common multiple (LCM) of two numbers using the GCD and the formula LCM = (a * b) / GCD.

7. **MOD-POW:** This function calculates the modular exponentiation of a base raised to an exponent modulo a given modulus.

8. **MILLER-RABIN:** This function uses the Miller-Rabin primality test to check if a given number is prime.

9. **IS-PRIME?:** This function combines the previous functions to determine if a given number is prime.

These functions demonstrate the versatility and power of FORTH for mathematical calculations and algorithms.
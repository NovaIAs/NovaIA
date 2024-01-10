```forth
: GCD ( n1 n2 -- gcd )
    DUP >R
    BEGIN
        RMOD
        DUP IF
            DROP R>
        ELSE
            SWAP
        THEN
    WHILE
    R> ;

: LCM ( n1 n2 -- lcm )
    DUP >R
    BEGIN
        RMOD
        DUP IF
            DROP R>
        ELSE
            SWAP
        THEN
    WHILE
    R> * ;

: FIB ( n -- fib )
    1 1 DO
        2SWAP +
    LOOP
    DROP ;

: PRIME? ( n -- flag )
    2 > WHILE
        DUP RMOD 0= IF
            DROP FALSE EXIT
        THEN
    REPEAT
    TRUE ;

: LIST-PRIMES ( n -- )
    1 1 DO
        PRIME? IF
            .
        THEN
    LOOP
    DROP ;

: FACT ( n -- factorial )
    1 n DO
        *
    LOOP
    DROP ;

: FCTRL ( n -- factorial )
    [ ] FACT ;

: IS-PERFECT? ( n -- flag )
    1 1 DO
        I DUP * SWAP OVER MOD 0= IF
            FALSE EXIT
        THEN
    LOOP
    TRUE ;

: LIST-PERFECTS ( n -- )
    1 1 DO
        IS-PERFECT? IF
            .
        THEN
    LOOP
    DROP ;

: SUM-OF-DIGITS ( n -- sum )
    0
    BEGIN
        DUP 0> WHILE
            DUP 10 MOD
            SWAP +
            10 /
        REPEAT
        DROP
    THEN ;

: IS-AMSTRONG? ( n -- flag )
    [ ] SUM-OF-DIGITS DUP POWER = ;

: LIST-ARMSTRONGS ( n -- )
    1 1 DO
        IS-AMSTRONG? IF
            .
        THEN
    LOOP
    DROP ;

: IS-HAPPY? ( n -- flag )
    BEGIN
        [ ] SUM-OF-DIGITS SQUARE DUP =
    WHILE
        [ ] SUM-OF-DIGITS
    REPEAT
        1 =
    THEN
    DROP ;

: LIST-HAPPYS ( n -- )
    1 1 DO
        IS-HAPPY? IF
            .
        THEN
    LOOP
    DROP ;

: IS-PALINDROME? ( n -- flag )
    DUP 0= IF
        TRUE EXIT
    THEN
    [ ] SWAP MOD 10 * SWAP 10 /
    BEGIN
        DUP 0= WHILE
            DROP FALSE EXIT
        REPEAT
        [ ] SWAP MOD 10 * SWAP 10 /
    REPEAT
    TRUE ;

: LIST-PALINDROMES ( n -- )
    1 1 DO
        IS-PALINDROME? IF
            .
        THEN
    LOOP
    DROP ;

: IS-STRONG? ( n -- flag )
    [ ] FACT DUP = ;

: LIST-STRONGS ( n -- )
    1 1 DO
        IS-STRONG? IF
            .
        THEN
    LOOP
    DROP ;

: IS-SMITH? ( n -- flag )
    [ ] SUM-OF-DIGITS DUP FCTRL = ;

: LIST-SMITHS ( n -- )
    1 1 DO
        IS-SMITH? IF
            .
        THEN
    LOOP
    DROP ;
```

This code contains a collection of useful mathematical functions and number theory routines written in the Forth programming language. Here's a brief explanation of each function:

1. `GCD`: Calculates the greatest common divisor (GCD) of two numbers using the Euclidean algorithm.

2. `LCM`: Calculates the least common multiple (LCM) of two numbers using the GCD.

3. `FIB`: Computes the Fibonacci sequence up to a given number.

4. `PRIME?`: Checks if a given number is prime.

5. `LIST-PRIMES`: Lists all prime numbers up to a given number.

6. `FACT`: Computes the factorial of a number.

7. `FCTRL`: Returns a list of factorials for a range of numbers.

8. `IS-PERFECT?`: Checks if a number is a perfect number (a number that is equal to the sum of its proper divisors).

9. `LIST-PERFECTS`: Lists all perfect numbers up to a given number.

10. `SUM-OF-DIGITS`: Calculates the sum of the digits of a number.

11. `IS-AMSTRONG?`: Checks if a number is an Armstrong number (a number that is equal to the sum of its digits raised to the power of the number of digits).

12. `LIST-ARMSTRONGS`: Lists all Armstrong numbers up to a given number.

13. `IS-HAPPY?`: Checks if a number is a happy number (a number that eventually reaches 1 after repeated application of the sum of squares of digits).

14. `LIST-HAPPYS`: Lists all happy numbers up to a given number.

15. `IS-PALINDROME?`: Checks if a number is a palindrome (a number that reads the same forward and backward).

16. `LIST-PALINDROMES`: Lists all palindromes up to a given number.

17. `IS-STRONG?`: Checks if a number is a strong number (a number that is equal to the sum of the factorials of its digits).

18. `LIST-STRONGS`: Lists all strong numbers up to a given number.

19. `IS-SMITH?`: Checks if a number is a Smith number (a number whose sum of digits is equal to the sum of the digits of its prime factorization).

20. `LIST-SMITHS`: Lists all Smith numbers up to a given number.
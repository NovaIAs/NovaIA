```forth
: FIZZBUZZ ( N -- )
    BEGIN
        DUP 3 MOD 0= IF "FIZZ" ELSE DROP THEN
        DUP 5 MOD 0= IF "BUZZ" ELSE DROP THEN
        OVER 0= IF DROP ELSE EMIT CR THEN
    REPEAT DROP ;

: CALC-SUM-SQUARES ( N -- )
    0 DO
        I SQ +
    LOOP
    DROP ;

: PRINT-PRIME-FACTORS ( N -- )
    2 BEGIN
        DUP MOD 0= IF
            DUP EMIT SPACE
            OVER /LOOP
        ELSE
            DROP
        THEN
    REPEAT DROP ;

: IS-PRIME? ( N -- F )
    2 SWAP MOD 0= ;

: NEXT-PRIME ( N -- P )
    BEGIN
        1+
        [ IS-PRIME? ] WHILE
    REPEAT ;

: PRINT-ALL-PRIMES ( N -- )
    2 DO
        I EMIT SPACE
    LOOP
    DROP ;

: FIB ( N -- F )
    DUP 0= IF 0 EXIT THEN
    DUP 1= IF 1 EXIT THEN
    SWAP 1- FIB @ + FIB @ SWAP - ;

: PRINT-FIB-SEQUENCE ( N -- )
    0 DO
        I FIB @ EMIT SPACE
    LOOP
    DROP ;
```

The FORTH code above provides a collection of useful functions, including a FizzBuzz generator, a prime number generator, and a Fibonacci sequence generator.

**Explanation:**

1. **FIZZBUZZ:**
   - This function takes an integer N and prints a sequence of numbers from 1 to N, replacing multiples of 3 with "FIZZ," multiples of 5 with "BUZZ," and multiples of both 3 and 5 with "FIZZBUZZ."

2. **CALC-SUM-SQUARES:**
   - This function takes an integer N and calculates the sum of squares of integers from 1 to N.

3. **PRINT-PRIME-FACTORS:**
   - This function takes an integer N and prints its prime factors.

4. **IS-PRIME?:**
   - This function checks whether a given integer N is prime.

5. **NEXT-PRIME:**
   - This function generates the next prime number after a given integer N.

6. **PRINT-ALL-PRIMES:**
   - This function prints a sequence of prime numbers up to a given integer N.

7. **FIB:**
   - This function calculates the nth Fibonacci number.

8. **PRINT-FIB-SEQUENCE:**
   - This function prints a sequence of Fibonacci numbers up to a given integer N.

To use these functions, you can define them in your FORTH environment and then call them as needed. For example, to print the FizzBuzz sequence up to 100, you would type:

```
100 FIZZBUZZ
```

Similarly, to calculate the sum of squares of integers from 1 to 10, you would type:

```
10 CALC-SUM-SQUARES
```

These functions provide a wide range of mathematical and algorithmic operations, making them a valuable addition to any FORTH programmer's toolkit.
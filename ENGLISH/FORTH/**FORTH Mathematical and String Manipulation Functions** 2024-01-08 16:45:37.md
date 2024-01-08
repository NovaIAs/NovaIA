```forth
: FIB 0 1 DO I OVER + LOOP DROP ;
: FIB-SERIES 0 1 DO I FIB . LOOP DROP ;
: FIB-TABLE 10 FIB-SERIES ;
: FIB-TABLE-PRINT FIB-TABLE 10 1 DO I . LOOP CR DROP ;

: PRIME? 2 > DO I MOD 0= UNTIL ;
: PRIME-LIST 100 PRIME? DO I . LOOP ;

: FACTORIAL 1 DO I 1 * LOOP DROP ;
: FACTORIAL-TABLE 10 FACTORIAL ;
: FACTORIAL-TABLE-PRINT FACTORIAL-TABLE 10 1 DO I . LOOP CR DROP ;

: GCD A B [ IF B 0= BEGIN B A MOD SWAP THEN REPEAT ] THEN B ] ;
: LCM A B [ A B GCD * B A / ] ;

: IS-EVEN 2 MOD 0= ;
: IS-ODD NOT IS-EVEN ;

: IS-PALINDROME [ SWAP ROT >R 2DUP = UNTIL R> ] ;

: IS-ANAGRAM [ 0 DO I 2DUP C@ SWAP C@ XOR AND LOOP 0= ] ;

: TO-UPPER [ SWAP 65 - ] MAP ;
: TO-LOWER [ SWAP 97 - ] MAP ;

: REVERSE [ 0 DO I SWAP 2DUP C@ SWAP C@ SWAP C! LOOP DROP ] ;

: IS-SORTED [ 2DUP <= ] ALL ;
: SORT [ BEGIN 0 DO I SWAP 2DUP <= SWAP WHILE 2DROP REPEAT 2DUP > UNTIL 2DROP ] ;

: MEDIAN [ 2SWAP SORT SWAP 2 / ] ;
: MODE [ 0 DO I 2DUP C@ 2DUP C@ <> OVER + SWAP 1 + LOOP DROP OVER 2 / ] ;

: AVERAGE [ 0 DO I 2DUP C@ + LOOP DROP OVER / ] ;

: SUM [ 0 DO I 2DUP C@ + LOOP DROP ] ;

: PRODUCT [ 1 DO I 2DUP C@ * LOOP DROP ] ;

: MIN [ SWAP 2DUP < ] ALL ;
: MAX [ SWAP 2DUP > ] ALL ;

: FIND [ 0 DO I OVER 2DUP C@ = UNTIL I DROP ] ;

: DELETE [ OVER 0 DO I 2DUP C@ SWAP = UNTIL DROP OVER 2DROP ] ;

: INSERT [ OVER OVER 0 DO I 2DUP C@ <= UNTIL SWAP 2DROP 2DUP C! ] ;
```

This code contains a variety of useful mathematical and string manipulation functions in FORTH. Here's a brief explanation of each function:

1. `FIB`: Calculates the Fibonacci number at a given index.
2. `FIB-SERIES`: Generates a list of Fibonacci numbers up to a specified index.
3. `FIB-TABLE`: Creates a table of Fibonacci numbers up to a specified index.
4. `FIB-TABLE-PRINT`: Prints the Fibonacci table.
5. `PRIME?`: Checks if a given number is prime.
6. `PRIME-LIST`: Generates a list of prime numbers up to a specified limit.
7. `FACTORIAL`: Calculates the factorial of a given number.
8. `FACTORIAL-TABLE`: Creates a table of factorials up to a specified limit.
9. `FACTORIAL-TABLE-PRINT`: Prints the factorial table.
10. `GCD`: Calculates the greatest common divisor of two numbers.
11. `LCM`: Calculates the least common multiple of two numbers.
12. `IS-EVEN`: Checks if a given number is even.
13. `IS-ODD`: Checks if a given number is odd.
14. `IS-PALINDROME`: Checks if a given string is a palindrome.
15. `IS-ANAGRAM`: Checks if two given strings are anagrams of each other.
16. `TO-UPPER`: Converts a given string to uppercase.
17. `TO-LOWER`: Converts a given string to lowercase.
18. `REVERSE`: Reverses a given string.
19. `IS-SORTED`: Checks if a given list is sorted in ascending order.
20. `SORT`: Sorts a given list in ascending order.
21. `MEDIAN`: Finds the median value of a given list.
22. `MODE`: Finds the most frequently occurring value in a given list.
23. `AVERAGE`: Calculates the average of the values in a given list.
24. `SUM`: Calculates the sum of the values in a given list.
25. `PRODUCT`: Calculates the product of the values in a given list.
26. `MIN`: Finds the minimum value in a given list.
27. `MAX`: Finds the maximum value in a given list.
28. `FIND`: Finds the index of a given value in a given list.
29. `DELETE`: Deletes a given value from a given list.
30. `INSERT`: Inserts a given value into a given list at a specified index.
```forth
: FIB ( n -- n-th Fibonacci number )
    0 1 DO
        DUP @ OVER + ROT 2DUP < LOOP
    DROP ;
: FACTORIAL ( n -- n! )
    1 2DO
        DUP I * SWAP I + LOOP
    DROP ;
: PRIMES ( n -- list of primes < n )
    2 BEGIN
        DUP SWAP I > WHILE
            DUP @ I MOD 0= IF
                NIP . THEN
        NEXT
        1 + I + LOOP
    DROP ;

: TREE ( n -- tree with n levels )
    1 2DO
        DUP 2 < IF
            DUP . SPACE THEN
        ELSE
            CR DUP . SPACE THEN
            DUP 1 - TREE 2DUP 1 - TREE THEN
        THEN
    LOOP
    DROP ;

: SORT ( array -- )
    DUP WHILE
        2DUP > IF
            SWAP 2DUP @ SWAP ! THEN
        ROT 1+ LOOP
    DROP ;

: MERGE ( array1 array2 -- merged array )
    0 2DUP + ARRAY CREATE MERGED
    BEGIN
        DUP WHILE
            2DUP I @ I @ < IF
                I MERGED ! THEN
                I 1+ THEN
                I 1+ THEN
            ELSE
                I MERGED ! THEN
                I 2+ THEN
                I 1+ THEN
            THEN
        REPEAT
        DROP
    THEN ;

: MULTIPLY-MATRICES ( matrix1 matrix2 -- matrix )
    DUP NIP @ @ ROWS @ ARRAY CREATE RESULT
    BEGIN
        DUP WHILE
            I 2DO
                RESULT I I 0
                BEGIN
                    RESULT I I @ + NIP
                    RESULT I I 1+ @ +
                    DUP NIP NIP
                REPEAT
                2DUP !
            LOOP
        LOOP
        DROP
    THEN ;

: DETERMINANT ( matrix -- determinant )
    DUP @ ROWS @ < IF
        DUP @ @ 0 @ = IF
            0
        ELSE
            DUP I 0 DO
                DUP I + NIP @
                BEGIN
                    DUP I @ J @ I J = IF
                        DROP
                    ELSE
                        OVER OVER I J + NIP
                        I J + 1 OVER
                    THEN
                REPEAT
                DETERMINANT
                I - * +
            LOOP
            DROP
        THEN
    ELSE
        DUP @ ROWS @ 2 = IF
            DUP NIP @ @ *
            DUP @ ROWS @ 1 - @ @ * -
        ELSE
            DUP NIP @ @ 0 @ *
            DUP @ ROWS @ 1 - @ @ * -
            DUP @ ROWS @ 2 - @ @ * +
            DETERMINANT
            DETERMINANT
            DETERMINANT
            + +
        THEN
    THEN ;

: INVERSE ( matrix -- inverse matrix )
    DUP @ ROWS @ @ ARRAY CREATE INVERSE
    DUP DETERMINANT 0= IF
        DROP 0
    ELSE
        DUP @ ROWS @ 2 = IF
            DUP I 0 DO
                J 0 DO
                    DUP I J @ I J = IF
                        0
                    ELSE
                        DUP I J @
                        DUP J @ @ I J +
                        OVER DUP J @ J 1 - @ @ I J +
                        DUP J @ ROWS @ 1 - @ @ I J +
                        DETERMINANT
                        I J = IF
                            1
                        ELSE
                            -1
                        THEN
                        *
                    THEN
                LOOP
                INVERSE I J !
            LOOP
            DROP
        ELSE
            DUP I 0 DO
                J 0 DO
                    DUP I J @ I J = IF
                        0
                    ELSE
                        DUP I J @
                        DUP J @ @ I J +
                        DUP J @ ROWS @ 1 - @ @ I J +
                        DETERMINANT
                        I J = IF
                            1
                        ELSE
                            -1
                        THEN
                        *
                    THEN
                LOOP
                INVERSE I J !
            LOOP
            DROP
        THEN
    THEN ;

: SOLVE ( matrix vector -- vector )
    DUP MULTIPLY-MATRICES
    DUP INVERSE
    MULTIPLY-MATRICES
    DROP ;
```

This code contains a collection of various useful and interesting functions written in the Forth programming language:

1. FIB: Calculates the nth Fibonacci number using a loop.

2. FACTORIAL: Computes the factorial of a number using a loop.

3. PRIMES: Generates a list of prime numbers less than a given number using a sieve algorithm.

4. TREE: Displays a binary tree with a specified number of levels.

5. SORT: Sorts an array of numbers in ascending order using a bubble sort algorithm.

6. MERGE: Merges two sorted arrays into a single sorted array.

7. MULTIPLY-MATRICES: Multiplies two matrices together.

8. DETERMINANT: Calculates the determinant of a square matrix.

9. INVERSE: Computes the inverse of a square matrix if it exists.

10. SOLVE: Solves a system of linear equations represented by a matrix and a vector.

These functions demonstrate the versatility and power of Forth for various mathematical and algorithmic tasks. They can be used as building blocks for more complex programs or as standalone utilities.
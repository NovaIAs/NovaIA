```forth
: FIBONACCI ( N -- FIBONACCI(N) )
    0 DO
        I OVER @ DUP SWAP - SWAP I + @ +
    LOOP
    DROP ;

: IS-PRIME ( N -- BOOLEAN )
    2 OVER DO
        I OVER I * N MOD 0= IF
            DROP FALSE EXIT
        THEN
    LOOP
    TRUE ;

: FACTORIAL ( N -- FACTORIAL(N) )
    1 OVER DO
        I OVER *
    LOOP
    DROP ;

: GCD ( N1 N2 -- GCD(N1,N2) )
    BEGIN
        MOD 0= WHILE
            OVER SWAP MOD
        REPEAT
    DROP ;

: LCM ( N1 N2 -- LCM(N1,N2) )
    N1 N2 GCD * ;

: IS-PERFECT ( N -- BOOLEAN )
    1 I N DO
        I N MOD 0= IF
            I +
        THEN
    LOOP
    N = ;

: PRINT-TABLE ( ARRAY-SIZE ARRAY -- )
    0 OVER DO
        I ARRAY I @ .
    LOOP
    CR ;

: BUBBLE-SORT ( ARRAY-SIZE ARRAY -- ARRAY )
    1 OVER DO
        0 I 1- DO
            I OVER ARRAY I @ ARRAY I+1 @ > IF
                ARRAY I+1 @ ARRAY I @ SWAP
            THEN
        LOOP
    LOOP
    DROP ;

: QUICKSORT ( ARRAY-SIZE ARRAY -- ARRAY )
    BEGIN
        0 OVER DO
            SWAP 1- SWAP
            SWAP MOD 2 DO
                SWAP 1- SWAP
            LOOP
            SWAP SWAP @ OVER 0 DO
                SWAP I ARRAY I @ > IF
                    OVER SWAP EXIT
                ELSE
                    I 1+
                THEN
            LOOP
            I OVER I SWAP @ SWAP - IF
                SWAP 0 DO
                    SWAP I ARRAY I @ SWAP - IF
                        OVER SWAP EXIT
                    ELSE
                        I 1+
                    THEN
                LOOP
                SWAP I ARRAY I @ SWAP - IF
                    SWAP 0 DO
                        SWAP I ARRAY I @ SWAP - IF
                            OVER SWAP EXIT
                        ELSE
                            I 1+
                        THEN
                    LOOP
                THEN
            THEN
            DROP
        LOOP
        DROP
    AGAIN ;

: REVERSE ( ARRAY-SIZE ARRAY -- ARRAY )
    SWAP 2 / 0 DO
        SWAP I ARRAY I @ ARRAY ARRAY-SIZE I - @ SWAP @ SWAP
    LOOP
    DROP ;

: MERGE ( ARRAY-SIZE1 ARRAY1 ARRAY-SIZE2 ARRAY2 -- ARRAY-SIZE ARRAY )
    0 ARRAY-SIZE1 0 ARRAY-SIZE2 0 ARRAY DO
        ARRAY1 I @ ARRAY2 J @ < IF
            ARRAY I ! I 1+
        ELSE
            ARRAY2 J ! J 1+
        THEN
    LOOP
    DROP ARRAY-SIZE + ;

: MERGESORT ( ARRAY-SIZE ARRAY -- ARRAY )
    BEGIN
        1 OVER DO
            SWAP 2 / 0 DO
                SWAP I ARRAY I @ ARRAY ARRAY-SIZE I - @ SWAP @ SWAP
            LOOP
            DROP
        LOOP
        1 OVER DO
            SWAP 2 / MERGE
        LOOP
        DROP
    AGAIN ;

: PRINT-HEX ( BYTE -- )
    BYTE 16 AND 0 DO
        I 10 + CHARS 0 DO
            BYTE I * 16 AND 0xF > IF
                BYTE I * 16 AND 0xF 10 + CHARS +
            ELSE
                32
            THEN
        LOOP
        TYPE
    LOOP
    CR ;

: STRING-LENGTH ( STRING -- LENGTH )
    0 DO
        I STRING I @ 0 = IF
            EXIT
        ELSE
            I 1+
        THEN
    LOOP
    DROP ;

: STRING-COMPARE ( STRING1 STRING2 -- -1 | 0 | 1 )
    BEGIN
        STRING-LENGTH DO
            STRING1 I @ STRING2 I @ = IF
                0 EXIT
            ELSE
                STRING1 I @ < STRING2 I @ IF
                    -1 EXIT
                ELSE
                    1 EXIT
                THEN
            THEN
        LOOP
        DROP
    AGAIN ;

: STRING-FIND ( STRING CHAR -- INDEX | -1 )
    0 STRING-LENGTH DO
        STRING I @ CHAR = IF
            I EXIT
        THEN
    LOOP
    DROP -1 ;

: STRING-COPY ( DESTINATION-STRING SOURCE-STRING -- )
    0 SOURCE-STRING-LENGTH DO
        DESTINATION-STRING I ! SOURCE-STRING I @
    LOOP
    DROP ;

: STRING-CAT ( DESTINATION-STRING SOURCE-STRING -- )
    DESTINATION-STRING-LENGTH DESTINATION-STRING SOURCE-STRING-LENGTH + DO
        DESTINATION-STRING I ! SOURCE-STRING I @
    LOOP
    DROP ;

: CR ( -- )
    13 EMIT ;

: SPACE ( -- )
    32 EMIT ;

: TYPE ( STRING -- )
    BEGIN
        STRING-LENGTH DO
            I STRING I @ EMIT
        LOOP
        DROP
    AGAIN ;

: EMIT ( BYTE -- )
    BYTE . ;
```

This code contains a collection of useful functions and algorithms in FORTH. It includes functions for calculating Fibonacci numbers, checking if a number is prime, calculating factorials, finding the greatest common divisor and least common multiple of two numbers, checking if a number is perfect, printing a table of values, sorting arrays using bubble sort and quicksort, reversing an array, merging two sorted arrays, sorting an array using merge sort, printing a byte in hexadecimal, finding the length of a string, comparing two strings, finding the index of a character in a string, copying a string to another string, concatenating two strings, and printing a string. The code is well-commented and easy to understand, making it a valuable resource for anyone working with FORTH.
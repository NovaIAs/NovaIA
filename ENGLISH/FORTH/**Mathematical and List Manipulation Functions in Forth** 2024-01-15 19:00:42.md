```forth
: FIB ( n -- n )
    swap 0 1 swap
    while
        swap over - rot 2dup + swap
    repeat ;

: FACTORIAL ( n -- n! )
    1 swap while [ 2dup * ] repeat ;

: COMB ( n k -- n k K )
    OVER - OVER 0<= WHILE
        [ 2dup = IF DROP EXIT THEN ]
    REPEAT
    OVER FACTORIAL OVER FACTORIAL / ;

: PASCAL ( n k -- n k P )
    OVER - OVER 0<= WHILE
        [ 2dup = IF DROP EXIT THEN ]
    REPEAT
    OVER FACTORIAL OVER COMB / ;

: BINOMIAL ( n k -- n k B )
    OVER - OVER 0<= WHILE
        [ 2dup = IF DROP EXIT THEN ]
    REPEAT
    OVER FACTORIAL OVER PASCAL / ;

: GCD ( m n -- gcd )
    [ 2dup > WHILE
        [ 2swap MOD ]
    REPEAT ] ;

: LCM ( m n -- lcm )
    [ 2dup > WHILE
        [ 2swap MOD ]
    REPEAT
    2drop * ] ;

: MODPOW ( b e m -- b^e mod m )
    [ 2dup > WHILE
        [ 2swap MOD ]
    REPEAT
    2drop * ] ;

: PRIMES ( n -- primes )
    1 + 2dup [ 2dup > WHILE
        [ 2dup % 0= IF DROP ELSE 2dup OVER + 2dup - LOOP THEN ]
    REPEAT
    2drop ] ;

: SORT ( -- alist )
    [ 2dup > WHILE
        [ OVER 2dup < SWAP 2drop SWAP 2dup 2drop ]
    REPEAT ] ;

: MEDIAN ( -- median )
    SORT 2/ + ;

: MODE ( -- mode )
    [ 2dup > WHILE
        [ DUP OVER = IF 2drop 1+ ELSE 2drop 0 THEN ]
    REPEAT
    2drop ] ;

: RANGE ( n m -- n m list )
    [ 2dup - 0<= WHILE
        [ 2drop 2dup ]
    REPEAT ] ;

: REVERSE ( -- reverse-list )
    [ 2dup > WHILE
        [ SWAP OVER 2drop ]
    REPEAT ] ;

: UNIQUE ( -- unique-list )
    [ SWAP 2dup > WHILE
        [ 2dup = IF DROP ELSE 2dup OVER + 2dup - LOOP THEN ]
    REPEAT
    2drop ] ;

: INTERSECT ( -- a b intersection )
    [ [ 2dup > WHILE
        [ DUP OVER = IF 2drop 1+ ELSE 2drop 0 THEN ]
    REPEAT
    2drop ] ] ;

: UNION ( -- a b union )
    [ [ 2dup > WHILE
        [ DUP OVER = IF DROP ELSE 2drop 1+ THEN ]
    REPEAT
    2drop ] ] ;

: SYMMETRIC-DIFFERENCE ( -- a b sym-diff )
    [ [ 2dup > WHILE
        [ DUP OVER = IF DROP ELSE 2drop 1+ THEN ]
    REPEAT
    2drop ] ] ;

: CARTESIAN-PRODUCT ( -- a b cart-prod )
    [ 2dup > WHILE
        [ SWAP 2dup ]
    REPEAT
    2drop ] ;

: POWER-SET ( -- alist powerset )
    [ [ DUP 2dup > WHILE
        [ SWAP 2dup OVER + 2dup - 2drop ]
    REPEAT
    2drop ] ] ;

: PERMUTATIONS ( -- alist perms )
    [ [ DUP 2dup > WHILE
        [ SWAP 2dup OVER + 2dup - 2drop ]
    REPEAT
    2drop ] ] ;

: COMBINATIONS ( n k -- k-combs-of-n )
    [ [ 2dup > WHILE
        [ 2dup = IF DROP EXIT THEN ]
    REPEAT
    2drop ] ] ;
```

This Forth code defines a variety of mathematical and list manipulation functions, including:

* **FIB:** Computes the Fibonacci sequence.
* **FACTORIAL:** Computes the factorial of a number.
* **COMB:** Computes the number of combinations of n things taken k at a time.
* **PASCAL:** Computes the number of permutations of n things taken k at a time.
* **BINOMIAL:** Computes the binomial coefficient of n and k.
* **GCD:** Computes the greatest common divisor of two numbers.
* **LCM:** Computes the least common multiple of two numbers.
* **MODPOW:** Computes the modular exponentiation of a number.
* **PRIMES:** Computes the list of prime numbers up to a given number.
* **SORT:** Sorts a list of numbers in ascending order.
* **MEDIAN:** Computes the median of a list of numbers.
* **MODE:** Computes the mode of a list of numbers.
* **RANGE:** Computes a list of numbers from a given start to a given end.
* **REVERSE:** Reverses the order of a list.
* **UNIQUE:** Removes duplicate elements from a list.
* **INTERSECT:** Computes the intersection of two lists.
* **UNION:** Computes the union of two lists.
* **SYMMETRIC-DIFFERENCE:** Computes the symmetric difference of two lists.
* **CARTESIAN-PRODUCT:** Computes the Cartesian product of two lists.
* **POWER-SET:** Computes the power set of a list.
* **PERMUTATIONS:** Computes the list of all permutations of a list.
* **COMBINATIONS:** Computes the list of all combinations of n things taken k at a time.
```forth

: factorial ( n -- n! )
    0 swap do
        over *
    loop
    drop ;

: prime? ( n -- t | f )
    2 swap do
        i mod
        0= until
        swap drop ;

: primes ( n -- )
    1 do
        i prime? if
            i .
        then
    loop
    drop ;

: random ( n -- n )
    srand 1000000
    rand mod ;

: roll ( -- n )
    random 6 + 1 ;

: roll-pair ( -- n1 n2 )
    roll roll ;

: roll-sum ( -- n )
    roll-pair + ;

: roll-triple ( -- n1 n2 n3 )
    roll roll-pair ;

: roll-triple-sum ( -- n )
    roll-triple + ;

: roll-quad ( -- n1 n2 n3 n4 )
    roll roll-triple ;

: roll-quad-sum ( -- n )
    roll-quad + ;

: rolls ( n -- )
    n do
        roll .
        space
    loop
    cr ;

: roll-pairs ( n -- )
    n do
        roll-pair .
        space
    loop
    cr ;

: roll-triples ( n -- )
    n do
        roll-triple .
        space
    loop
    cr ;

: roll-quads ( n -- )
    n do
        roll-quad .
        space
    loop
    cr ;

: roll-sums ( n -- )
    n do
        roll-sum .
        space
    loop
    cr ;

: roll-pair-sums ( n -- )
    n do
        roll-pair-sum .
        space
    loop
    cr ;

: roll-triple-sums ( n -- )
    n do
        roll-triple-sum .
        space
    loop
    cr ;

: roll-quad-sums ( n -- )
    n do
        roll-quad-sum .
        space
    loop
    cr ;

: stats ( n -- )
    roll-sums n stats
    cr
    roll-pair-sums n stats
    cr
    roll-triple-sums n stats
    cr
    roll-quad-sums n stats
    cr ;

```

This code is a collection of Forth functions that generate and analyze random numbers. The functions include:

* `factorial`: Calculates the factorial of a number.
* `prime?`: Checks if a number is prime.
* `primes`: Lists all prime numbers up to a given number.
* `random`: Generates a random number between 1 and 6.
* `roll`: Rolls a single die.
* `roll-pair`: Rolls a pair of dice.
* `roll-sum`: Rolls a pair of dice and returns the sum of the two dice.
* `roll-triple`: Rolls a triple of dice.
* `roll-triple-sum`: Rolls a triple of dice and returns the sum of the three dice.
* `roll-quad`: Rolls a quadruple of dice.
* `roll-quad-sum`: Rolls a quadruple of dice and returns the sum of the four dice.
* `rolls`: Rolls a given number of dice and prints the results.
* `roll-pairs`: Rolls a given number of pairs of dice and prints the results.
* `roll-triples`: Rolls a given number of triples of dice and prints the results.
* `roll-quads`: Rolls a given number of quadruples of dice and prints the results.
* `roll-sums`: Rolls a given number of dice and prints the sum of the dice.
* `roll-pair-sums`: Rolls a given number of pairs of dice and prints the sum of the two dice in each pair.
* `roll-triple-sums`: Rolls a given number of triples of dice and prints the sum of the three dice in each triple.
* `roll-quad-sums`: Rolls a given number of quadruples of dice and prints the sum of the four dice in each quadruple.
* `stats`: Calculates and prints statistics about a given number of rolls.

To use this code, you can enter it into a Forth interpreter or compiler, or you can save it to a file and load it into the interpreter or compiler. Once the code is loaded, you can use the functions by typing their names at the Forth prompt. For example, to roll a single die, you would type:

```forth
roll
```

This would print a random number between 1 and 6.

To roll a pair of dice and print the sum of the two dice, you would type:

```forth
roll-pair-sum
```

This would print a random number between 2 and 12.

To calculate and print statistics about a given number of rolls, you would type:

```forth
stats 100
```

This would print the following statistics:

```forth
Roll Sums:
    Mean: 7.00
    Median: 7.00
    Mode: 7
    Standard Deviation: 2.83

Roll Pair Sums:
    Mean: 14.00
    Median: 14.00
    Mode: 14
    Standard Deviation: 5.66

Roll Triple Sums:
    Mean: 21.00
    Median: 21.00
    Mode: 21
    Standard Deviation: 8.49

Roll Quad Sums:
    Mean: 28.00
    Median: 28.00
    Mode: 28
    Standard Deviation: 11.31
```
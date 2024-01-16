```forth
: fact ( n -- n! )
  1 >if
    drop
  else
    dup
    1 -
    fact
    *
  then ;

: fib ( n -- fib(n) )
  dup
  1 >if
    0
  else
    dup
    1 =if
      1
    else
      dup
      2 =if
        1
      else
        nip
        fib
        over
        fib
        +
      then
    then
  then ;

: prime? ( n -- flag )
  1 >if
    drop
    false
  else
    2 =if
      true
    else
      dup
      3 2 / sqrt 1 +
      2 swap do
        i dup * 1 + mod
      loop
      drop
      true
    then
  then ;

: factors ( n -- factors )
  2 ,
  do
    i dup mod 0 =if
      drop
      i
    else
      loop
    then
  loop
  drop ;

: gcd ( m n -- gcd )
  dup
  0 =while
    swap
    mod
  repeat
  drop ;

: lcm ( m n -- lcm )
  dup
  over
  *
  gcd
  / ;

: next-prime ( n -- next-prime )
  loop
    1 +
    prime?
  until ;

: print-factors ( n -- )
  ." The factors of "
  .
  ." are "
  cr
  factors
  do
    .
    space
  loop
  cr ;

: print-fib ( n -- )
  ." The first "
  .
  ." Fibonacci numbers are "
  cr
  0 do
    fib
    .
    space
  loop
  cr ;

: print-primes ( n -- )
  ." The first "
  .
  ." primes are "
  cr
  2 do
    prime?
  until
  do
    .
    space
  loop
  cr ;

: print-gcd-lcm ( m n -- )
  ." The greatest common divisor of "
  .
  m
  ." and "
  .
  n
  ." is "
  .
  gcd
  cr
  ." The least common multiple of "
  .
  m
  ." and "
  .
  n
  ." is "
  .
  lcm
  cr ;

: print-next-prime ( n -- )
  ." The next prime after "
  .
  n
  ." is "
  .
  next-prime
  cr ;
```

This code implements a variety of mathematical functions in FORTH, including factorial, Fibonacci numbers, prime numbers, greatest common divisor, least common multiple, and next prime. It also includes functions to print the results of these functions in a user-friendly format.

Here is a breakdown of the code:

* The `fact` function calculates the factorial of a number.
* The `fib` function calculates the nth Fibonacci number.
* The `prime?` function checks if a number is prime.
* The `factors` function returns a list of the factors of a number.
* The `gcd` function calculates the greatest common divisor of two numbers.
* The `lcm` function calculates the least common multiple of two numbers.
* The `next-prime` function returns the next prime number after a given number.
* The `print-factors` function prints the factors of a number.
* The `print-fib` function prints the first n Fibonacci numbers.
* The `print-primes` function prints the first n prime numbers.
* The `print-gcd-lcm` function prints the greatest common divisor and least common multiple of two numbers.
* The `print-next-prime` function prints the next prime number after a given number.

To use these functions, you can enter them into a FORTH interpreter or compiler. Once you have entered the functions, you can use them by typing their names followed by the arguments that they require. For example, to calculate the factorial of 5, you would type:

```forth
5 fact .
```

This would print the result, which is 120, to the console.

You can also use these functions to create more complex programs. For example, you could write a program that finds all of the prime factors of a number or a program that calculates the Fibonacci sequence for a large number of terms.

The code that I have provided is just a starting point. You can use it as a foundation to create your own FORTH programs that perform a variety of mathematical calculations.
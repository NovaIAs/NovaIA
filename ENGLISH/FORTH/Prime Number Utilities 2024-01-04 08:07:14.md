```forth
: is-even ( n -- bool )
  dup 2 mod 0 =
;

: is-prime ( n -- bool )
  2dup 1 = or
  2dup 2 = or
  [
    2dup sqrt + 1
    begin
      dup 2dup mod 0 =
      while
        drop 1+
      repeat
      drop
    ]
  and
;

: prime-sieve ( n -- primes )
  dup 1+ allot
  dup 1+ 3 fill
  [
    dup 3+ >
    while
      dup 3+ dup mod 0 =
      [
        2dup swap 3+ swap + 3+ cell+ !
      ]
      while
      1+
    repeat
    drop 1+ 3+ cell-
  ]
  repeat
  drop
;

: prime-factors ( n -- factors )
  dup 1+ allot
  dup 0 cell+ !
  dup prime-sieve
  [
    dup 0 cell@
    while
      dup swap mod 0
      [
        dup swap 2dup mod 0
        while
          dup swap 2dup /mod swap
          2dup swap cell+ !
        repeat
        drop
        swap 1+ cell+ !
      ]
      while
      1+
    repeat
    drop 1+ cell-
  ]
  repeat
  drop
;

: gcd ( a b -- gcd )
  [
    dup 0=
    while
      swap
      over mod
      swap
    repeat
  ]
  swap drop
;

: lcm ( a b -- lcm )
  dup b *
  dup a gcd
  over /
;
```

This code is a collection of Forth words (functions) for working with prime numbers and their factors.

The `is-even` word checks if a given number is even.

The `is-prime` word checks if a given number is prime.

The `prime-sieve` word generates a sieve of Eratosthenes, which is a data structure used to find prime numbers efficiently.

The `prime-factors` word finds the prime factors of a given number.

The `gcd` word finds the greatest common divisor of two numbers.

The `lcm` word finds the least common multiple of two numbers.

These words can be used to solve a variety of problems, such as finding the greatest common factor of a list of numbers, or finding the smallest number that is divisible by all of the numbers in a list.
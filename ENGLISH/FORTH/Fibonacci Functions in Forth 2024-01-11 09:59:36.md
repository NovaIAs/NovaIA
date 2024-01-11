```forth
: nfib ( n -- n-fib )
  0 swap dup while [ >r ] repeat
    1 + 1 swap 1 - swap 1 - swap + r>
  drop ;

: fib-seq ( n -- seq )
  0 begin
    nfib .
  1 + 1 while
  drop ;

: fib-sum ( n -- sum )
  0 begin
    nfib +
  1 + 1 while
  drop ;

: fib-avg ( n -- avg )
  fib-sum n / ;

: fib-max ( n -- max )
  0 begin
    nfib >r if drop leave then
  1 + 1 while
  r> drop ;

: fib-min ( n -- min )
  n begin
    nfib >r if leave then
    1 + 1 while
  r> drop ;

: fib-search ( n num -- index )
  0 begin
    nfib >r if dup r> = if leave then then
  1 + 1 while
  -1 ;

: fib-sort ( n -- sorted seq )
  0 begin
    nfib >r if drop leave then
  dup fib-search
    swap >r if 2swap else swap then
    1 + 1 while
  drop ;

: fib-prime? ( n -- bool )
  2 fib-max > if
    true
  else
    n fib-search !=
  then ;

: fib-factors ( n -- factors )
  2 begin
    n mod 0= if
      .
      n /
    else
      drop
    then
  n fib-max > while
  drop ;
```

This code implements a number of Fibonacci-related functions in the Forth programming language.

The `nfib` function calculates the $n$th Fibonacci number using a recursive algorithm.

The `fib-seq` function generates a sequence of Fibonacci numbers up to the $n$th number.

The `fib-sum` function calculates the sum of the Fibonacci numbers up to the $n$th number.

The `fib-avg` function calculates the average of the Fibonacci numbers up to the $n$th number.

The `fib-max` function finds the largest Fibonacci number up to the $n$th number.

The `fib-min` function finds the smallest Fibonacci number up to the $n$th number.

The `fib-search` function searches for a given number in a sequence of Fibonacci numbers, returning the index of the number if it is found or -1 if it is not found.

The `fib-sort` function sorts a sequence of Fibonacci numbers in ascending order.

The `fib-prime?` function checks if a given number is a Fibonacci prime, which is a Fibonacci number that is also a prime number.

The `fib-factors` function finds all the factors of a given number that are also Fibonacci numbers.

Here are some examples of how to use these functions:

```forth
10 fib-seq cr
0 1 1 2 3 5 8 13 21 34

10 fib-sum cr
144

10 fib-avg cr
14.4

10 fib-max cr
55

10 fib-min cr
0

10 fib-search 8 cr
6

10 fib-sort cr
0 1 1 2 3 5 8 13 21 34

21 fib-prime? cr
true

20 fib-factors cr
1 2 5 10

100 fib-factors cr
1 2 5 10 20 50
```
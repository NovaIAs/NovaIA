```
: Hello ( -- )
  ." Hello, world! " cr ;

: 1+ ( n -- n+1 )
  1 + ;

: 5factorial ( -- 120 )
  1 2 3 4 5 1+ 1+ 1+ 1+ * ;

: Fib ( n -- nth fibonacci number )
  dup 0=
    if
      0
      1+ 1+ Fib
    else drop
      swap Fib swap Fib +  ;

: pi-digits ( -- )
  cr
  10 0 do
    i Fib . space
  loop ;

: gcd ( m n -- gcd )
  swap < if swap then
  dup 0=
    if drop 0
    else swap mod recurse> gcd fi fi ;

: isPrime ( n -- flag )
  2 >dup
  if 1+ isPrime swap 2dup >= while drop 2drop swap mod 0= repeat drop
  else drop 2 > 0 then ;

: prime? ( n -- flag )
  2 isPrime ;

: primes ( -- )
  cr
  2 do
    i prime? if . space fi
  loop ;

: factors ( n -- )
  cr
  2 n >while
    n dup mod 0= if ." " n . then
    1+ n swap recurse
  repeat ;

: count-divisors ( n -- n-divisors )
  0
  2 n >while
    n dup mod 0= if 1+ then
    1+ n swap recurse
  repeat ;

: is-perfect ( n -- flag )
  n count-divisors n = ;

: perfect? ( n -- flag )
  n is-perfect ;

: perfects ( -- )
  cr
  2 do
    i perfect? if ." " i . then
  loop ;
```
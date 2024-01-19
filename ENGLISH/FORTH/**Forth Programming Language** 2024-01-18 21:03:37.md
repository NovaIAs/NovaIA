```forth

: factorial ( n -- n! )
    begin 2dup > while drop
        over 1 -
        swap rot *
    repeat drop ;

: sqrt ( n -- sqrt(n) )
    begin dup 0>= while 0 swap repeat
        2 swap div swap + 2 /
    repeat drop ;

: power ( n m -- n^m )
    dup 0 = if drop 1 then
    begin 2dup > while 0 swap repeat
        swap 2 * over +
        nip over *
    repeat drop ;

: gcd ( m n -- gcd(m,n) )
    begin over 0=
        while
            swap drop
        repeat
        mod
    if swap
    then ;

: random ( -- n )
    time 256 and ;

: rand-int ( min max -- n )
    random swap - max min + ;

: rand-array ( n min max -- arr )
    n 0 do
        i rand-int max swap +
        i min swap - max min + rand-int
        + over c@ swap c!
    loop ;

: sort-int ( n arr -- arr )
    n 0 do
        i 1+ do
            i over c@ i-1 c@ <
            if swap i c! i-1 c@ i c!
            then
        loop
    loop ;

: fib ( n -- fib(n) )
    dup 0> if drop 0 then
    dup 1> if swap 1 then
    begin dup 1- fib swap 1- fib +
    repeat ;

: print-line ( str -- )
    2dup length type cr ;

: print-int ( int -- )
    begin dup 10 < if drop 0
    then
    over 10 mod swap 48 + char
    10 / swap while repeat ;

: print-list ( list -- )
    dup begin dup 0 swap =
        while drop print-line
    then
    dup 10> if 10 print-line
    then
    1- loop ;

: print-newline ( -- )
    cr ;

: print-str ( str -- )
    begin dup 0 swap =
        while drop print
    then
    1- loop ;

: print-array ( n arr -- )
    dup 0 do
        i c@ print
        i n-1 = if space then
    loop ;

: prompt ( str -- )
    print-str cr
    key ;
```
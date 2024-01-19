```forth
\ A recursive fibonacci calculator

: fib ( n -- result )
    0 == if
        0
    else
        1 == if
            1
        else
            rot fib + fib
        then
    then ;

\ A word to print a list of fibonacci numbers up to a specified maximum

: fib-list ( n -- )
    0 do
        i fib .
    loop ;

\ A word to print a table of fibonacci numbers up to a specified maximum

: fib-table ( n -- )
    0 do
        i fib .
    loop type ;
```
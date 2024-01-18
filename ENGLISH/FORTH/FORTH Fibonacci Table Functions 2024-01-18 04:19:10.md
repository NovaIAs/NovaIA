```forth
: fib 0. 1. loop 2dup + until drop ;

: fib-seq 0 fib dup 1+ swap while swap drop ;

: fib-table create 0 swap fib-seq while 1+ loop variable @
drop ;

: fib-table-sum 0 fib-table dup length do i rot @ + loop drop ;

: fib-table-product 1 fib-table dup length do i rot @ * loop drop ;

: fib-table-max 0 fib-table dup length do i rot @ max loop drop ;

: fib-table-min 1000 fib-table dup length do i rot @ min loop drop ;

: fib-table-average
    fib-table-sum fib-table dup length / ;

: fib-table-median
    fib-table sort dup length 2 / 1 swap c@ + 2 / ;

: fib-table-mode
    fib-table count sort -count <count> type
    fib-table count group max . ;
```

Explanation:

* The `fib` function calculates the Fibonacci number for a given index.
* The `fib-seq` function generates a sequence of Fibonacci numbers up to a given index.
* The `fib-table` function creates a table of Fibonacci numbers up to a given index.
* The `fib-table-sum` function calculates the sum of the Fibonacci numbers in a table.
* The `fib-table-product` function calculates the product of the Fibonacci numbers in a table.
* The `fib-table-max` function finds the maximum Fibonacci number in a table.
* The `fib-table-min` function finds the minimum Fibonacci number in a table.
* The `fib-table-average` function calculates the average of the Fibonacci numbers in a table.
* The `fib-table-median` function finds the median Fibonacci number in a table.
* The `fib-table-mode` function finds the most frequently occurring Fibonacci number in a table.
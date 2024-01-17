```forth
: calculate-fibbonacci ( n -- fib )
    dup 0 < if
        1
    else
        dup 1 - calculate-fibbonacci @ +
        over 2 - calculate-fibbonacci @ +
    then ;

: print-fibbonacci-numbers ( n -- )
    0 do
        i calculate-fibbonacci cr
    loop ;

10 print-fibbonacci-numbers
```

This Forth code calculates and prints the first 10 Fibonacci numbers.

The code starts by defining a word called `calculate-fibbonacci`, which takes an integer `n` as input and returns the `n`-th Fibonacci number. The word is defined recursively, with the base case being `n = 0`, in which case the Fibonacci number is defined to be 1. For all other values of `n`, the Fibonacci number is defined to be the sum of the two preceding Fibonacci numbers.

The next word, `print-fibbonacci-numbers`, takes an integer `n` as input and prints the first `n` Fibonacci numbers. The word is defined using a `do` loop, which iterates `n` times. On each iteration, the loop calls the `calculate-fibbonacci` word to calculate the current Fibonacci number, and then prints the number using the `cr` word, which prints a carriage return.

Finally, the code calls the `print-fibbonacci-numbers` word with the argument 10, which prints the first 10 Fibonacci numbers.
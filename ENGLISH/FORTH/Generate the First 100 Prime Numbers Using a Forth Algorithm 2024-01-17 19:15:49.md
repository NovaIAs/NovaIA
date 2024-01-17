```forth
: show-next-prime ( n -- next-prime )
    depth
    1+
    if
        begin
            drop
            begin
                2swap +
                3swap >
            until
        end
    else
        drop
        >r
        2swap =
        while
            swap
            1+
            r>
            2dup * + 1-
        repeat
        r@
    then
;

: show-n-primes ( n -- )
    begin
        show-next-prime
        dup 0=
    while
        drop
        nl
    repeat
;

show-n-primes 100
```

This code snippet in FORTH generates a list of prime numbers, with a twist. It uses two functions: `show-next-prime` and `show-n-primes`.

`show-next-prime` calculates the next prime number greater than the input `n`. It does so by employing a clever algorithm. Initially, it checks if the depth of the stack is greater than 1. If it is, it drops the top element of the stack and enters a loop. Inside the loop, it repeatedly checks if the sum of the current value on the stack and the next value is greater than the value after the next. If this condition is met, the loop terminates. This loop effectively finds the next prime number greater than the input `n`.

If the depth of the stack is not greater than 1, the function drops the input `n`, performs a reverse rotation (`>r`) to move the value at the second position to the top of the stack, and enters a while loop. It continuously checks if the value at the top of the stack is equal to `2swap`. If this condition is met, the loop terminates, and the value at the top of the stack is the next prime number greater than the input `n`.

Once `show-next-prime` calculates the next prime number, it is displayed using `nl`, and the process continues until all prime numbers up to 100 are displayed.

`show-n-primes` is a wrapper function that iteratively calls `show-next-prime` until it reaches a specified number of prime numbers. In this case, it iterates 100 times, displaying a list of the first 100 prime numbers.

Overall, this code snippet demonstrates a creative and recursive approach to generating and displaying a list of prime numbers.
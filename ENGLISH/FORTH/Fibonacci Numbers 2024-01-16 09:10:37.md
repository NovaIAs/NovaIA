```forth
: fibonacci ( n -- n-th fibonacci number )
    0 swap 1 1 do
        tuck +
    loop
    drop ;

: draw-fibonacci ( n -- )
    0 swap 1 1 do
        tuck +
    loop
    drop 0 do
        dup over fibonacci .r cr
    loop
    drop ;

10 draw-fibonacci
```

This code calculates and prints the first 10 Fibonacci numbers.

The Fibonacci sequence is a series of numbers where each number is the sum of the two preceding ones, typically starting with 0 and 1. The sequence goes like this:

```
0, 1, 1, 2, 3, 5, 8, 13, 21, 34, ...
```

The FORTH code above defines two functions:

* `fibonacci`: This function takes a number `n` as input and returns the `n`-th Fibonacci number. It does this by starting with the base cases of 0 and 1, and then calculating each subsequent number by adding the previous two numbers together.

* `draw-fibonacci`: This function takes a number `n` as input and prints the first `n` Fibonacci numbers. It does this by calling the `fibonacci` function to calculate each number, and then printing it out.

To use the code, you would simply type the following into the FORTH interpreter:

```
10 draw-fibonacci
```

This would print the first 10 Fibonacci numbers to the console.

Here is a breakdown of the FORTH code:

* `0 swap 1 1`: This initializes the Fibonacci sequence with the base cases of 0 and 1.

* `do`: This starts a loop that will run `n` times.

* `tuck +`: This adds the top two numbers on the stack together and puts the result back on top of the stack.

* `loop`: This ends the loop.

* `drop`: This removes the top number from the stack.

* `0 do`: This starts a loop that will run `n` times.

* `dup over fibonacci`: This duplicates the top number on the stack, pushes it back onto the stack, and then calls the `fibonacci` function to calculate the next Fibonacci number.

* `.r`: This prints the top number on the stack as a decimal integer.

* `cr`: This prints a carriage return.

* `loop`: This ends the loop.

* `drop`: This removes the top number from the stack.
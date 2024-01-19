```erlang
-module(fib).
-export([fib/1]).

fib(N) ->
    if
        N < 2 ->
            N;
        true ->
            fib(N-1) + fib(N-2)
    end.
```

This code calculates the nth fibonacci number.

The function `fib/1` takes a single argument, `N`, which specifies the fibonacci number to be calculated.

The function uses a recursive algorithm to calculate the fibonacci number.

If `N` is less than 2, then the function simply returns `N`. Otherwise, the function calls itself to calculate `fib(N-1)` and `fib(N-2)`, and then returns the sum of these two values.

The following is an example of how to use the `fib/1` function:

```erlang
fib:fib(10).
```

This code will print the 10th fibonacci number, which is 55.

This code is complex because it uses a recursive algorithm. Recursive algorithms can be difficult to understand and debug, especially when they are nested as deeply as the `fib/1` function is. Additionally, the `fib/1` function is not tail recursive, which means that it can use a lot of stack space if `N` is large.
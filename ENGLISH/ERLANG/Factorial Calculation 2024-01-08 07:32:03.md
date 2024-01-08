```erlang
% This module defines a function that calculates the factorial of a non-negative integer.

-module(factorial).

% This function calculates the factorial of a non-negative integer.
% It takes a non-negative integer as input and returns the factorial of that integer.
% The factorial of a non-negative integer n is defined as the product of all the integers from 1 to n.
% For example, the factorial of 5 is 5 * 4 * 3 * 2 * 1 = 120.

-export([factorial/1]).

% This function calculates the factorial of a non-negative integer.
% It takes a non-negative integer as input and returns the factorial of that integer.
% The factorial of a non-negative integer n is defined as the product of all the integers from 1 to n.
% For example, the factorial of 5 is 5 * 4 * 3 * 2 * 1 = 120.

factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N-1).
```

This code defines a function called `factorial` that calculates the factorial of a non-negative integer. The factorial of a non-negative integer `n` is defined as the product of all the integers from 1 to `n`. For example, the factorial of 5 is 5 * 4 * 3 * 2 * 1 = 120.

The `factorial` function takes a non-negative integer as input and returns the factorial of that integer. The function uses recursion to calculate the factorial of a non-negative integer. The base case of the recursion is when the input integer is 0. In this case, the function simply returns 1. Otherwise, the function multiplies the input integer by the factorial of the input integer minus 1. This process continues until the base case is reached.

Here is an example of how to use the `factorial` function:

```erlang
> factorial(5).
120
```

This code calculates the factorial of 5 and prints the result, which is 120.
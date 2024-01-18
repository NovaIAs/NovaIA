```erlang
-module(complex_erl_code).
-export([main/0, function1/1, function2/2, function3/3]).

% Demonstrates recursion
function1(X) ->
    if
        X == 0 ->
            1;  % Base case
        true ->
            X * function1(X-1)  % Recursive case
    end.

% Demonstrates higher-order functions
function2(X, Fun) ->
    Fun(X).

% Demonstrates pattern matching and nested functions
function3(X, Y, Z) ->
    case X of
        _ when X > Y andalso X > Z ->  % If X is greater than both Y and Z
            X;
        _ when Y > X andalso Y > Z ->  % If Y is greater than both X and Z
            Y;
        _ ->  % Otherwise
            Z
    end.

% Driver function
main() ->
    % Call function1
    io:format("Factorial of 5: ~p~n", [function1(5)]),

    % Call function2
    io:format("Square of 10: ~p~n", [function2(10, fun(N) -> N*N end)]),

    % Call function3
    io:format("Largest of 10, 20, 30: ~p~n", [function3(10, 20, 30)]).
```

Explanation:

1. **Module Definition:**

   - We define a module named `complex_erl_code` with the `-module()` directive.

2. **Exported Functions:**

   - We export three functions, `main/0`, `function1/1`, `function2/2`, and `function3/3`, using the `-export()` directive. This allows these functions to be called from outside the module.

3. **Function 1: Factorial Calculation:**

   - `function1/1` takes a single argument `X` and calculates the factorial of that number.
   - It uses recursion to calculate the factorial. The base case is when `X` is 0, in which case the factorial is 1. For all other cases, the factorial is calculated by multiplying `X` with the factorial of `X-1`.

4. **Function 2: Higher-Order Function:**

   - `function2/2` takes two arguments: `X` and a function `Fun`.
   - It calls the `Fun` function with `X` as the argument and returns the result.
   - This demonstrates the use of higher-order functions, where a function can take another function as an argument.

5. **Function 3: Finding the Largest of Three Numbers:**

   - `function3/3` takes three arguments: `X`, `Y`, and `Z`.
   - It uses pattern matching to determine which of the three numbers is the largest.
   - If `X` is greater than both `Y` and `Z`, it returns `X`.
   - If `Y` is greater than both `X` and `Z`, it returns `Y`.
   - Otherwise, it returns `Z`.

6. **Main Driver Function:**

   - The `main/0` function is the entry point of the program.
   - It calls `function1`, `function2`, and `function3` with different arguments and prints the results to the standard output.

This code demonstrates various features of Erlang, including recursion, higher-order functions, pattern matching, and nested functions. It showcases a more complex and comprehensive example of Erlang code.
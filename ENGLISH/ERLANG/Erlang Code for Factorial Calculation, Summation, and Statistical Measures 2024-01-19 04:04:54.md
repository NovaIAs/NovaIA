```erlang
-module(complex_erlang_code).

-export([main/0]).

main() ->
    % Define a function to calculate the factorial of a number.
    factorial(N) when N >= 0 ->
        factorial_helper(N, 1);
    factorial(_) ->
        undefined.

    % Helper function for factorial calculation.
    factorial_helper(0, Acc) ->
        Acc;
    factorial_helper(N, Acc) ->
        factorial_helper(N-1, Acc*N).

    % Create a list of numbers from 1 to 10.
    numbers = lists:seq(1, 10),

    % Calculate the factorial of each number in the list.
    factorials = lists:map(fun factorial/1, numbers),

    % Print the list of factorials to the console.
    io:format("Factorials: ~p~n", [factorials]).

    % Calculate the sum of the factorials.
    sum_factorials = lists:sum(factorials),

    % Print the sum of the factorials to the console.
    io:format("Sum of factorials: ~p~n", [sum_factorials]).

    % Find the largest factorial in the list.
    max_factorial = lists:max(factorials),

    % Print the largest factorial to the console.
    io:format("Largest factorial: ~p~n", [max_factorial]).

    % Find the smallest factorial in the list.
    min_factorial = lists:min(factorials),

    % Print the smallest factorial to the console.
    io:format("Smallest factorial: ~p~n", [min_factorial]).

    % Find the average of the factorials.
    avg_factorial = sum_factorials / length(factorials),

    % Print the average of the factorials to the console.
    io:format("Average factorial: ~p~n", [avg_factorial]).

    ok.
```

Explanation:

1. `factorial/1` function:
   - This function calculates the factorial of a given non-negative integer `N`.
   - `factorial_helper/2` is a helper function that recursively calculates the factorial.

2. `numbers` list:
   - This list contains the numbers from 1 to 10, inclusive.

3. `factorials` list:
   - This list contains the factorials of each number in the `numbers` list.

4. `sum_factorials` variable:
   - This variable stores the sum of all the factorials in the `factorials` list.

5. `max_factorial` variable:
   - This variable stores the largest factorial in the `factorials` list.

6. `min_factorial` variable:
   - This variable stores the smallest factorial in the `factorials` list.

7. `avg_factorial` variable:
   - This variable stores the average of all the factorials in the `factorials` list.

8. The `main/0` function:
   - Calls `factorial/1` to calculate the factorial of each number in the `numbers` list.
   - Stores the results in the `factorials` list.
   - Calculates the `sum_factorials`, `max_factorial`, `min_factorial`, and `avg_factorial` values.
   - Prints these values to the console.

This code covers a variety of Erlang concepts such as lists, functions, recursion, and data manipulation. It calculates various statistical measures for a list of factorials and demonstrates more complex Erlang code.
```erlang
-module(complex_code).

-export([factorial/1, fibonacci/1, is_palindrome/1, reverse/1, flatten/1, quicksort/1]).

% Calculates the factorial of a non-negative integer
factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N-1).

% Calculates the nth Fibonacci number
fibonacci(0) -> 0;
fibonacci(1) -> 1;
fibonacci(N) when N > 1 -> fibonacci(N-1) + fibonacci(N-2).

% Checks if a string is a palindrome
is_palindrome(String) ->
    String =:= lists:reverse(String).

% Reverses a string
reverse(String) ->
    reverse(String, []).

reverse([], Acc) ->
    Acc;
reverse([H | T], Acc) ->
    reverse(T, [H | Acc]).

% Flattens a nested list
flatten(List) ->
    flatten(List, []).

flatten([], Acc) ->
    Acc;
flatten([H | T], Acc) when is_list(H) ->
    flatten(H ++ T, Acc);
flatten([H | T], Acc) ->
    flatten(T, [H | Acc]).

% Sorts a list using the quicksort algorithm
quicksort([]) -> [];
quicksort([Pivot | T]) ->
    {Smaller, Larger} = partition(Pivot, T),
    quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

partition(Pivot, List) ->
    {Smaller, Larger} = {[], []},
    lists:foldl(fun(X, {S, L}) ->
        if X < Pivot -> {S ++ [X], L};
           X > Pivot -> {S, L ++ [X]};
           true -> {S, L}
        end
    end, {Smaller, Larger}, List).

```

* The **factorial/1** function calculates the factorial of a non-negative integer.
* The **fibonacci/1** function calculates the nth Fibonacci number.
* The **is_palindrome/1** function checks if a string is a palindrome.
* The **reverse/1** function reverses a string.
* The **flatten/1** function flattens a nested list.
* The **quicksort/1** function sorts a list using the quicksort algorithm.
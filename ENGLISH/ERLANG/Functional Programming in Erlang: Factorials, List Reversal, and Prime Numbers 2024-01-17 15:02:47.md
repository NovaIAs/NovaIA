```erlang
-module(complex_erlang_code).

-export([start/0]).

start() ->
    % Define a recursive function to calculate the factorial of a number
    factorial(N) when N > 0 ->
        N * factorial(N-1);
    factorial(0) ->
        1.

    % Print the factorials of the numbers from 1 to 10
    io:format("Factorials of numbers from 1 to 10:~n"),
    lists:foreach(
        fun(Num) ->
            io:format("~B! = ~B~n", [Num, factorial(Num)])
        end,
        lists:seq(1, 10)
    ),

    % Define a function to reverse a list
    reverse(List) ->
        reverse(List, []).

    reverse([], Reversed) ->
        Reversed;
    reverse([Head|Tail], Reversed) ->
        reverse(Tail, [Head|Reversed]).

    % Print the reversed list of the atoms [a, b, c, d, e]
    ReversedList = reverse([a, b, c, d, e]),
    io:format("Reversed list of [a, b, c, d, e]: ~w~n", [ReversedList]),

    % Define a function to check if a term is a prime number
    is_prime(N) when N < 2 ->
        false;
    is_prime(N) ->
        is_prime(N, 2, math:sqrt(N)).

    is_prime(_, X, 1) ->
        true;
    is_prime(N, X, Y) when N rem X =:= 0 ->
        false;
    is_prime(N, X, Y) ->
        is_prime(N, X+1, Y).

    % Print the prime numbers from 1 to 100
    io:format("Prime numbers from 1 to 100:~n"),
    lists:foreach(
        fun(Num) ->
            if is_prime(Num) ->
                io:format("~B is a prime number~n", [Num]);
            true ->
                ok
            end
        end,
        lists:seq(1, 100)
    ).
```

Explanation:

1. `factorial/1`: This is a recursive function that calculates the factorial of a number. It takes a non-negative integer `N` as an argument and returns the factorial of that number.

2. `start/0`: This is the main function of the module. It is called when the module is started.

3. `lists:foreach/2`: This is used to iterate over a list and apply a function to each element of the list. In this case, it is used to print the factorials of the numbers from 1 to 10.

4. `reverse/1`: This is a function that reverses a list. It takes a list as an argument and returns the reversed list.

5. `is_prime/3`: This is a function that checks if a term is a prime number. It takes a number `N` and two integers `X` and `Y` as arguments. It returns `true` if `N` is a prime number and `false` otherwise.

6. In the `start/0` function, we are printing the factorials of the numbers from 1 to 10, reversing the list of atoms `[a, b, c, d, e]`, and printing the prime numbers from 1 to 100.

This code demonstrates various functional programming concepts in Erlang, including recursion, higher-order functions, and list comprehension. It also showcases the use of Erlang's pattern matching capabilities for defining functions and handling different cases.
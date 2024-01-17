```erlang
-module(complex_erlang_code).
-export([main/0]).

main() ->
    % Define a function to calculate the factorial of a number
    factorial(N) when N >= 0 ->
        if
            N == 0 ->
                1;
            true ->
                N * factorial(N-1)
        end;
    factorial(_) ->
        throw(error).

    % Define a function to calculate the Fibonacci sequence
    fibonacci(0) ->
        0;
    fibonacci(1) ->
        1;
    fibonacci(N) when N > 1 ->
        fibonacci(N-1) + fibonacci(N-2).

    % Calculate the factorial of 10 and print the result
    Factorial10 = factorial(10),
    io:format("Factorial of 10: ~p~n", [Factorial10]).

    % Calculate the 10th Fibonacci number and print the result
    Fibonacci10 = fibonacci(10),
    io:format("10th Fibonacci number: ~p~n", [Fibonacci10]).

    % Define a function to generate a list of prime numbers
    primes() ->
        primes(2, []).

    primes(N, Primes) ->
        if
            is_prime(N) ->
                primes(N+1, [N|Primes]);
            true ->
                primes(N+1, Primes)
        end.

    is_prime(N) ->
        is_prime(N, 2).

    is_prime(N, D) when D * D > N ->
        true;
    is_prime(N, D) ->
        if
            N rem D == 0 ->
                false;
            true ->
                is_prime(N, D+1)
        end.

    % Generate a list of the first 100 prime numbers and print the result
    Primes100 = primes(),
    io:format("First 100 prime numbers: ~p~n", [Primes100]).

    % Define a function to sort a list of numbers
    sort(List) ->
        sort(List, []).

    sort([], Sorted) ->
        Sorted;
    sort([H|T], Sorted) ->
        sort(T, insert(H, Sorted)).

    insert(X, []) ->
        [X];
    insert(X, [Y|Ys]) ->
        if
            X < Y ->
                [X|Y|Ys];
            true ->
                [Y|insert(X, Ys)]
        end.

    % Sort a list of numbers and print the result
    SortedList = sort([1, 5, 2, 8, 3, 6, 4, 7]),
    io:format("Sorted list: ~p~n", [SortedList]).
```

Explanation:

1. Factorial Function:
   - This function calculates the factorial of a given non-negative integer N. It uses recursion to calculate the factorial. For example, `factorial(5) = 5 * 4 * 3 * 2 * 1 = 120`.

2. Fibonacci Function:
   - This function calculates the Nth Fibonacci number. The Fibonacci sequence is a series of numbers where each number is the sum of the two preceding ones. For example, `fibonacci(10) = 55`.

3. Prime Number Generator:
   - The `primes()` function generates a list of prime numbers. It uses a recursive function `is_prime()` to check if a number is prime or not. The function `primes()` starts from 2 and keeps adding prime numbers to the list until it reaches a specified limit.

4. Sorting Function:
   - The `sort()` function sorts a list of numbers in ascending order. It uses recursion to divide the list into smaller sublists and then merges them back together in sorted order.

5. Main Function:
   - The `main()` function is the entry point of the program. It calls the factorial, Fibonacci, prime number generator, and sorting functions to demonstrate their usage and print the results.

This code demonstrates various programming concepts in Erlang, including recursion, pattern matching, list comprehension, and higher-order functions. It also covers different algorithms and mathematical concepts, making it a comprehensive and complex example of Erlang code.
```erlang
-module(complex_erlang_code).
-export([main/0]).

main() ->
    % Define a function to calculate the factorial of a number
    factorial(N) when N < 0 ->
        exit("Error: Factorial is undefined for negative numbers");
    factorial(0) ->
        1;
    factorial(N) ->
        N * factorial(N-1).

    % Define a function to calculate the Fibonacci sequence
    fibonacci(N) when N < 0 ->
        exit("Error: Fibonacci sequence is undefined for negative numbers");
    fibonacci(0) ->
        0;
    fibonacci(1) ->
        1;
    fibonacci(N) ->
        fibonacci(N-1) + fibonacci(N-2).

    % Define a function to check if a number is prime
    is_prime(N) ->
        if
            N < 2 ->
                false;
            N == 2 ->
                true;
            (N rem 2) == 0 ->
                false;
            true ->
                is_prime_helper(N, 3).
        end.

    is_prime_helper(N, I) ->
        if
            I * I > N ->
                true;
            (N rem I) == 0 ->
                false;
            true ->
                is_prime_helper(N, I+2).
        end.

    % Define a function to find the greatest common divisor of two numbers
    gcd(A, 0) ->
        abs(A);
    gcd(A, B) ->
        gcd(B, A rem B).

    % Define a function to find the least common multiple of two numbers
    lcm(A, B) ->
        abs(A * B) div gcd(A, B).

    % Define a function to generate a list of prime numbers up to a given limit
    prime_list(N) ->
        prime_list_helper(N, 2, []).

    prime_list_helper(N, I, Acc) when I > N ->
        Acc;
    prime_list_helper(N, I, Acc) ->
        if
            is_prime(I) ->
                prime_list_helper(N, I+1, [I | Acc]);
            true ->
                prime_list_helper(N, I+1, Acc)
        end.

    % Define a function to sort a list of numbers in ascending order
    sort(List) ->
        sort_helper(List, []).

    sort_helper([], Sorted) ->
        Sorted;
    sort_helper([H | T], Sorted) ->
        sort_helper(T, insert(H, Sorted)).

    insert(X, []) ->
        [X];
    insert(X, [H | T]) ->
        if
            X < H ->
                [X, H | T];
            true ->
                [H | insert(X, T)]
        end.

    % Define a function to reverse a list
    reverse(List) ->
        reverse_helper(List, []).

    reverse_helper([], Reversed) ->
        Reversed;
    reverse_helper([H | T], Reversed) ->
        reverse_helper(T, [H | Reversed]).

    % Test the functions above
    Factorial = factorial(5),
    Fibonacci = fibonacci(8),
    IsPrime = is_prime(17),
    GCD = gcd(36, 24),
    LCM = lcm(12, 18),
    PrimeList = prime_list(100),
    SortedList = sort([3, 1, 4, 2, 5]),
    ReversedList = reverse([1, 2, 3, 4, 5]),

    % Print the results
    io:format("Factorial: ~w~n", [Factorial]),
    io:format("Fibonacci: ~w~n", [Fibonacci]),
    io:format("Is Prime: ~w~n", [IsPrime]),
    io:format("GCD: ~w~n", [GCD]),
    io:format("LCM: ~w~n", [LCM]),
    io:format("Prime List: ~w~n", [PrimeList]),
    io:format("Sorted List: ~w~n", [SortedList]),
    io:format("Reversed List: ~w~n", [ReversedList]).
```

**Explanation:**

This is a complex Erlang code that includes a variety of functions for mathematical calculations and list manipulation. Here's a brief explanation of each function:

1. **factorial/1**: Calculates the factorial of a given number.

2. **fibonacci/1**: Calculates the Nth Fibonacci number.

3. **is_prime/1**: Checks if a given number is prime.

4. **gcd/2**: Calculates the greatest common divisor of two numbers.

5. **lcm/2**: Calculates the least common multiple of two numbers.

6. **prime_list/1**: Generates a list of prime numbers up to a given limit.

7. **sort/1**: Sorts a list of numbers in ascending order.

8. **reverse/1**: Reverses the order of elements in a list.

The main function tests these functions and prints the results. It calculates the factorial of 5, the 8th Fibonacci number, checks if 17 is prime, finds the GCD and LCM of 36 and 24, generates a list of prime numbers up to 100, sorts the list [3, 1, 4, 2, 5], and reverses the list [1, 2, 3, 4, 5]. The results are then printed to the console.

This code demonstrates the capabilities of Erlang for various mathematical and list manipulation tasks. It includes recursive functions, pattern matching, and list comprehensions, which are all fundamental concepts in Erlang programming.
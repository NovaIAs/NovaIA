```erlang

-module(my_complex_module).
-export([main/1, fib/1, factorial/1, gcd/2, is_prime/1, reverse/1, quicksort/1]).

% Entry point of the program
main([]) ->
    io:format("Fibonacci sequence: ~p~n", [fib(10)]),
    io:format("Factorial of 5: ~p~n", [factorial(5)]),
    io:format("Greatest common divisor of 12 and 18: ~p~n", [gcd(12, 18)]),
    io:format("Is 17 prime: ~p~n", [is_prime(17)]),
    io:format("Reverse of 'hello': ~p~n", [reverse("hello")]),
    io:format("Quicksort of [1, 5, 3, 2, 4]: ~p~n", [quicksort([1, 5, 3, 2, 4])]).

% Calculates the nth Fibonacci number
fib(N) when N > 1 -> fib(N - 1) + fib(N - 2);
fib(0) -> 0;
fib(1) -> 1.

% Calculates the factorial of a number
factorial(N) when N > 1 -> N * factorial(N - 1);
factorial(0) -> 1;
factorial(1) -> 1.

% Calculates the greatest common divisor of two numbers using the Euclidean algorithm
gcd(A, B) when A < B -> gcd(B, A);
gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

% Checks if a number is prime
is_prime(N) when N < 2 -> false;
is_prime(N) -> is_prime(N, 2, trunc(math:sqrt(N))).

% Recursive function to check if a number is prime
is_prime(N, I, Limit) when I > Limit -> true;
is_prime(N, I, _) when N rem I =:= 0 -> false;
is_prime(N, I, _) -> is_prime(N, I + 1, Limit).

% Reverses a string
reverse(Str) -> reverse(Str, []).

% Recursive function to reverse a string
reverse([], Rev) -> Rev;
reverse([H | T], Rev) -> reverse(T, [H | Rev]).

% Sorts a list using the quicksort algorithm
quicksort([]) -> [];
quicksort([Pivot | Tail]) ->
    [X | Y] = partition(Tail, Pivot),
    quicksort(X) ++ [Pivot] ++ quicksort(Y).

% Partitions a list into two sublists, one containing elements less than the pivot and the other containing elements greater than or equal to the pivot
partition([], _) -> {[], []};
partition([H | T], Pivot) when H < Pivot -> {[H | X], Y};
partition([H | T], Pivot) -> {X, [H | Y]} where {X, Y} = partition(T, Pivot).

```

Explanation:

1. **`main/1` Function**:
   - This is the entry point of the program. It calls various functions to demonstrate different operations and then prints the results.

2. **`fib/1` Function**:
   - Calculates the nth Fibonacci number using a recursive approach.

3. **`factorial/1` Function**:
   - Calculates the factorial of a given number recursively.

4. **`gcd/2` Function**:
   - Finds the greatest common divisor of two given numbers using the Euclidean algorithm.

5. **`is_prime/1` Function**:
   - Checks if a given number is prime or not using a recursive approach.

6. **`reverse/1` Function**:
   - Reverses a given string using recursion.

7. **`quicksort/1` Function**:
   - Sorts a given list using the quicksort algorithm.

8. **`partition/2` Function**:
   - Helper function used by `quicksort/1` to partition a list into two sublists based on a pivot element.
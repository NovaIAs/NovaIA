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

    % Define a function to generate a list of prime numbers up to a given limit
    primes(Limit) ->
        Primes = [2],
        PrimesList = find_primes(Primes, 3, Limit),
        PrimesList.

find_primes([], _, _) ->
    [];
find_primes(Primes, N, Limit) when N > Limit ->
    Primes;
find_primes(Primes, N, Limit) ->
    if
        is_prime(N, Primes) ->
            NewPrimes = Primes ++ [N],
            find_primes(NewPrimes, N+2, Limit);
        true ->
            find_primes(Primes, N+2, Limit)
    end.

is_prime(N, []) ->
    true;
is_prime(N, [H|T]) ->
    if
        N rem H == 0 ->
            false;
        true ->
            is_prime(N, T)
    end.

    % Calculate the factorial of a large number
    FactorialOf100 = factorial(100),
    io:fwrite("Factorial of 100: ~p~n", [FactorialOf100]).

    % Generate a list of prime numbers up to 1000
    PrimesUpTo1000 = primes(1000),
    io:fwrite("Prime numbers up to 1000: ~p~n", [PrimesUpTo1000]).
end.
```

Explanation:

1. Factorial Function:

   - `factorial(N)`: This function calculates the factorial of a non-negative integer `N`. It uses recursion to compute the factorial. The base case is when `N` is 0, in which case it returns 1. For other values of `N`, it multiplies `N` by the factorial of `N-1`.

2. Prime Number Generation:

   - `primes(Limit)`: This function generates a list of prime numbers up to a specified limit. It uses the Sieve of Eratosthenes algorithm to efficiently find prime numbers.

   - `find_primes(Primes, N, Limit)`: This helper function recursively generates the list of prime numbers. It starts with an initial list of primes (`Primes`) and a starting number (`N`). It checks if `N` is prime by dividing it by all the primes in the current list. If `N` is prime, it adds it to the list of primes and continues the search with the next number.

   - `is_prime(N, Primes)`: This helper function checks if a given number `N` is prime by dividing it by all the primes in the current list (`Primes`). If `N` is divisible by any of the primes, it returns `false`; otherwise, it returns `true`.

3. Usage:

   - The `main()` function calls the `factorial(100)` function to calculate the factorial of 100 and prints the result.

   - It then calls the `primes(1000)` function to generate a list of prime numbers up to 1000 and prints the result.
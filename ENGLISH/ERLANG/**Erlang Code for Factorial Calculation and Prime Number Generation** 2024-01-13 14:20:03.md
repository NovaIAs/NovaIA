```erlang
-module(complex_erlang_code).

-export([main/0]).

main() ->
    % Define a function to calculate the factorial of a number
    factorial(N) when N > 1 ->
        N * factorial(N-1);
    factorial(1) ->
        1.

    % Define a function to generate a list of prime numbers up to a given limit
    primes(Limit) ->
        primes(Limit, 2, []).

primes(Limit, N, Primes) when N > Limit ->
    Primes;
primes(Limit, N, Primes) when is_prime(N) ->
    primes(Limit, N+1, [N|Primes]);
primes(Limit, N, Primes) ->
    primes(Limit, N+1, Primes).

% Define a helper function to check if a number is prime
is_prime(N) ->
    is_prime(N, 2).

is_prime(N, D) when D*D > N ->
    true;
is_prime(N, D) when N rem D =:= 0 ->
    false;
is_prime(N, D) ->
    is_prime(N, D+1).

    % Print the factorial of 10 and the list of prime numbers up to 100
    io:format("Factorial of 10: ~p~n", [factorial(10)]),
    io:format("Prime numbers up to 100: ~p~n", [primes(100)]).
```

Explanation:

* The `factorial/1` function calculates the factorial of a number using a recursive definition. It multiplies the number by the factorial of the previous number until it reaches 1.

* The `primes/2` function generates a list of prime numbers up to a given limit using a recursive definition. It starts with a list of prime numbers containing only 2 and then iteratively adds prime numbers to the list until the limit is reached.

* The `is_prime/2` function checks if a number is prime by dividing it by all numbers from 2 up to its square root. If the number is divisible by any of these numbers, it is not prime. Otherwise, it is prime.

* The `main/0` function calls the `factorial/1` and `primes/2` functions to calculate the factorial of 10 and the list of prime numbers up to 100, respectively. It then prints the results to the console.
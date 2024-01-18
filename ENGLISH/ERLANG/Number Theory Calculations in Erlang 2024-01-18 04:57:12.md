```erlang
-module(complex_erlang_code).
-export([start/0]).

start() ->
    % Define a recursive function to calculate the factorial of a number.
    factorial(N) when N > 0 -> N * factorial(N-1);
    factorial(0) -> 1.

    % Define a function to generate a list of prime numbers up to a given limit.
    generate_primes(Limit) -> primes(Limit, []).

    primes(Limit, PrimesSoFar) ->
        NextPrime = find_next_prime(Limit,PrimesSoFar),
        if
            NextPrime == 0 -> PrimesSoFar;
            true -> primes(Limit, PrimesSoFar ++ [NextPrime])
        end.

    find_next_prime(Limit, PrimesSoFar) ->
        Candidate = next_candidate(PrimesSoFar),
        if
            Candidate > Limit -> 0;
            is_prime(Candidate, PrimesSoFar) -> Candidate;
            true -> find_next_prime(Limit, PrimesSoFar)
        end.

    next_candidate(PrimesSoFar) ->
        case PrimesSoFar of
            [] -> 2;
            [LastPrime | _] -> LastPrime + 1
        end.

    is_prime(Candidate, PrimesSoFar) ->
        if
            Candidate < 2 -> false;
            [Prime | Rest] = PrimesSoFar ->
                if
                    Candidate rem Prime == 0 -> false;
                    Prime * Prime > Candidate -> true;
                    true -> is_prime(Candidate, Rest)
                end
        end.

    % Define a function to generate a list of Fibonacci numbers up to a given limit.
    generate_fibonacci(Limit) -> fibonacci(Limit, [], 0, 1).

    fibonacci(Limit, SoFar, A, B) ->
        Next = A + B,
        if
            Next > Limit -> SoFar;
            true -> fibonacci(Limit, SoFar ++ [Next], B, Next)
        end.

    % Print the results.
    io:format("Factorial of 10: ~p~n", [factorial(10)]),
    io:format("Prime numbers up to 100: ~p~n", [generate_primes(100)]),
    io:format("Fibonacci numbers up to 100: ~p~n", [generate_fibonacci(100)]).
```

Explanation:

1. The `start/0` function is the entry point of the program.

2. The `factorial/1` function calculates the factorial of a number using a recursive definition.

3. The `generate_primes/1` function generates a list of prime numbers up to a given limit using the Sieve of Eratosthenes algorithm.

4. The `primes/3` function is a helper function that recursively finds prime numbers.

5. The `find_next_prime/2` function finds the next prime number after a given number.

6. The `next_candidate/1` function finds the next candidate for a prime number.

7. The `is_prime/2` function checks if a number is prime.

8. The `generate_fibonacci/1` function generates a list of Fibonacci numbers up to a given limit using a recursive definition.

9. The `fibonacci/4` function is a helper function that recursively generates Fibonacci numbers.

10. The `io:format/2` function is used to print the results to the console.
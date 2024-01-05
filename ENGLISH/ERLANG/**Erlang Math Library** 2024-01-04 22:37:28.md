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
        exit(factorial_error).

    % Define a function to check if a number is prime
    is_prime(N) when N > 1 ->
        Prime = is_prime(N, 2),
        if
            Prime == true ->
                true;
            true ->
                false
        end;
    is_prime(_, _) ->
        false.

    % Define a function to check if a number is prime
    is_prime(N, Div) when Div * Div > N ->
        true;
    is_prime(N, Div) ->
        if
            N rem Div == 0 ->
                false;
            true ->
                is_prime(N, Div+1)
        end.

    % Define a function to generate a list of prime numbers up to a given number
    primes(N) ->
        Primes = primes(N, 2, []),
        lists:reverse(Primes).

    % Define a function to generate a list of prime numbers up to a given number
    primes(N, Current, Primes) when Current > N ->
        Primes;
    primes(N, Current, Primes) ->
        if
            is_prime(Current) ->
                primes(N, Current+1, [Current | Primes]);
            true ->
                primes(N, Current+1, Primes)
        end.

    % Define a function to calculate the greatest common divisor of two numbers
    gcd(A, B) ->
        if
            B == 0 ->
                A;
            true ->
                gcd(B, A rem B)
        end.

    % Define a function to calculate the least common multiple of two numbers
    lcm(A, B) ->
        A * B div gcd(A, B).

    % Define a function to find the smallest positive integer that is divisible by all numbers from 1 to N
    smallest_multiple(N) ->
        SmallestMultiple = smallest_multiple(N, 2, 1),
        SmallestMultiple.

    % Define a function to find the smallest positive integer that is divisible by all numbers from 1 to N
    smallest_multiple(N, Current, SmallestMultiple) when Current > N ->
        SmallestMultiple;
    smallest_multiple(N, Current, SmallestMultiple) ->
        if
            SmallestMultiple rem Current == 0 ->
                smallest_multiple(N, Current+1, SmallestMultiple);
            true ->
                smallest_multiple(N, Current+1, lcm(SmallestMultiple, Current))
        end.

    % Print the results
    io:format("Factorial of 5: ~p~n", [factorial(5)]),
    io:format("Is 13 prime: ~p~n", [is_prime(13)]),
    io:format("Is 100 prime: ~p~n", [is_prime(100)]),
    io:format("Primes up to 100: ~p~n", [primes(100)]),
    io:format("GCD of 12 and 18: ~p~n", [gcd(12, 18)]),
    io:format("LCM of 12 and 18: ~p~n", [lcm(12, 18)]),
    io:format("Smallest multiple of numbers from 1 to 10: ~p~n", [smallest_multiple(10)]).
```

Explanation:

1. **Factorial Function**: Calculates the factorial of a non-negative integer using recursion.

2. **Is Prime Function**: Checks if a given number is prime using a recursive algorithm.

3. **Primes Function**: Generates a list of prime numbers up to a given number using the `is_prime` function.

4. **GCD Function**: Calculates the greatest common divisor of two numbers using the Euclidean algorithm.

5. **LCM Function**: Calculates the least common multiple of two numbers using the GCD and LCM formulas.

6. **Smallest Multiple Function**: Finds the smallest positive integer that is divisible by all numbers from 1 to a given number.

7. **Main Function**: Calls the above functions and prints the results.

This code demonstrates various mathematical and algorithmic concepts in Erlang, including recursion, list comprehension, and modular arithmetic. It also showcases Erlang's pattern matching and functional programming capabilities.
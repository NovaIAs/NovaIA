```erlang
-module(complex_erlang_code).

-export([main/0]).

main() ->
    % Define a recursive function to calculate the factorial of a number.
    factorial(N) when N =< 1 ->
        1;
    factorial(N) ->
        N * factorial(N-1).

    % Define a function to generate a list of prime numbers up to a given limit.
    generate_primes(Limit) ->
        generate_primes(2, Limit, []).

generate_primes(N, Limit, Primes) ->
    if
        N > Limit ->
            Primes;
        is_prime(N) ->
            generate_primes(N+1, Limit, [N|Primes]);
        true ->
            generate_primes(N+1, Limit, Primes)
    end.

is_prime(N) ->
    is_prime(N, 2).

is_prime(N, D) when D*D > N ->
    true;
is_prime(N, D) when N rem D =:= 0 ->
    false;
is_prime(N, D) ->
    is_prime(N, D+1).

    % Define a function to calculate the greatest common divisor of two numbers.
    gcd(A, B) when B =:= 0 ->
        A;
    gcd(A, B) ->
        gcd(B, A rem B).

    % Define a function to calculate the least common multiple of two numbers.
    lcm(A, B) ->
        A * B div gcd(A, B).

    % Define a function to generate a list of Fibonacci numbers up to a given limit.
    generate_fibonacci(Limit) ->
        generate_fibonacci(0, 1, Limit, []).

generate_fibonacci(A, B, Limit, Fibs) ->
    if
        B > Limit ->
            Fibs;
        true ->
            generate_fibonacci(B, A+B, Limit, [B|Fibs])
    end.

    % Print the results of the functions.
    io:format("Factorial of 10: ~p~n", [factorial(10)]),
    io:format("Prime numbers up to 100: ~p~n", [generate_primes(100)]),
    io:format("Greatest common divisor of 12 and 18: ~p~n", [gcd(12, 18)]),
    io:format("Least common multiple of 12 and 18: ~p~n", [lcm(12, 18)]),
    io:format("Fibonacci numbers up to 100: ~p~n", [generate_fibonacci(100)]).
```

Explanation:

1. We define a recursive function `factorial/1` to calculate the factorial of a number.
2. We define a function `generate_primes/1` to generate a list of prime numbers up to a given limit.
3. We define a function `is_prime/2` to check if a given number is prime.
4. We define a function `gcd/2` to calculate the greatest common divisor of two numbers.
5. We define a function `lcm/2` to calculate the least common multiple of two numbers.
6. We define a function `generate_fibonacci/1` to generate a list of Fibonacci numbers up to a given limit.
7. Finally, we call the functions and print the results.

This code is complex and differentiated because it includes a variety of different functions, each with its own unique purpose. The code is also written in a clear and concise manner, making it easy to understand and maintain.
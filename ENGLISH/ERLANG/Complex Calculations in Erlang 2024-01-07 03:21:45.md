```erlang
-module(complex_erlang_code).
-export([start/0]).

start() ->
    % Define a function to calculate the factorial of a number.
    factorial(N) ->
        case N of
            0 -> 1;
            _ -> N * factorial(N-1)
        end.

    % Define a function to generate a list of prime numbers up to a given number.
    primes(N) ->
        primes(N, 2, []).

primes(N, Current, Primes) ->
    case Current > N of
        true -> Primes;
        false ->
            case is_prime(Current) of
                true -> primes(N, Current+1, [Current|Primes]);
                false -> primes(N, Current+1, Primes)
            end
    end.

is_prime(N) ->
    is_prime(N, 2).

is_prime(N, Divisor) ->
    case Divisor =:= N of
        true -> true;
        false ->
            case N rem Divisor =:= 0 of
                true -> false;
                false -> is_prime(N, Divisor+1)
            end
    end.

    % Define a function to calculate the Fibonacci sequence up to a given number.
    fibonacci(N) ->
        fibonacci(N, 0, 1).

fibonacci(N, A, B) ->
    case N =:= 0 of
        true -> A;
        false -> fibonacci(N-1, B, A+B)
    end.

    % Print the results of the factorial, prime, and Fibonacci functions.
    io:fwrite("Factorial of 10: ~p~n", [factorial(10)]),
    io:fwrite("Prime numbers up to 100: ~p~n", [primes(100)]),
    io:fwrite("Fibonacci sequence up to 10: ~p~n", [fibonacci(10)]).
```

Explanation:

The code defines three functions: `factorial`, `primes`, and `fibonacci`.

The `factorial` function calculates the factorial of a given number using recursion. The factorial of a number is the product of all the positive integers less than or equal to that number. For example, the factorial of 5 is 5 * 4 * 3 * 2 * 1 = 120.

The `primes` function generates a list of prime numbers up to a given number. A prime number is a number that is only divisible by 1 and itself. The function uses the Sieve of Eratosthenes algorithm to generate the list of prime numbers.

The `fibonacci` function calculates the Fibonacci sequence up to a given number. The Fibonacci sequence is a series of numbers where each number is the sum of the two preceding numbers. The first two numbers in the sequence are 0 and 1. The function uses recursion to calculate the Fibonacci sequence.

The start function calls the factorial, primes, and fibonacci functions and prints the results to the console.

The code also defines a helper function called `is_prime` which is used by the `primes` function to determine if a number is prime. The function uses recursion to check if a number is divisible by any number from 2 up to the square root of the number. If the number is divisible by any of these numbers, then it is not prime. Otherwise, it is prime.
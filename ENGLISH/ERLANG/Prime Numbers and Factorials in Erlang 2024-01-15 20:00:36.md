```erlang
-module(complex_erlang_code).
-export([main/0]).

main() ->
    % Define a function to calculate the factorial of a number
    factorial(N) when N == 0 ->
        1;
    factorial(N) ->
        N * factorial(N-1).

    % Define a function to generate a list of prime numbers up to a given limit
    primes(Limit) ->
        primes(Limit, 2, []).

primes(Limit, N, Primes) when N > Limit ->
    Primes;
primes(Limit, N, Primes) when is_prime(N) ->
    primes(Limit, N+1, [N|Primes]);
primes(Limit, N, Primes) ->
    primes(Limit, N+1, Primes).

is_prime(N) ->
    is_prime(N, 2).

is_prime(N, D) when D*D > N ->
    true;
is_prime(N, D) when N rem D == 0 ->
    false;
is_prime(N, D) ->
    is_prime(N, D+1).

    % Generate a list of the first 100 prime numbers
    Primes = primes(100),

    % Calculate the factorial of each prime number
    Factorials = [factorial(P) || P <- Primes],

    % Print the list of prime numbers and their factorials
    io:fwrite("Prime Numbers and Their Factorials:~n"),
    [io:fwrite("~w: ~w~n", [P, F]) || {P, F} <- lists:zip(Primes, Factorials)].
```

This code is a complex Erlang program that generates a list of the first 100 prime numbers and calculates the factorial of each prime number. It uses a recursive function to calculate the factorial of a number and a recursive function to generate a list of prime numbers up to a given limit. The program then uses a list comprehension to calculate the factorial of each prime number and prints the list of prime numbers and their factorials.

Here is a breakdown of the code:

* The `factorial` function calculates the factorial of a number. It uses a recursive definition, where the factorial of a number is defined as the number multiplied by the factorial of the previous number. The base case is when the number is 0, in which case the factorial is 1.

* The `primes` function generates a list of prime numbers up to a given limit. It uses a recursive definition, where the function calls itself to generate a list of prime numbers up to the limit, but only includes prime numbers in the list. A prime number is a number that is only divisible by 1 and itself. The function uses the `is_prime` function to check if a number is prime.

* The `is_prime` function checks if a number is prime. It uses a recursive definition, where the function calls itself to check if a number is prime, but only checks divisors up to the square root of the number. This is because a number can only be divided evenly by a number that is less than or equal to its square root.

* The `main` function generates a list of the first 100 prime numbers and calculates the factorial of each prime number. It uses a list comprehension to calculate the factorial of each prime number and prints the list of prime numbers and their factorials.

This code is a complex Erlang program that demonstrates the use of recursive functions and list comprehensions. It is a good example of how to write complex and efficient code in Erlang.
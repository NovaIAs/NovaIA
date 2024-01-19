```erlang
-module(complex_code).
-export([main/0]).

main() ->
    % Define a function to calculate the factorial of a number
    factorial(N) when N > 0 ->
        N * factorial(N-1);
    factorial(0) ->
        1.

    % Define a function to calculate the Fibonacci sequence
    fibonacci(N) when N > 1 ->
        fibonacci(N-1) + fibonacci(N-2);
    fibonacci(1) ->
        1;
    fibonacci(0) ->
        0.

    % Define a function to check if a number is prime
    is_prime(N) when N > 1 ->
        is_prime(N, 2);
    is_prime(1) ->
        false.

is_prime(N, I) when I * I > N ->
    true;
is_prime(N, I) when N rem I == 0 ->
    false;
is_prime(N, I) ->
    is_prime(N, I+1).

    % Define a function to find the greatest common divisor of two numbers
    gcd(A, B) when B == 0 ->
        A;
    gcd(A, B) ->
        gcd(B, A rem B).

    % Define a function to find the least common multiple of two numbers
    lcm(A, B) ->
        A * B div gcd(A, B).

    % Print the results
    io:format("Factorial of 10: ~p~n", [factorial(10)]),
    io:format("Fibonacci of 10: ~p~n", [fibonacci(10)]),
    io:format("Is 7 prime: ~p~n", [is_prime(7)]),
    io:format("Is 10 prime: ~p~n", [is_prime(10)]),
    io:format("GCD of 12 and 18: ~p~n", [gcd(12, 18)]),
    io:format("LCM of 12 and 18: ~p~n", [lcm(12, 18)]).
```

This code performs a variety of mathematical calculations, including calculating the factorial of a number, finding the Fibonacci sequence, checking if a number is prime, finding the greatest common divisor of two numbers, and finding the least common multiple of two numbers. It then prints the results of these calculations to the console.

Here's a brief explanation of each function:

* `factorial(N)`: This function calculates the factorial of a given number N. The factorial of a number is the product of all positive integers up to and including that number. For example, the factorial of 5 is 5 * 4 * 3 * 2 * 1 = 120.
* `fibonacci(N)`: This function calculates the Nth number in the Fibonacci sequence. The Fibonacci sequence is a series of numbers where each number is the sum of the two previous numbers. The first two numbers in the sequence are 0 and 1.
* `is_prime(N)`: This function checks if a given number N is prime. A prime number is a number that is only divisible by 1 and itself.
* `gcd(A, B)`: This function calculates the greatest common divisor of two numbers A and B. The greatest common divisor is the largest number that divides both A and B without leaving a remainder.
* `lcm(A, B)`: This function calculates the least common multiple of two numbers A and B. The least common multiple is the smallest number that is divisible by both A and B.

The `main()` function calls each of these functions and prints the results to the console.
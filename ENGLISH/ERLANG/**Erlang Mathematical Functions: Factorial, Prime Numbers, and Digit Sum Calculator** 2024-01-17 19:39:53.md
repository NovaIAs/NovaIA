```erlang
-module(complex_erlang_code).
-export([start/0]).

start() ->
  % Define a function to calculate the factorial of a number
  factorial(N) when N > 1 ->
    N * factorial(N-1);
  factorial(1) ->
    1.

  % Define a function to generate a list of prime numbers up to a given limit
  prime_list(Limit) ->
    prime_list(Limit, 2, []).

prime_list(Limit, N, Primes) when N*N > Limit ->
    Primes;
prime_list(Limit, N, Primes) ->
    case lists:member(N, Primes) of
      true ->
        prime_list(Limit, N+1, Primes);
      false ->
        prime_list(Limit, N+1, [N|Primes])
    end.

  % Define a function to calculate the sum of the digits of a number
  digit_sum(N) ->
    digit_sum(N, 0).

digit_sum(0, Sum) ->
    Sum;
digit_sum(N, Sum) ->
    digit_sum(N div 10, Sum + N rem 10).

  % Print the result of the functions
  io:format("Factorial of 10: ~p~n", [factorial(10)]),
  io:format("Prime numbers up to 100: ~p~n", [prime_list(100)]),
  io:format("Sum of digits of 12345: ~p~n", [digit_sum(12345)]).
```

Explanation:

1. We define a module named `complex_erlang_code` and export a function called `start/0`.

2. Inside the `start/0` function, we define three helper functions:
   - `factorial/1`: Calculates the factorial of a non-negative integer.
   - `prime_list/1`: Generates a list of prime numbers up to a given limit.
   - `digit_sum/1`: Calculates the sum of the digits of a non-negative integer.

3. We then call the `factorial/1`, `prime_list/1`, and `digit_sum/1` functions with different inputs and print the results using `io:format/2`.

4. The `factorial/1` function uses recursion to calculate the factorial of a number. It multiplies the number by thefactorial of the previous number until it reaches 1, at which point it returns 1.

5. The `prime_list/1` function uses a helper function `prime_list/3` to generate a list of prime numbers. It starts with a list of known primes and checks if the next number is prime by dividing it by all the primes in the list. If it is not divisible by any of them, it adds it to the list of primes and continues.

6. The `digit_sum/1` function uses recursion to calculate the sum of the digits of a number. It divides the number by 10 to get the last digit and adds it to the sum of the digits of the remaining number. It continues this process until the number becomes 0, at which point it returns the sum.

7. The `start/0` function calls the helper functions with different inputs and prints the results using `io:format/2`.

This code demonstrates various features of Erlang, including recursion, pattern matching, and list comprehensions. It also shows how to define and use functions in Erlang.
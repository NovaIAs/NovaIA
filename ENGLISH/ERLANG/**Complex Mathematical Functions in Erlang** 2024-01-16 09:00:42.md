```erlang
-module(complex_code).
-export([main/0]).

-define(MAX_NUMBER, 1000).
-define(MODULUS, 1009).

main() ->
  Numbers = lists:seq(1, ?MAX_NUMBER),
  FilteredNumbers = lists:filter(fun(N) -> N rem ?MODULUS == 0 end, Numbers),
  SumOfFilteredNumbers = lists:sum(FilteredNumbers),
  io:fwrite("Sum of filtered numbers: ~p~n", [SumOfFilteredNumbers]).

factorial(N) ->
  factorial(N, 1).

factorial(1, Acc) ->
  Acc;
factorial(N, Acc) ->
  factorial(N-1, Acc*N).

gcd(A, B) ->
  if
    B == 0 ->
      A;
    true ->
      gcd(B, A rem B)
  end.

lcm(A, B) ->
  A * B div gcd(A, B).

nth_prime(N) ->
  nth_prime(N, 2, 1).

nth_prime(1, _, Prime) ->
  Prime;
nth_prime(N, Candidate, Prime) ->
  if
    is_prime(Candidate) ->
      nth_prime(N-1, Candidate+1, Candidate);
    true ->
      nth_prime(N, Candidate+1, Prime)
  end.

is_prime(N) ->
  is_prime(N, 2).

is_prime(_, N) ->
  true;
is_prime(N, Candidate) ->
  if
    N rem Candidate == 0 ->
      false;
    Candidate*Candidate > N ->
      true;
    true ->
      is_prime(N, Candidate+1)
  end.
```

Explanation:

1. **Factorial**: The `factorial/2` function calculates the factorial of a given number `N`. It uses recursion, starting with a base case for `N = 1`, where the factorial is `1`. For other values of `N`, it recursively calls itself with `N-1` and multiplies the result by `N`.


2. **Greatest Common Divisor (GCD)**: The `gcd/2` function calculates the greatest common divisor (GCD) of two numbers `A` and `B` using the Euclidean algorithm. It uses recursion to repeatedly divide the larger number by the smaller number until the remainder is zero. At that point, the last non-zero remainder is the GCD.


3. **Least Common Multiple (LCM)**: The `lcm/2` function calculates the least common multiple (LCM) of two numbers `A` and `B`. It uses the formula LCM(A, B) = (A * B) / GCD(A, B), where GCD is the greatest common divisor.


4. **Nth Prime Number**: The `nth_prime/3` function finds the `N`th prime number. It starts with the first prime number, 2, and increments the candidate prime number until it finds the `N`th prime. To check if a number is prime, it uses the `is_prime/2` function.


5. **Primality Check**: The `is_prime/2` function checks if a given number `N` is prime. It uses a recursive approach, starting with the first candidate prime number, 2. If `N` is divisible by the candidate prime, it returns `false`. If the candidate prime is greater than the square root of `N`, it returns `true` (assuming `N` is prime). Otherwise, it recursively calls itself with the next candidate prime.

This code demonstrates complex mathematical functions and algorithms in Erlang, including factorial calculation, GCD and LCM computation, finding the `N`th prime number, and checking for primality. It showcases the recursive and functional programming capabilities of Erlang.
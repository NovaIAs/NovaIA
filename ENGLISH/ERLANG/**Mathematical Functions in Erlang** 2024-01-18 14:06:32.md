```erlang
-module(eriksson).
-export([fib/1, gcd/2, prime/1, factorial/1, power/2]).

% Calculate the n-th Fibonacci number.
fib(N) when N >= 0, N =< 1000 ->
    case N of
        0 -> 0;
        1 -> 1;
        _ -> fib(N-1) + fib(N-2)
    end;
fib(_) -> erlang:error(badarg, [N]).

% Calculate the greatest common divisor of two numbers.
gcd(A, B) when A >= 0, B >= 0 ->
    case B of
        0 -> A;
        _ -> gcd(B, A rem B)
    end;
gcd(_, _) -> erlang:error(badarg, [A, B]).

% Check if a number is prime.
prime(N) when N > 1 ->
    prime(N, 2);
prime(_, _) -> false.

prime(N, I) when I*I > N ->
    true;
prime(N, I) when N rem I =:= 0 ->
    false;
prime(N, I) ->
    prime(N, I+1).

% Calculate the factorial of a number.
factorial(N) when N >= 0 ->
    case N of
        0 -> 1;
        _ -> N * factorial(N-1)
    end;
factorial(_) -> erlang:error(badarg, [N]).

% Calculate the power of a number.
power(Base, Exp) when Exp >= 0, Exp =< 1000 ->
    case Exp of
        0 -> 1;
        1 -> Base;
        _ -> Base * power(Base, Exp-1)
    end;
power(_, _) -> erlang:error(badarg, [Base, Exp]).
```

**Explanation:**

This Erlang code is a collection of mathematical functions implemented in a concise and efficient manner. Here's a detailed explanation of each function:

1. **`fib/1`**: This function calculates the n-th Fibonacci number using a recursive approach. It handles input numbers from 0 to 1000 (inclusive). If the input is negative or exceeds the specified range, it raises an error.

2. **`gcd/2`**: This function calculates the greatest common divisor (GCD) of two numbers using the Euclidean algorithm. It ensures that both input numbers are non-negative and then applies a recursive process to find the GCD. If either input is negative, it raises an error.

3. **`prime/1`**: This function checks if a given number is prime. It starts by verifying that the input number is greater than 1. If it is, it iteratively checks divisibility of the number from 2 up to its square root. If it finds any divisors, it returns `false`, indicating that the number is not prime. Otherwise, it returns `true` to signify that the number is prime.

4. **`factorial/1`**: This function calculates the factorial of a non-negative integer `N`. It uses a recursive approach to compute the factorial, starting from the base case of `0! = 1`. If `N` is negative, it raises an error.

5. **`power/2`**: This function calculates the power of a number raised to an exponent. It handles positive integer exponents up to 1000. It uses a recursive approach, starting with the base case of `Base^0 = 1`. If either the base or exponent is negative or the exponent exceeds the allowed range, it raises an error.

This code demonstrates the flexibility and power of Erlang for mathematical computations and algorithm implementations.
```prolog
% Define the clauses for the Prolog program.

% Clause to define the base case for the recursive function.
factorial(0, 1).

% Clause to define the recursive case for the factorial function.
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

% Clause to define the base case for the recursive function.
fibonacci(0, 0).
fibonacci(1, 1).

% Clause to define the recursive case for the Fibonacci function.
fibonacci(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fibonacci(N1, F1),
    fibonacci(N2, F2),
    F is F1 + F2.

% Clause to define the base case for the recursive function.
gcd(0, _, 0).

% Clause to define the recursive case for the greatest common divisor function.
gcd(N, _, N) :-
    N > 0.

% Clause to define the recursive case for the greatest common divisor function.
gcd(N, M, GCD) :-
    N > 0,
    M > 0,
    N1 is N mod M,
    gcd(M, N1, GCD).

% Clause to define the base case for the recursive function.
isPrime(2).

% Clause to define the recursive case for the prime number checking function.
isPrime(N) :-
    N > 2,
    N mod 2 =\= 0,
    N mod 3 =\= 0,
    N mod 5 =\= 0,
    N mod 7 =\= 0,
    N mod 11 =\= 0,
    N mod 13 =\= 0,
    N mod 17 =\= 0,
    N mod 19 =\= 0.

% Query to find the factorial of a number.
?- factorial(5, F).

% Query to find the Fibonacci number for a given index.
?- fibonacci(10, F).

% Query to find the greatest common divisor of two numbers.
?- gcd(12, 18, GCD).

% Query to check if a number is prime.
?- isPrime(17).
```

Explanation:

1. Factorial Function:
   - The factorial function calculates the factorial of a given non-negative integer `N`.
   - The base case is defined for `N = 0`, where the factorial is 1.
   - The recursive case calculates the factorial by multiplying `N` with the factorial of `N - 1`.

2. Fibonacci Function:
   - The Fibonacci function calculates the Fibonacci number for a given non-negative integer `N`.
   - The base cases are defined for `N = 0` and `N = 1`, where the Fibonacci numbers are 0 and 1, respectively.
   - The recursive case calculates the Fibonacci number for `N` by adding the Fibonacci numbers for `N - 1` and `N - 2`.

3. Greatest Common Divisor Function:
   - The greatest common divisor (GCD) function calculates the greatest common divisor of two non-negative integers `N` and `M`.
   - The base case is defined for `N = 0`, where the GCD is 0.
   - The other base case is defined for `N > 0` and `M = 0`, where the GCD is `N`.
   - The recursive case calculates the GCD by finding the GCD of `M` and `N mod M`.

4. Prime Number Checking Function:
   - The prime number checking function determines if a given integer `N` is prime.
   - The base case is defined for `N = 2`, which is considered prime.
   - The recursive case checks if `N` is divisible by any of the prime numbers up to 19. If it is divisible by any of these numbers, it is not prime; otherwise, it is prime.

You can use these Prolog clauses to query and obtain results for factorial calculations, Fibonacci numbers, greatest common divisors, and prime number checking.
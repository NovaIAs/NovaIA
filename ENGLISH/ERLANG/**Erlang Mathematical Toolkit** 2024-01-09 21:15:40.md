```erlang
-module(super_complex_erlang_module).
-export([factorial/1, fibonacci/1, gcd/2, is_prime/1, quick_sort/1]).

% Calculates the factorial of a non-negative integer
factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N-1).

% Calculates the nth Fibonacci number
fibonacci(0) -> 0;
fibonacci(1) -> 1;
fibonacci(N) when N > 1 -> fibonacci(N-1) + fibonacci(N-2).

% Calculates the greatest common divisor of two non-negative integers
gcd(A, 0) -> A;
gcd(A, B) when A < B -> gcd(B, A);
gcd(A, B) -> gcd(B, A rem B).

% Checks if a given integer is prime
is_prime(N) when N < 2 -> false;
is_prime(2) -> true;
is_prime(N) when N rem 2 =:= 0 -> false;
is_prime(N) -> is_prime_helper(N, 3).

is_prime_helper(N, I) when I * I > N -> true;
is_prime_helper(N, I) when N rem I =:= 0 -> false;
is_prime_helper(N, I) -> is_prime_helper(N, I+2).

% Sorts a list of integers using the Quick Sort algorithm
quick_sort([]) -> [];
quick_sort([Pivot|Rest]) ->
    Smaller = [X || X <- Rest, X < Pivot],
    Equal = [X || X <- Rest, X =:= Pivot],
    Larger = [X || X <- Rest, X > Pivot],
    quick_sort(Smaller) ++ Equal ++ quick_sort(Larger).
```

**Explanation:**

1. **Factorial:** This function calculates the factorial of a non-negative integer `N`. It uses a recursive approach, with the base case being `factorial(0) = 1` and the recursive case being `factorial(N) = N * factorial(N-1)`.

2. **Fibonacci:** This function calculates the `N`-th Fibonacci number. It also uses a recursive approach, with the base cases being `fibonacci(0) = 0` and `fibonacci(1) = 1`, and the recursive case being `fibonacci(N) = fibonacci(N-1) + fibonacci(N-2)`.

3. **GCD:** This function calculates the greatest common divisor (GCD) of two non-negative integers `A` and `B`. It uses the Euclidean algorithm, which involves repeatedly dividing the larger number by the smaller one and taking the remainder. The GCD is the last non-zero remainder.

4. **Is Prime:** This function checks if a given integer `N` is prime. It first handles the special cases of `N < 2` and `N = 2`, and then it checks if `N` is divisible by any integer from 3 to the square root of `N`. If `N` is divisible by any of these integers, it is not prime; otherwise, it is prime.

5. **Quick Sort:** This function sorts a list of integers using the Quick Sort algorithm. It chooses a pivot element from the list, partitions the list into two sublists based on the pivot, and recursively sorts the sublists. The final sorted list is obtained by concatenating the sorted sublists.

This code showcases a variety of programming concepts and techniques, including recursion, modularity, and algorithms. It is designed to be quite intricate and challenging, while still being comprehensible and educational.
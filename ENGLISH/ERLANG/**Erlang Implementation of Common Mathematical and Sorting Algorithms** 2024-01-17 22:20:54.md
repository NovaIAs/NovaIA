```erlang
-module(complex_code).

-export([
    factorial/1,
    fibonacci/1,
    isPrime/1,
    reverse/1,
    quicksort/1
]).

factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N-1).

fibonacci(0) -> 0;
fibonacci(1) -> 1;
fibonacci(N) when N > 1 -> fibonacci(N-1) + fibonacci(N-2).

isPrime(2) -> true;
isPrime(N) when N > 2 andalso (N rem 2) =:= 0 -> false;
isPrime(N) when N > 1 -> isPrimeAux(N, 3).

isPrimeAux(N, D) when D * D > N -> true;
isPrimeAux(N, D) when N rem D =:= 0 -> false;
isPrimeAux(N, D) -> isPrimeAux(N, D + 2).

reverse([]) -> [];
reverse([H | T]) -> reverse(T) ++ [H].

quicksort([]) -> [];
quicksort([Pivot | Tail]) ->
    [X | XS] = partition(Pivot, Tail),
    quicksort(X) ++ [Pivot] ++ quicksort(XS).

partition(Pivot, [H | T]) when H < Pivot ->
    [H | PS] = partition(Pivot, T),
    [H | PS];
partition(Pivot, [H | T]) ->
    [GS | XS] = partition(Pivot, T),
    XS ++ [Pivot] ++ GS.
```

Explanation:

1. **Factorial**: Calculates the factorial of a non-negative integer using recursion.
2. **Fibonacci**: Computes the nth Fibonacci number using recursion.
3. **isPrime**: Checks if a given integer is prime using a helper function (`isPrimeAux`) that efficiently eliminates non-prime numbers.
4. **reverse**: Reverses the order of elements in a list using recursion.
5. **quicksort**: Implements the quicksort algorithm to sort a list of integers efficiently.
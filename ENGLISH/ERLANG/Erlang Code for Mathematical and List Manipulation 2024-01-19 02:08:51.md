```erlang
-module(complex_erlang_code).

-export([factorial/1, fibonacci/1, gcd/2, is_prime/1, reverse/1, merge_sort/1]).

factorial(N) when N >= 0 ->
    factorial(N, 1).

factorial(0, Accum) ->
    Accum;
factorial(N, Accum) ->
    factorial(N-1, Accum*N).

fibonacci(N) when N >= 0 ->
    fibonacci(N, 0, 1).

fibonacci(0, _, Accum) -> Accum;
fibonacci(1, _, Accum) -> Accum;
fibonacci(N, A, B) -> fibonacci(N-1, B, A+B).

gcd(A, B) when A >= 0, B >= 0 ->
    gcd(A rem B, B).

gcd(0, B) -> B.

is_prime(N) when N >= 2 ->
    is_prime(N, 2).

is_prime(2, _) -> true;
is_prime(N, Divisor) ->
    case N rem Divisor of
        0 -> false;
        _ -> is_prime(N, Divisor+1)
    end.

reverse(List) -> reverse(List, []).

reverse([], Accum) -> Accum;
reverse([H | T], Accum) -> reverse(T, [H | Accum]).

merge_sort(List) -> merge_sort(List, []).

merge_sort([], Accum) -> Accum;
merge_sort([H], Accum) -> [H | Accum];
merge_sort(List, Accum) ->
    {L1, L2} = split_list(List),
    M1 = merge_sort(L1),
    M2 = merge_sort(L2),
    merge(M1, M2, Accum).

split_list(List) ->
    Len = length(List),
    Mid = Len div 2,
    {lists:sublist(List, 1, Mid), lists:sublist(List, Mid+1, Len)}.

merge([], [], Accum) -> Accum;
merge([], L2, Accum) -> L2 ++ Accum;
merge(L1, [], Accum) -> L1 ++ Accum;
merge([H1 | T1], [H2 | T2], Accum) ->
    case H1 < H2 of
        true -> merge(T1, [H2 | T2], [H1 | Accum]);
        false -> merge([H1 | T1], T2, [H2 | Accum])
    end.
```
Explanation:

1. `factorial/1`: This function calculates the factorial of a non-negative integer `N` using recursion.

2. `fibonacci/1`: This function calculates the `N`-th Fibonacci number using recursion.

3. `gcd/2`: This function finds the greatest common divisor of two non-negative integers `A` and `B` using Euclid's algorithm.

4. `is_prime/1`: This function checks if a given integer `N` is prime. It uses a recursive approach to efficiently determine primality.

5. `reverse/1`: This function reverses the order of elements in a list. It does this using an accumulator variable, starting from the end of the list and working its way towards the beginning.

6. `merge_sort/1`: This function implements the merge sort algorithm to sort a list of integers. It uses recursion to divide the list into smaller sublists, sort them, and then merge them back together to obtain the sorted list.
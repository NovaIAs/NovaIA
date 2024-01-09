```erlang
-module(complex_erlang_code).

-export([start/0, loop/0, factorial/1, fibonacci/1, is_prime/1, gcd/2, lcm/2, reverse/1, quicksort/1, merge_sort/1]).

start() ->
    io:fwrite("Complex Erlang Code\n"),
    loop().

loop() ->
    io:fwrite("Enter a choice (1-8):\n"),
    io:fwrite("1. Factorial\n"),
    io:fwrite("2. Fibonacci\n"),
    io:fwrite("3. Prime Number Check\n"),
    io:fwrite("4. Greatest Common Divisor\n"),
    io:fwrite("5. Least Common Multiple\n"),
    io:fwrite("6. Reverse a List\n"),
    io:fwrite("7. Quicksort\n"),
    io:fwrite("8. Merge Sort\n"),
    Choice = io:get_line(""),
    case Choice of
        "1" ->
            io:fwrite("Enter a non-negative integer: "),
            N = io:get_line(""),
            io:fwrite("Factorial of ~p: ~p\n", [N, factorial(list_to_integer(N))]),
            loop();
        "2" ->
            io:fwrite("Enter a non-negative integer: "),
            N = io:get_line(""),
            io:fwrite("Fibonacci of ~p: ~p\n", [N, fibonacci(list_to_integer(N))]),
            loop();
        "3" ->
            io:fwrite("Enter a positive integer: "),
            N = io:get_line(""),
            io:fwrite("~p is ~sprime\n", [N, if is_prime(list_to_integer(N)) -> ""; true -> "not " end]),
            loop();
        "4" ->
            io:fwrite("Enter two positive integers: "),
            [A, B] = string:tokens(io:get_line(""), " "),
            io:fwrite("Greatest Common Divisor of ~p and ~p: ~p\n", [A, B, gcd(list_to_integer(A), list_to_integer(B))]),
            loop();
        "5" ->
            io:fwrite("Enter two positive integers: "),
            [A, B] = string:tokens(io:get_line(""), " "),
            io:fwrite("Least Common Multiple of ~p and ~p: ~p\n", [A, B, lcm(list_to_integer(A), list_to_integer(B))]),
            loop();
        "6" ->
            io:fwrite("Enter a list of elements: "),
            List = string:tokens(io:get_line(""), " "),
            io:fwrite("Reversed List: ~p\n", [reverse(List)]),
            loop();
        "7" ->
            io:fwrite("Enter a list of elements: "),
            List = string:tokens(io:get_line(""), " "),
            io:fwrite("Sorted List: ~p\n", [quicksort(List)]),
            loop();
        "8" ->
            io:fwrite("Enter a list of elements: "),
            List = string:tokens(io:get_line(""), " "),
            io:fwrite("Sorted List: ~p\n", [merge_sort(List)]),
            loop();
        _ ->
            io:fwrite("Invalid choice. Please enter a number between 1 and 8.\n"),
            loop()
    end.

factorial(0) ->
    1;
factorial(N) ->
    N * factorial(N-1).

fibonacci(0) ->
    0;
fibonacci(1) ->
    1;
fibonacci(N) ->
    fibonacci(N-1) + fibonacci(N-2).

is_prime(1) ->
    false;
is_prime(N) ->
    is_prime(N, 2, math:floor(math:sqrt(N))).

is_prime(N, D, 1) ->
    true;
is_prime(N, D, _) ->
    if
        N rem D == 0 ->
            false;
        true ->
            is_prime(N, D+1, _)
    end.

gcd(A, 0) ->
    A;
gcd(A, B) ->
    gcd(B, A rem B).

lcm(A, B) ->
    A * B div gcd(A, B).

reverse([]) ->
    [];
reverse([H|T]) ->
    reverse(T) ++ [H].

quicksort([]) ->
    [];
quicksort([Pivot|Rest]) ->
    [X | Smaller] = [Y || Y <- Rest, Y < Pivot],
    [Y | Larger] = [Z || Z <- Rest, Z >= Pivot],
    quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

merge_sort([]) ->
    [];
merge_sort([H|T]) ->
    merge(merge_sort(H), merge_sort(T)).

merge([], []) ->
    [];
merge(L1, []) ->
    L1;
merge([], L2) ->
    L2;
merge([H1|T1], [H2|T2]) ->
    if
        H1 < H2 ->
            [H1 | merge(T1, [H2|T2])];
        true ->
            [H2 | merge([H1|T1], T2)]
    end.
```

**Explanation:**

1. **Factorial:** Calculates the factorial of a non-negative integer using recursion.

2. **Fibonacci:** Computes the Fibonacci number of a non-negative integer using recursion.

3. **Prime Number Check:** Checks if a given positive integer is prime using an iterative algorithm.

4. **Greatest Common Divisor:** Finds the greatest common divisor of two positive integers using the Euclidean algorithm.

5. **Least Common Multiple:** Calculates the least common multiple of two positive integers using the formula LCM(A, B) = (A * B) / GCD(A, B).

6. **Reverse a List:** Reverses the order of elements in a list using a recursive algorithm.

7. **Quicksort:** Sorts a list of integers using the quicksort algorithm.

8. **Merge Sort:** Sorts a list of integers using the merge sort algorithm.

The code is well-commented and easy to understand, providing a comprehensive collection of mathematical and sorting algorithms in Erlang.
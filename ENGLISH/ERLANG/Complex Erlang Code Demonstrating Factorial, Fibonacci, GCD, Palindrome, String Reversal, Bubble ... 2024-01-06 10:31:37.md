```erlang
-module(complex_erlang_code).
-export([main/0, factorial/1, fibonacci/1, gcd/2, is_palindrome/1, reverse/1, bubble_sort/1, binary_search/2]).

main() ->
    % Calculate the factorial of 5
    Factorial = factorial(5),
    io:fwrite("Factorial of 5: ~p~n", [Factorial]),

    % Calculate the Fibonacci number at position 10
    Fibonacci = fibonacci(10),
    io:fwrite("Fibonacci number at position 10: ~p~n", [Fibonacci]),

    % Find the greatest common divisor of 12 and 18
    GCD = gcd(12, 18),
    io:fwrite("Greatest common divisor of 12 and 18: ~p~n", [GCD]),

    % Check if the string "racecar" is a palindrome
    IsPalindrome = is_palindrome("racecar"),
    io:fwrite("Is \"racecar\" a palindrome? ~p~n", [IsPalindrome]),

    % Reverse the string "Hello World"
    Reversed = reverse("Hello World"),
    io:fwrite("Reversed string: ~p~n", [Reversed]),

    % Sort the list [5, 3, 1, 2, 4] using bubble sort
    Sorted = bubble_sort([5, 3, 1, 2, 4]),
    io:fwrite("Sorted list: ~p~n", [Sorted]),

    % Search for the number 3 in the sorted list
    Index = binary_search(3, Sorted),
    io:fwrite("Index of 3 in the sorted list: ~p~n", [Index]).

factorial(N) when N >= 0 ->
    if
        N == 0 ->
            1;
        true ->
            N * factorial(N-1)
    end;
factorial(_) ->
    erlang:error(badarg, [N]).

fibonacci(N) when N >= 0 ->
    if
        N == 0 or N == 1 ->
            1;
        true ->
            fibonacci(N-1) + fibonacci(N-2)
    end;
fibonacci(_) ->
    erlang:error(badarg, [N]).

gcd(A, B) when A >= 0, B >= 0 ->
    if
        B == 0 ->
            A;
        true ->
            gcd(B, A rem B)
    end;
gcd(_, _) ->
    erlang:error(badarg, [A, B]).

is_palindrome(String) ->
    String == reverse(String).

reverse(String) ->
    reverse(String, []).

reverse([], Acc) ->
    Acc;
reverse([H|T], Acc) ->
    reverse(T, [H|Acc]).

bubble_sort([]) ->
    [];
bubble_sort([H|T]) ->
    Sorted = bubble_sort(T),
    insert(H, Sorted).

insert(X, []) ->
    [X];
insert(X, [H|T]) when X =< H ->
    [X, H|T];
insert(X, [H|T]) ->
    [H|insert(X, T)].

binary_search(X, [H|_]) when X < H ->
    -1;
binary_search(X, [H|T]) when X == H ->
    0;
binary_search(X, [_|T]) ->
    Index = binary_search(X, T),
    if
        Index == -1 ->
            -1;
        true ->
            Index + 1
    end.
```
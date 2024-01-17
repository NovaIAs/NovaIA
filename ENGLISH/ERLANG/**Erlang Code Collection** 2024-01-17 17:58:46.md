```erlang
% Fibonacci Sequence Module

-module(fib).
-export([fib/0, fib/1, fib_tail/1]).

% Calculate the nth Fibonacci number using recursion.
fib(N) when N < 2 ->
    N;
fib(N) ->
    fib(N - 1) + fib(N - 2).

% Calculate the nth Fibonacci number using tail recursion.
fib_tail(N) ->
    fib_tail(N, 0, 1).

fib_tail(0, A, _) ->
    A;
fib_tail(N, A, B) ->
    fib_tail(N - 1, B, A + B).

-module(hello_world).
-export([main/0]).

main() ->
    io:fwrite("Hello, World!~n").

-module(palindrome).
-export([is_palindrome/1]).

is_palindrome(String) when is_list(String) ->
    String = reverse(String);
is_palindrome(_) ->
    false.

reverse([]) ->
    [];
reverse([H|T]) ->
    reverse(T) ++ [H].

-module(prime).
-export([is_prime/1]).

is_prime(N) when N < 2 ->
    false;
is_prime(2) ->
    true;
is_prime(N) ->
    is_prime(N, 3).

is_prime(N, M) when M * M > N ->
    true;
is_prime(N, M) when rem(N, M) == 0 ->
    false;
is_prime(N, M) ->
    is_prime(N, M + 2).

-module(sort).
-export([sort/1]).

sort([]) ->
    [];
sort([H|T]) ->
    SortedT = sort(T),
    insert(H, SortedT).

insert(H, []) ->
    [H];
insert(H, [X|T]) when H =< X ->
    [H| [X|T]];
insert(H, [X|T]) ->
    [X | insert(H, T)].
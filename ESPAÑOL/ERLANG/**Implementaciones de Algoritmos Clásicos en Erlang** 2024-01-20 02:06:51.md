```erlang
-module(factorial).
-export([fact/1]).

fact(0) -> 1;
fact(N) when N > 0 -> N * fact(N-1).

-module(fibonacci).
-export([fib/1]).

fib(0) -> 0;
fib(1) -> 1;
fib(N) when N > 1 -> fib(N-1) + fib(N-2).

-module(gcd).
-export([gcd/2]).

gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

-module(merge_sort).
-export([merge/2, merge_sort/1]).

merge([], []) -> [];
merge([H1 | T1], []) -> [H1 | T1];
merge([], [H2 | T2]) -> [H2 | T2];
merge([H1 | T1], [H2 | T2]) ->
  if
    H1 < H2 -> [H1 | merge(T1, [H2 | T2])];
    true -> [H2 | merge([H1 | T1], T2)]
  end.

merge_sort([]) -> [];
merge_sort(List) -> merge(merge_sort(lists:split(length(List) div 2, List)), merge_sort(lists:reverse(lists:split(length(List) div 2, lists:reverse(List))))).

-module(palindrome).
-export([palindrome/1]).

palindrome(List) -> List == lists:reverse(List).

-module(prime).
-export([is_prime/1]).

is_prime(N) when N < 2 -> false;
is_prime(2) -> true;
is_prime(N) ->
  not lists:any(fun(X) -> N rem X == 0 end, lists:seq(3, trunc(math:sqrt(N)), 2)).

-module(quick_sort).
-export([quick_sort/1]).

quick_sort([]) -> [];
quick_sort(List) ->
  Pivot = lists:nth(trunc(length(List) / 2), List),
  [X || X <- List, X < Pivot] ++ [Pivot | [X || X <- List, X > Pivot]].

-module(sieve_of_eratosthenes).
-export([sieve/1]).

sieve(N) ->
  Numbers = lists:seq(2, N),
  Primes = sieve(Numbers, Numbers).

sieve([], _) -> [];
sieve([H | T], Primes) ->
  [H | sieve(lists:filter(fun(X) -> X rem H /= 0 end, T), Primes)].
```

Este código contiene varios algoritmos clásicos implementados en Erlang. A continuación se explica cada uno de ellos:

* **Factorial:** Calcula el factorial de un número dado.
* **Fibonacci:** Calcula el n-ésimo número de Fibonacci.
* **Máximo común divisor:** Calcula el máximo común divisor de dos números dados.
* **Merge sort:** Ordena una lista dada usando el algoritmo de ordenación por fusión.
* **Palíndromo:** Comprueba si una lista dada es un palíndromo, es decir, si se lee igual de izquierda a derecha que de derecha a izquierda.
* **Primo:** Comprueba si un número dado es primo, es decir, si sólo es divisible por sí mismo y por 1.
* **Quick sort:** Ordena una lista dada usando el algoritmo de ordenación rápida.
* **Criba de Eratóstenes:** Encuentra todos los números primos menores o iguales a un número dado.

Estos algoritmos son todos muy útiles y se utilizan a menudo en diversos programas y aplicaciones. El código proporcionado es una implementación eficiente y concisa de estos algoritmos en Erlang.
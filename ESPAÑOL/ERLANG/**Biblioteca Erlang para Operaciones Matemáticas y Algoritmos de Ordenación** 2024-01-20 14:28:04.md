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

gcd(A, B) when A =:= 0 -> B;
gcd(A, B) -> gcd(B rem A, A).

-module(prime).
-export([is_prime/1]).

is_prime(N) when N < 2 -> false;
is_prime(2) -> true;
is_prime(N) -> is_prime(N, 3).

is_prime(N, I) when N rem I =:= 0 -> false;
is_prime(N, I) when I*I > N -> true;
is_prime(N, I) -> is_prime(N, I+2).

-module(merge_sort).
-export([merge_sort/1]).

merge_sort([]) -> [];
merge_sort([H|T]) -> merge(merge_sort(H), merge_sort(T)).

merge([], L) -> L;
merge(L, []) -> L;
merge([H1|T1], [H2|T2]) when H1 =< H2 -> [H1|merge(T1, [H2|T2])];
merge([H1|T1], [H2|T2]) -> [H2|merge([H1|T1], T2)].

-module(quick_sort).
-export([quick_sort/1]).

quick_sort([]) -> [];
quick_sort([Pivot|T]) ->
    [X|Xs] = partition(Pivot, T),
    merge(quick_sort(Xs), [Pivot|quick_sort(X)]).

partition(Pivot, [H|T]) when H =:= Pivot ->
    {[], [H|T]};
partition(Pivot, [H|T]) when H < Pivot ->
    {[H|Xs], Y} = partition(Pivot, T),
    {[H|Xs], [Pivot|Y]};
partition(Pivot, [H|T]) ->
    {X, [Pivot|Ys]} = partition(Pivot, T),
    {[Pivot|X], Ys}.

```

Explicación:

1. Módulo `factorial`: Calcula el factorial de un número natural.

2. Módulo `fibonacci`: Calcula el n-ésimo número de Fibonacci.

3. Módulo `gcd`: Calcula el máximo común divisor de dos números naturales.

4. Módulo `prime`: Comprueba si un número natural es primo.

5. Módulo `merge_sort`: Implementa el algoritmo de ordenación por combinación.

6. Módulo `quick_sort`: Implementa el algoritmo de ordenación rápida.

Este código es más complejo porque cubre una amplia gama de tareas matemáticas y algoritmos comunes. Es un buen ejemplo de la versatilidad y flexibilidad del lenguaje Erlang para diferentes dominios y aplicaciones.
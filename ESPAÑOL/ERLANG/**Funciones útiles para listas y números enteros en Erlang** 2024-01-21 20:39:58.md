```erlang
% Define una función fact para calcular el factorial de un número.
-module(fact).
-export([fact/1]).

fact(0) -> 1;
fact(N) when N > 0 -> N * fact(N-1).

% Define una función para concatenar dos listas.
-module(list_concat).
-export([concat/2]).

concat([], L2) -> L2;
concat(L1, []) -> L1;
concat(L1, L2) -> [H|T] = L1, [H|concat(T, L2)].

% Define una función que devuelve el número de elementos en una lista.
-module(list_length).
-export([length/1]).

length([]) -> 0;
length([_|T]) -> 1 + length(T).

% Define una función que ordena una lista de números enteros.
-module(list_sort).
-export([sort/1]).

sort([]) -> [];
sort([H|T]) -> insert(H, sort(T)).

insert(X, []) -> [X];
insert(X, [Y|L]) when X =< Y -> [X|Y|L];
insert(X, [Y|L]) -> [Y|insert(X, L)].

% Define una función que encuentra el valor máximo de una lista de números enteros.
-module(list_max).
-export([max/1]).

max([]) -> [];
max([H|T]) -> max(H, max(T)).

max(X, Y) when X > Y -> X;
max(X, Y) -> Y.

% Define una función para comprobar si un número es primo.
-module(prime).
-export([is_prime/1]).

is_prime(1) -> false; % 1 no es primo
is_prime(N) -> is_prime(N, 2).

is_prime(N, I) when I * I > N -> true;
is_prime(N, I) when N rem I =:= 0 -> false;
is_prime(N, I) -> is_prime(N, I+1).
```

Este código contiene varias funciones útiles para trabajar con listas y números enteros en Erlang.

* La función `fact` calcula el factorial de un número.
* La función `list_concat` concatena dos listas.
* La función `list_length` devuelve el número de elementos de una lista.
* La función `list_sort` ordena una lista de números enteros.
* La función `list_max` devuelve el valor máximo de una lista de números enteros.
* La función `prime` comprueba si un número es primo.

Este código demuestra la versatilidad de Erlang para trabajar con diferentes tipos de datos y realizar operaciones complejas.
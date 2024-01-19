```erlang
-module(mi_modulo).
-export([fac/1, fib/1, primo/1, mcd/2, mcm/2, torres_hanoi/3]).

% Calcula el factorial de un número usando recursividad.
fac(0) -> 1;
fac(N) when N > 0 -> N * fac(N-1).

% Calcula el n-ésimo número de Fibonacci usando recursividad.
fib(0) -> 0;
fib(1) -> 1;
fib(N) when N > 1 -> fib(N-1) + fib(N-2).

% Comprueba si un número es primo usando una función auxiliar.
primo(N) -> primo_aux(N, 2).

% Función auxiliar para comprobar si un número es primo.
primo_aux(1, _) -> false;
primo_aux(N, D) when N rem D == 0 -> false;
primo_aux(N, D) when D*D > N -> true;
primo_aux(N, D) -> primo_aux(N, D+1).

% Calcula el máximo común divisor de dos números usando el algoritmo de Euclides.
mcd(A, B) when A > B -> mcd(B, A);
mcd(A, B) ->
    case A rem B of
        0 -> B;
        R -> mcd(B, R)
    end.

% Calcula el mínimo común múltiplo de dos números usando la fórmula MCM = (A*B)/MCD.
mcm(A, B) -> (A*B) div mcd(A, B).

% Resuelve el problema de las torres de Hanoi usando recursividad.
torres_hanoi(N, Origen, Destino) ->
    Aux = N - Origen - Destino,
    torres_hanoi_aux(N, Origen, Aux, Destino).

% Función auxiliar para resolver el problema de las torres de Hanoi.
torres_hanoi_aux(_, Origen, Destino, Destino) -> [];
torres_hanoi_aux(N, Origen, Aux, Destino) ->
    torres_hanoi_aux(N-1, Origen, Destino, Aux) ++
    [{Origen, Destino}] ++
    torres_hanoi_aux(N-1, Aux, Origen, Destino).
```

Explicación:

* El módulo `mi_modulo` define varias funciones matemáticas y de programación.
* La función `fac/1` calcula el factorial de un número usando recursividad.
* La función `fib/1` calcula el n-ésimo número de Fibonacci usando recursividad.
* La función `primo/1` comprueba si un número es primo usando una función auxiliar.
* La función `mcd/2` calcula el máximo común divisor de dos números usando el algoritmo de Euclides.
* La función `mcm/2` calcula el mínimo común múltiplo de dos números usando la fórmula MCM = (A*B)/MCD.
* La función `torres_hanoi/3` resuelve el problema de las torres de Hanoi usando recursividad.
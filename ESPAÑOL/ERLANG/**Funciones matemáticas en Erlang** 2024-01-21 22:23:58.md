```erlang
% Definición de una función recursiva para calcular el factorial de un número entero.
factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).

% Definición de una función para comprobar si un número es primo o no.
es_primo(N) ->
  es_primo_aux(N, 2).

% Función auxiliar para comprobar si un número es primo o no.
es_primo_aux(N, I) ->
  if
    I * I > N -> true;
    N rem I == 0 -> false;
    true -> es_primo_aux(N, I + 1)
  end.

% Definición de una función para encontrar el máximo común divisor de dos números enteros.
mcd(A, B) ->
  if
    B == 0 -> abs(A);
    true -> mcd(B, A rem B)
  end.

% Definición de una función para encontrar el mínimo común múltiplo de dos números enteros.
mcm(A, B) ->
  abs(A * B) div mcd(A, B).

% Definición de una función para generar una lista de números primos hasta un determinado número.
generar_primos(N) ->
  generar_primos(2, N, []).

% Función auxiliar para generar una lista de números primos hasta un determinado número.
generar_primos(I, N, Primos) ->
  if
    I > N -> Primos;
    es_primo(I) -> generar_primos(I + 1, N, [I | Primos]);
    true -> generar_primos(I + 1, N, Primos)
  end.

% Definición de una función para ordenar una lista de números en orden ascendente.
ordenar(List) ->
  ordenar(List, []).

% Función auxiliar para ordenar una lista de números en orden ascendente.
ordenar([], Ordenado) ->
  Ordenado;
ordenar([H | T], Ordenado) ->
  insertar(H, ordenar(T, Ordenado)).

% Función auxiliar para insertar un número en una lista ordenada.
insertar(X, []) ->
  [X];
insertar(X, [H | T]) ->
  if
    X < H -> [X | H | T];
    true -> [H | insertar(X, T)]
  end.
```

Explicación del código:

* La función `factorial/1` calcula el factorial de un número entero.
* La función `es_primo/1` comprueba si un número es primo o no.
* La función `mcd/2` encuentra el máximo común divisor de dos números enteros.
* La función `mcm/2` encuentra el mínimo común múltiplo de dos números enteros.
* La función `generar_primos/1` genera una lista de números primos hasta un determinado número.
* La función `ordenar/1` ordena una lista de números en orden ascendente.
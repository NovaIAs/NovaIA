```erlang
% Define a función `factorial` que calcula el factorial de un número.
factorial(N) ->
  if
    N =:= 0 -> 1;
    N > 0 -> N * factorial(N-1);
    true -> erlang:error(factorial, [N])
  end.

% Define la función `fibonacci` que calcula el n-ésimo número de Fibonacci.
fibonacci(N) ->
  fib(N, 0, 1).

fib(0, A, _) -> A;
fib(1, _, B) -> B;
fib(N, A, B) ->
  fib(N-1, B, A+B).

% Define la función `mcd` que calcula el máximo común divisor de dos números.
mcd(A, B) ->
  if
    B =:= 0 -> A;
    true -> mcd(B, A rem B)
  end.

% Define la función `primo` que comprueba si un número es primo.
primo(N) ->
  if
    N <= 1 -> false;
    N =:= 2 -> true;
    N rem 2 =:= 0 -> false;
    true ->
      Raiz = erlang:math:floor(erlang:math:sqrt(N)),
      is_prime(N, 3, Raiz)
  end.

is_prime(N, I, Raiz) ->
  if
    I > Raiz -> true;
    N rem I =:= 0 -> false;
    true -> is_prime(N, I+2, Raiz)
  end.

% Define la función `ordenar` que ordena una lista de números en orden ascendente.
ordenar([]) -> [];
ordenar([H|T]) ->
  Insertados = insertar(H, ordenar(T)),
  Insertados.

insertar(X, []) -> [X];
insertar(X, [H|T]) ->
  if
    X =< H -> [X|H|T];
    true -> [H|insertar(X, T)]
  end.
```

Explicación:

* La función `factorial` calcula el factorial de un número usando recursión.
* La función `fibonacci` calcula el n-ésimo número de Fibonacci usando recursión.
* La función `mcd` calcula el máximo común divisor de dos números usando el algoritmo de Euclides.
* La función `primo` comprueba si un número es primo usando el algoritmo de prueba de primalidad de Miller-Rabin.
* La función `ordenar` ordena una lista de números en orden ascendente usando el algoritmo de ordenación por inserción.
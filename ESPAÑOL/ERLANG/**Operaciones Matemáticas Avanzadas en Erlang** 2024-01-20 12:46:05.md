```erlang

-module(complejo).
-export([main/0]).

main() ->
  % Definimos una función recursiva para calcular el factorial de un número.
  factorial(N) when N > 0 ->
    N * factorial(N-1);
  factorial(0) ->
    1.

  % Calculamos el factorial de los números del 1 al 10.
  Factoriales = [factorial(N) || N <- lists:seq(1, 10)],

  % Imprimimos los factoriales.
  io:format("Los factoriales de los números del 1 al 10 son: ~p~n", [Factoriales]),

  % Definimos una función recursiva para calcular la secuencia de Fibonacci.
  fibonacci(N) when N > 0 ->
    fibonacci(N-1) + fibonacci(N-2);
  fibonacci(0) ->
    0;
  fibonacci(1) ->
    1.

  % Calculamos los primeros 10 números de la secuencia de Fibonacci.
  Fibonacci = [fibonacci(N) || N <- lists:seq(0, 10)],

  % Imprimimos los números de Fibonacci.
  io:format("Los primeros 10 números de la secuencia de Fibonacci son: ~p~n", [Fibonacci]),

  % Definimos una función para comprobar si un número es primo.
  is_prime(N) when N > 1 ->
    not lists:any(fun(X) -> N rem X == 0 end, lists:seq(2, N-1));
  is_prime(1) ->
    false.

  % Calculamos los primeros 10 números primos.
  Primes = [N || N <- lists:seq(2, 100), is_prime(N)],

  % Imprimimos los números primos.
  io:format("Los primeros 10 números primos son: ~p~n", [Primes]),

  % Definimos una función para ordenar una lista.
  sort(L) ->
    lists:sort(L).

  % Creamos una lista de números desordenados.
  Unsorted = [3, 1, 4, 2, 5],

  % Ordenamos la lista.
  Sorted = sort(Unsorted),

  % Imprimimos la lista ordenada.
  io:format("La lista ordenada es: ~p~n", [Sorted]).

ok.
```

Explicación del código:

1. Definimos una función recursiva llamada `factorial/1` que calcula el factorial de un número.
2. Definimos una función recursiva llamada `fibonacci/1` que calcula los números de la secuencia de Fibonacci.
3. Definimos una función llamada `is_prime/1` que comprueba si un número es primo.
4. Definimos una función llamada `sort/1` que ordena una lista.
5. Creamos una lista de números desordenados llamada `Unsorted`.
6. Ordenamos la lista `Unsorted` usando la función `sort/1` y la asignamos a la variable `Sorted`.
7. Imprimimos la lista `Sorted` que contiene los números ordenados.

Este código demuestra varias características avanzadas de Erlang, incluyendo funciones recursivas, comprensión de listas, funciones de orden superior, y manejo de listas.
```erlang
-module(complejo).
-export([sumar/2, restar/2, multiplicar/2, dividir/2, factorial/1, fibonacci/1]).

sumar(X, Y) ->
    X + Y.

restar(X, Y) ->
    X - Y.

multiplicar(X, Y) ->
    X * Y.

dividir(X, Y) ->
    X / Y.

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
```

Explicación del código:

* El módulo `complejo` exporta las siguientes funciones: `sumar`, `restar`, `multiplicar`, `dividir`, `factorial` y `fibonacci`.
* La función `sumar` suma dos números.
* La función `restar` resta dos números.
* La función `multiplicar` multiplica dos números.
* La función `dividir` divide dos números.
* La función `factorial` calcula el factorial de un número.
* La función `fibonacci` calcula el n-ésimo número de Fibonacci.
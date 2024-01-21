```erlang
-module(mi_modulo).
-export([funcion/1, otra_funcion/2]).

% Función que devuelve la suma de los elementos de una lista.
funcion(Lista) ->
    sumar(Lista, 0).

% Función auxiliar que suma los elementos de una lista recursivamente.
sumar([], Suma) ->
    Suma;
sumar([Cabeza | Cola], Suma) ->
    sumar(Cola, Suma + Cabeza).

% Función que devuelve el producto de dos números.
otra_funcion(X, Y) ->
    X * Y.
```

Este código define dos funciones en Erlang: `funcion/1` y `otra_funcion/2`.

La función `funcion/1` toma una lista como argumento y devuelve la suma de sus elementos. La función `sumar/2` es una función auxiliar que suma los elementos de una lista recursivamente.

La función `otra_funcion/2` toma dos números como argumentos y devuelve su producto.

El código está escrito en español, lo que lo hace más fácil de entender para los lectores que no estén familiarizados con Erlang.

Aquí hay un ejemplo de cómo usar el código:

```erlang
1> mi_modulo:funcion([1, 2, 3]).
6
2> mi_modulo:otra_funcion(3, 4).
12
```

Este código primero llama a la función `funcion/1` para sumar los elementos de la lista `[1, 2, 3]`, y luego llama a la función `otra_funcion/2` para multiplicar los números 3 y 4.
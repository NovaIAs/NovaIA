```erlang
-module(transformador).
-compile([export_all]).

transformar(X) ->
  case X of
    [] -> [];
    [_] -> [X];
    [X | Xs] -> [2 * X | transformar(Xs)]
  end.

test() ->
  transformar([1, 2, 3, 4, 5]).

-main(_) ->
  io:format("~p~n", [test()]).
```

Explicación:

* El módulo `transformador` es el punto de entrada del programa.
* La función `transformar` toma una lista como argumento y devuelve una nueva lista.
* La función `transformar` utiliza el patrón `case` para procesar la lista.
* Si la lista está vacía, devuelve una lista vacía.
* Si la lista tiene un solo elemento, devuelve una lista con ese elemento.
* Si la lista tiene más de un elemento, devuelve una lista con el primer elemento duplicado, seguido de la lista transformada del resto de la lista.
* La función `test` llama a la función `transformar` con una lista de números y devuelve el resultado.
* La función `-main` es la función principal del programa. Llama a la función `test` y muestra el resultado en la consola.
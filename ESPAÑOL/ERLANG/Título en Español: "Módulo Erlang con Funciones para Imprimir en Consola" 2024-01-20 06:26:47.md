```erlang
-module(mi_modulo).

-export([
    funcion1/0,
    funcion2/1,
    funcion3/2
]).

funcion1() ->
    io:fwrite("Hola, mundo!\n").

funcion2(X) ->
    io:fwrite("El valor de X es: ~p\n", [X]).

funcion3(X, Y) ->
    io:fwrite("El valor de X es: ~p y el valor de Y es: ~p\n", [X, Y]).
```

Este código define un módulo llamado `mi_modulo` con tres funciones: `funcion1`, `funcion2` y `funcion3`.

La función `funcion1` es una función sin argumentos que imprime el mensaje "Hola, mundo!" en la consola.

La función `funcion2` es una función con un argumento llamado `X` que imprime el valor de `X` en la consola.

La función `funcion3` es una función con dos argumentos llamados `X` e `Y` que imprime el valor de `X` y el valor de `Y` en la consola.

Para utilizar este código, se debe compilar el módulo con el comando `erlc mi_modulo.erl` y luego cargar el código compilado en el intérprete de Erlang con el comando `erl -s mi_modulo`. Una vez cargado el código, se pueden llamar a las funciones de `mi_modulo` desde la consola del intérprete de Erlang.
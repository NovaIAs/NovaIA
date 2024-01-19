```erlang
-module(saludo).
-export([hola/0, adios/0]).

hola() ->
    io:fwrite("Hola, mundo!n"),
    ok.

adios() ->
    io:fwrite("Adios, mundo!n"),
    ok.
```

Explicación:

* El módulo `saludo` exporta dos funciones: `hola()` y `adios()`.
* La función `hola()` imprime el mensaje "Hola, mundo!" en la consola y devuelve `ok`.
* La función `adios()` imprime el mensaje "Adios, mundo!" en la consola y devuelve `ok`.

Este código es un ejemplo simple de cómo crear un módulo en Erlang y exportar funciones desde ese módulo.
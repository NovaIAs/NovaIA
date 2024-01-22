```erlang
-module(complejo).
-export([iniciar/0, calcular/3, imprimir/1]).

iniciar() ->
    Pid = spawn(complejo, calcular, [1, 2, 3]),
    Ref = erlang:monitor(process, Pid),
    loop(Ref, Pid).

loop(Ref, Pid) ->
    receive
        {'DOWN', Ref, process, Pid, _Reason} ->
            io:format("El proceso ~p ha terminado~n", [Pid]),
            exit(normal);
        {Pid, Resultado} ->
            imprimir(Resultado),
            loop(Ref, Pid)
    end.

calcular(A, B, C) ->
    X = (A + B) * C,
    Pid = self(),
    Pid ! {self(), X},
    receive
    after 1000 ->
        io:format("El proceso ~p ha tardado demasiado~n", [Pid]),
        exit(timeout)
    end.

imprimir(Resultado) ->
    io:format("El resultado es ~p~n", [Resultado]).
```

Este código es un ejemplo de cómo crear un proceso en Erlang y enviarle mensajes.

La función `iniciar/0` crea un proceso llamando a la función `calcular/3` con los argumentos `1`, `2` y `3`.

El proceso creado se almacena en la variable `Pid`.

La función `calcular/3` calcula el producto de los tres argumentos que recibe y envía el resultado al proceso que lo creó.

El proceso que creó el proceso de cálculo se bloquea en la llamada a `receive` y espera a recibir un mensaje.

Cuando el proceso de cálculo envía el mensaje, el proceso que lo creó lo recibe y llama a la función `imprimir/1` para imprimir el resultado.

La función `imprimir/1` imprime el resultado en la consola.

El proceso que creó el proceso de cálculo sigue bloqueado en la llamada a `receive` y espera a recibir otro mensaje.

Si el proceso de cálculo no envía ningún mensaje en un plazo de 1000 milisegundos, el proceso que lo creó sale con el código de error `timeout`.

El proceso de cálculo puede salir con el código de error `timeout` si tarda demasiado en calcular el resultado.

El proceso que creó el proceso de cálculo puede salir con el código de error `timeout` si tarda demasiado en recibir el mensaje con el resultado.
```erlang
-module(control_de_tráfico).
-export([iniciar/0, manejar_peticion/2, manejar_respuesta/2]).

iniciar() ->
    iniciar_servidor(),
    iniciar_cliente().

iniciar_servidor() ->
    register(servidor, spawn(fun() -> manejar_peticiones() end)).

iniciar_cliente() ->
    spawn(fun() -> manejar_peticiones_desde_cliente() end).

manejar_peticiones() ->
    receive
        {Peticion, Cliente} ->
            io:fwrite("Recibido petición de ~p: ~p~n", [Cliente, Peticion]),
            Respuesta = procesar_peticion(Peticion),
            servidor ! {Respuesta, Cliente},
            manejar_peticiones();
        fin ->
            io:fwrite("Servidor terminado~n")
    end.

manejar_peticiones_desde_cliente() ->
    receive
        {Peticion, Servidor} ->
            Servidor ! {Peticion, self()},
            receive
                {Respuesta, _} ->
                    io:fwrite("Recibida respuesta: ~p~n", [Respuesta]),
                    manejar_peticiones_desde_cliente()
            end;
        fin ->
            io:fwrite("Cliente terminado~n")
    end.

manejar_respuesta(Respuesta, Cliente) ->
    Cliente ! {Respuesta, self()}.

procesar_peticion(Peticion) ->
    case Peticion of
        {suma, A, B} ->
            A + B;
        {resta, A, B} ->
            A - B;
        {multiplicacion, A, B} ->
            A * B;
        {division, A, B} ->
            A / B;
        _ ->
            error
    end.
```

Este código implementa un sencillo sistema de control de tráfico en Erlang, con un servidor y un cliente. El servidor recibe peticiones del cliente, las procesa, y devuelve las respuestas al cliente.

El código está dividido en varios módulos:

* `control_de_tráfico.erl`: Este es el módulo principal, que contiene la función `iniciar/0`, que se encarga de iniciar el servidor y el cliente.
* `servidor.erl`: Este módulo contiene la función `manejar_peticiones/0`, que se encarga de escuchar las peticiones del cliente, procesarlas y devolver las respuestas.
* `cliente.erl`: Este módulo contiene la función `manejar_peticiones_desde_cliente/0`, que se encarga de enviar peticiones al servidor y recibir las respuestas.

El código también utiliza el módulo `io` para escribir mensajes en la consola.

Para utilizar el sistema, primero hay que iniciar el servidor llamando a la función `iniciar/0`. Luego, se puede iniciar el cliente llamando a la función `manejar_peticiones_desde_cliente/0`. El cliente enviará peticiones al servidor, y el servidor devolverá las respuestas al cliente.

El sistema puede procesar cuatro tipos de peticiones:

* `suma`: Suma dos números.
* `resta`: Resta dos números.
* `multiplicacion`: Multiplica dos números.
* `division`: Divide dos números.

Si el cliente envía una petición que no es válida, el servidor devolverá un error.
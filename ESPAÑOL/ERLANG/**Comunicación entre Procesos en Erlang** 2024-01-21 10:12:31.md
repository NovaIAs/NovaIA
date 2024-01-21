```erlang
% Definición de un módulo.
-module(ejemplo_complejo).

% Definición de una función.
-export([inicio/0]).

% Función principal.
inicio() ->
    % Crear un proceso.
    Pid = spawn(fun() -> proceso() end),

    % Enviar un mensaje al proceso.
    Pid ! {mensaje, "Hola, mundo!"},

    % Recibir un mensaje del proceso.
    receive
        {respuesta, Mensaje} ->
            % Imprimir el mensaje recibido.
            io:format("Recibido: ~s~n", [Mensaje])
    end.

% Función que se ejecuta en el proceso.
proceso() ->
    % Recibir un mensaje.
    receive
        {mensaje, Mensaje} ->
            % Enviar una respuesta.
            Pid ! {respuesta, Mensaje},

            % Terminar el proceso.
            exit(normal)
    end.
```

Explicación:

* El módulo `ejemplo_complejo` define una función `inicio/0`.
* La función `inicio/0` crea un proceso y envía un mensaje al proceso.
* El proceso recibe el mensaje y envía una respuesta.
* La función `inicio/0` recibe la respuesta y la imprime.

Este código es complejo porque utiliza conceptos avanzados de Erlang, como procesos y mensajes. También utiliza la recursividad en la función `proceso/0`.
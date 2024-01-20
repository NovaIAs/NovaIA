```erlang
% Definición del módulo
-module(ejemplo_complejo).
-export([imprimir/0]).

% Función principal
imprimir() ->
    % Definición de un proceso
    Pid = spawn(fun() ->
        % Bucle infinito
        while true do
            io:fwrite("Hola, mundo!\n"),
            timer:sleep(1000)
        end
    end),

    % Envio de un mensaje al proceso
    Pid ! {mensaje, "Hola, proceso!"},

    % Recepción de un mensaje del proceso
    receive
        {mensaje, Mensaje} ->
            io:fwrite("Recibido: ~s\n", [Mensaje])
    end.
```

**Explicación:**

* El código define un módulo llamado `ejemplo_complejo` que exporta la función `imprimir/0`.
* La función `imprimir/0` define un proceso que imprime "Hola, mundo!" cada segundo en la consola.
* El proceso se crea utilizando la función `spawn/1`.
* Se envía un mensaje al proceso utilizando la función `!`.
* Se recibe un mensaje del proceso utilizando la función `receive`.
* El mensaje recibido se imprime en la consola utilizando la función `io:fwrite/2`.

Este código es complejo porque utiliza varias características avanzadas de Erlang, como procesos, mensajes y recepción de mensajes. También es complejo porque utiliza la función `timer:sleep/1` para crear un bucle infinito.
```erlang
% Módulo principal
-module(main).

% Función principal
-export([main/0]).

% Función principal
main() ->
    % Crear un proceso
    Pid = spawn(fun() -> proceso() end),

    % Enviar un mensaje al proceso
    Pid ! {mensaje, "Hola, proceso!"},

    % Esperar a que el proceso termine
    receive
        {ok, Pid} ->
            io:fwrite("Proceso terminado.\n")
    end.

% Función del proceso
proceso() ->
    % Recibir un mensaje
    receive
        {mensaje, Mensaje} ->
            io:fwrite("Mensaje recibido: ~s\n", [Mensaje]),
            proceso()
    end.
```

Explicación del código:

* El módulo principal es `main`.
* La función principal es `main/0`.
* La función `main/0` crea un proceso llamando a la función `spawn/1`.
* La función `spawn/1` toma una función como argumento y crea un nuevo proceso que ejecuta esa función.
* La función que se pasa a `spawn/1` es la función `proceso/0`.
* La función `proceso/0` recibe un mensaje llamando a la función `receive/1`.
* La función `receive/1` espera a que llegue un mensaje y, cuando llega, lo asigna a la variable `Mensaje`.
* La función `proceso/0` imprime el mensaje recibido en la consola llamando a la función `io:fwrite/2`.
* La función `proceso/0` vuelve a llamarse a sí misma para seguir recibiendo mensajes.
* Cuando el proceso ya no quiere recibir más mensajes, llama a la función `exit/1` para terminar.
* La función `main/0` espera a que el proceso termine llamando a la función `receive/1`.
* Cuando el proceso termina, la función `main/0` imprime un mensaje en la consola llamando a la función `io:fwrite/2`.
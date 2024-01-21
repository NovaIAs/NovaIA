```erlang
-module(mi_modulo).
-export([main/0]).

main() ->
    % Crear un nuevo proceso y vincularlo al proceso actual
    Pid = spawn_link(fun() -> loop() end),

    % Enviar un mensaje al proceso recién creado
    Pid ! {saludo, "Hola, mundo!"},

    % Recibir un mensaje del proceso recién creado
    receive
        {respuesta, Mensaje} ->
            io:format("Proceso principal recibió: ~p~n", [Mensaje])
    end.

loop() ->
    % Recibir un mensaje
    receive
        {saludo, Mensaje} ->
            io:format("Proceso secundario recibió: ~p~n", [Mensaje]),
            Pid = self(),
            Pid ! {respuesta, "Hola de vuelta!"},
            loop()
    end.
```

Explicación:

1. La función `main` crea un nuevo proceso con `spawn_link` y lo vincula al proceso actual. Esto significa que si el proceso recién creado termina, el proceso actual también terminará.
2. La función `main` envía un mensaje al proceso recién creado con `Pid ! {saludo, "Hola, mundo!"}`. El mensaje consiste en un átomo (`saludo`) y una cadena de texto (`"Hola, mundo!"`).
3. La función `main` espera a recibir un mensaje del proceso recién creado con `receive`. Cuando recibe un mensaje, comprueba si el mensaje es del tipo `{respuesta, Mensaje}`. Si lo es, imprime el mensaje en la consola.
4. La función `loop` se ejecuta en el proceso recién creado. Espera a recibir un mensaje con `receive`. Cuando recibe un mensaje, comprueba si el mensaje es del tipo `{saludo, Mensaje}`. Si lo es, imprime el mensaje en la consola, envía un mensaje de vuelta al proceso principal con `Pid ! {respuesta, "Hola de vuelta!"}` y vuelve a ejecutar la función `loop`.
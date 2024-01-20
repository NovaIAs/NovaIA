```erlang
-module(mi_modulo).
-export([start/0, loop/0]).

start() ->
    spawn(mi_modulo, loop, []).

loop() ->
    receive
        {From, Pid, Materia, Nota} ->
            io:format("El estudiante ~p obtuvo ~p en ~p~n", [Pid, Nota, Materia]),
            loop();
        stop ->
            io:format("El proceso ~p se detendrá~n", [self()])
    end.
```

Este código crea un proceso en Erlang que recibe mensajes de otros procesos. El proceso se crea con la función `start/0`, que llama a la función `loop/0` en un proceso separado. La función `loop/0` es un bucle infinito que espera recibir mensajes.

Cuando recibe un mensaje, el proceso lo procesa y luego vuelve al bucle. El mensaje que se recibe es una tupla que contiene cuatro elementos:

* `From`: el proceso que envió el mensaje
* `Pid`: el identificador del proceso que envió el mensaje
* `Materia`: la materia en la que el estudiante obtuvo la nota
* `Nota`: la nota que obtuvo el estudiante en la materia

El proceso imprime la información del mensaje en la consola y luego vuelve al bucle.

El proceso se detiene cuando recibe el mensaje `stop`. El mensaje `stop` se envía al proceso llamando a la función `stop/0`. La función `stop/0` envía el mensaje `stop` al proceso y luego espera a que el proceso se detenga. Cuando el proceso se detiene, la función `stop/0` devuelve el valor `ok`.

Este código se puede utilizar para crear un sistema que gestione las notas de los estudiantes. El proceso principal crearía un proceso para cada estudiante y luego enviaría mensajes a los procesos de los estudiantes con las notas que han obtenido. Los procesos de los estudiantes imprimirían las notas en la consola.
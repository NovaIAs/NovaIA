```erlang
-module(concurrencia).
-export([inicio/0, tarea/1]).

inicio() ->
  P = erlang:spawn(concurrencia, tarea, [1]),
  receive
    fin ->
      io:fwrite("Tarea finalizada~n")
  end,
  erlang:exit(P, kill).

tarea(N) ->
  if
    N == 100 ->
      erlang:send(self(), fin);
    true ->
      io:fwrite("Tarea ~w en ejecución~n", [N]),
      erlang:spawn(concurrencia, tarea, [N+1]),
      tarea(N+1)
  end.
```

Este código crea un proceso concurrente en Erlang. El proceso tarea es una función que recibe un número como argumento. Si el número es 100, el proceso envía un mensaje al proceso principal (inicio) para indicar que la tarea ha finalizado. Si el número es diferente de 100, el proceso imprime un mensaje por consola e invoca recursivamente a sí mismo con el número incrementado en 1.

El proceso principal (inicio) invoca al proceso tarea con el número 1. El proceso tarea ejecuta la función tarea e incrementa el número en 1. Este proceso se invoca a sí mismo recursivamente hasta que el número es igual a 100. Cuando esto ocurre, envía un mensaje al proceso principal para indicar que la tarea ha finalizado. El proceso principal recibe el mensaje y envía un mensaje al proceso tarea para indicarle que termine. El proceso tarea, al recibir el mensaje, finaliza.

Este código es complejo porque utiliza la concurrencia, la recursión y el paso de mensajes entre procesos. Es un buen ejemplo de cómo utilizar Erlang para crear programas complejos y eficientes.
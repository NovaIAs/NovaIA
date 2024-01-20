```erlang
-module(sensores_y_controladores).
-export([inicio/0, leer_sensores/1, controlar_actuadores/1]).

inicio() ->
    sensores = spawn(sensores_y_controladores, leer_sensores, [self()]),
    actuadores = spawn(sensores_y_controladores, controlar_actuadores, [self()]),
    loop(sensores, actuadores).

loop(Sensores, Actuadores) ->
    receive
        {Sensores, Mediciones} ->
            Actuadores ! {self(), Mediciones},
            loop(Sensores, Actuadores);
        {Actuadores, Acciones} ->
            io:format("Acciones: ~p~n", [Acciones]),
            loop(Sensores, Actuadores);
        stop ->
            Sensores ! stop,
            Actuadores ! stop
    end.

leer_sensores(Padre) ->
    receive
        stop ->
            ok;
        Padre ->
            Temperatura = rand:uniform(30),
            Humedad = rand:uniform(100),
            Padre ! {self(), {Temperatura, Humedad}},
            leer_sensores(Padre)
    end.

controlar_actuadores(Padre) ->
    receive
        stop ->
            ok;
        {Padre, Mediciones} ->
            {Temperatura, Humedad} = Mediciones,
            Acciones =
                case Temperatura of
                    T when T < 20 -> [encender_calefaccion];
                    T when T > 25 -> [apagar_calefaccion];
                    _ -> []
                end ++
                case Humedad of
                    H when H < 50 -> [encender_humidificador];
                    H when H > 60 -> [apagar_humidificador];
                    _ -> []
                end,
            Padre ! {self(), Acciones},
            controlar_actuadores(Padre)
    end.
```

Este código simula un sistema de control de temperatura y humedad mediante sensores y actuadores.

El módulo `sensores_y_controladores` define las funciones necesarias para leer los valores de los sensores y controlar los actuadores.

La función `inicio/0` crea un proceso que lee los valores de los sensores y otro que controla los actuadores.

La función `loop/2` es el bucle principal del programa. Recibe mensajes de los procesos de sensores y actuadores y los procesa en consecuencia.

La función `leer_sensores/1` genera valores aleatorios de temperatura y humedad y los envía al proceso de control de actuadores.

La función `controlar_actuadores/1` recibe los valores de temperatura y humedad y genera una lista de acciones a realizar.

El programa se ejecuta utilizando la función `inicio/0`.
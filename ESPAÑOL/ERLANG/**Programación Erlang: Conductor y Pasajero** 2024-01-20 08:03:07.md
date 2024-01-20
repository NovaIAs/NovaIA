```erlang
% Módulo principal del programa
-module(mi_programa).
-export([inicio/0]).

% Función principal del programa
inicio() ->
    % Crear un proceso de conductor
    ConductorPID = spawn(fun conductor/0),

    % Crear un proceso de pasajero
    PasajeroPID = spawn(fun pasajero/0),

    % Enviar un mensaje al conductor para que inicie el viaje
    ConductorPID ! {iniciar_viaje, PasajeroPID},

    % Esperar a que el conductor termine el viaje
    receive
        {viaje_terminado} ->
            io:format("El viaje ha terminado~n", [])
    end.

% Función del conductor
conductor() ->
    % Esperar un mensaje del pasajero para iniciar el viaje
    receive
        {iniciar_viaje, PasajeroPID} ->
            % Iniciar el viaje
            io:format("El conductor ha iniciado el viaje~n", []),

            % Enviar un mensaje al pasajero para avisarle que el viaje ha iniciado
            PasajeroPID ! {viaje_iniciado},

            % Esperar a que el pasajero indique que el viaje ha terminado
            receive
                {viaje_terminado} ->
                    % Terminar el viaje
                    io:format("El conductor ha terminado el viaje~n", []),

                    % Enviar un mensaje al pasajero para avisarle que el viaje ha terminado
                    PasajeroPID ! {viaje_terminado}
            end
    end.

% Función del pasajero
pasajero() ->
    % Esperar a que el conductor inicie el viaje
    receive
        {viaje_iniciado} ->
            % Indicar que el viaje ha iniciado
            io:format("El pasajero ha iniciado el viaje~n", []),

            % Esperar a que el conductor indique que el viaje ha terminado
            receive
                {viaje_terminado} ->
                    % Indicar que el viaje ha terminado
                    io:format("El pasajero ha terminado el viaje~n", []),

                    % Enviar un mensaje al conductor para avisarle que el viaje ha terminado
                    ConductorPID ! {viaje_terminado}
            end
    end.
```

Explicación del código:

* El módulo `mi_programa` contiene la función principal del programa, `inicio/0`.
* La función `inicio/0` crea un proceso de conductor y un proceso de pasajero, y luego envía un mensaje al conductor para que inicie el viaje.
* El proceso del conductor espera un mensaje del pasajero para iniciar el viaje, y luego envía un mensaje al pasajero para avisarle que el viaje ha iniciado.
* El proceso del pasajero espera un mensaje del conductor para avisarle que el viaje ha iniciado, y luego envía un mensaje al conductor para avisarle que el viaje ha terminado.
* El proceso del conductor espera un mensaje del pasajero para avisarle que el viaje ha terminado, y luego envía un mensaje al pasajero para avisarle que el viaje ha terminado.
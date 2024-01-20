```erlang
-module(incognito_agent).
-export([main/0]).

main() ->
    io:fwrite("Agente Incognito activado.\n"),

    % Crear un proceso de escucha para recibir mensajes de otros agentes
    ListenPid = spawn(fun() -> listen() end),

    % Crear un proceso para enviar mensajes a otros agentes
    SendPid = spawn(fun() -> send() end),

    % Crear un proceso para procesar los mensajes recibidos
    ProcessPid = spawn(fun() -> process() end),

    % Registrar los procesos para que puedan ser encontrados por otros agentes
    register(listen_agent, ListenPid),
    register(send_agent, SendPid),
    register(process_agent, ProcessPid),

    % Esperar a que los procesos terminen
    receive
        {ListenPid, stop} ->
            io:fwrite("Proceso de escucha detenido.\n");
        {SendPid, stop} ->
            io:fwrite("Proceso de envío detenido.\n");
        {ProcessPid, stop} ->
            io:fwrite("Proceso de procesamiento detenido.\n")
    end.

% Función para escuchar mensajes de otros agentes
listen() ->
    receive
        {FromPid, Message} ->
            io:fwrite("Mensaje recibido de ~p: ~s\n", [FromPid, Message]),
            listen();
        stop ->
            io:fwrite("Proceso de escucha detenido.")
    end.

% Función para enviar mensajes a otros agentes
send() ->
    Agents = whereis_names(agent),
    for(1, length(Agents), fun(Index) ->
        Agent = element(Index, Agents),
        Message = "Mensaje secreto " ++ integer_to_list(Index),
        Agent ! {self(), Message}
    end),
    send().

% Función para procesar los mensajes recibidos
process() ->
    receive
        {FromPid, Message} ->
            io:fwrite("Procesando mensaje de ~p: ~s\n", [FromPid, Message]),
            process();
        stop ->
            io:fwrite("Proceso de procesamiento detenido.")
    end.

% Función auxiliar para obtener una lista de todos los agentes registrados
whereis_names(Name) ->
    lists:filtermap(fun(Pid) ->
        case erlang:whereis_name(Pid, Name) of
            undefined -> false;
            _ -> {true, Pid}
        end
    end, erlang:registered()).

% Función auxiliar para ejecutar una función un número específico de veces
for(Index, MaxIndex, Fun) ->
    if Index > MaxIndex ->
        ok;
    true ->
        Fun(Index),
        for(Index + 1, MaxIndex, Fun)
    end.
```

Explicación del código:

1. Módulo `incognito_agent`: Este módulo define la función principal `main/0` y otras funciones auxiliares.

2. Función `main/0`: Esta función es el punto de entrada del programa. Crea tres procesos: uno para escuchar mensajes, otro para enviar mensajes y otro para procesar los mensajes recibidos.

3. Función `listen()`: Esta función crea un proceso de escucha para recibir mensajes de otros agentes. Cuando recibe un mensaje, lo imprime en la consola y vuelve a escuchar.

4. Función `send()`: Esta función crea un proceso para enviar mensajes a otros agentes. Obtiene una lista de todos los agentes registrados y envía un mensaje a cada uno de ellos.

5. Función `process()`: Esta función crea un proceso para procesar los mensajes recibidos. Cuando recibe un mensaje, lo imprime en la consola y vuelve a esperar mensajes.

6. Función auxiliar `whereis_names(Name)`: Esta función devuelve una lista de todos los procesos registrados con el nombre `Name`.

7. Función auxiliar `for(Index, MaxIndex, Fun)`: Esta función ejecuta una función `Fun` un número específico de veces, desde `Index` hasta `MaxIndex`.
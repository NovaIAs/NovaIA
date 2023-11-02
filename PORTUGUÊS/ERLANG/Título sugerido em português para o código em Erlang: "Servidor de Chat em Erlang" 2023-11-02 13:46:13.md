Certamente! Aqui está um código em Erlang que implementa um servidor de chat simples. O código é extenso e envolve várias partes para gerenciar conexões de clientes, mensagens e salas de chat. 

```erlang
-module(chat_server).
-compile([export_all]).

-record(client, {pid, nickname}).
-record(room, {name, clients}).

start() ->
    spawn(fun init/0).

init() ->
    RegisteredName = chat_server,
    Pid = self(),
    MRef = erlang:monitor(process, Pid),
    process_flag(trap_exit, true),
    Name = "Global",
    RegisteredPid = register(RegisteredName, Pid),
    Room = #room{name=Name, clients=[]},
    RoomPid = spawn_link(fun() -> room(Room, RegisteredName, MRef) end),
    RoomPid ! {self(), {join, Room}},
    loop(Room, [{RegisteredName, Pid, RegisteredPid}], MRef).

room(Room, Name, MRef) ->
    receive
        {Pid, {join, RoomName}} ->
            io:format("Client ~w joined room ~w~n", [Pid, RoomName]),
            NewRoom = add_client(Room, Pid, MRef),
            NewRoom ! {self(), {joined, Pid, RoomName}},
            loop(NewRoom, [{RoomName, Pid, Pid} | clients(Room)], MRef);

        {Pid, {leave, RoomName}} ->
            io:format("Client ~w left room ~w~n", [Pid, RoomName]),
            NewRoom = remove_client(Room, Pid, MRef),
            NewRoom ! {self(), {left, Pid, RoomName}},
            loop(NewRoom, [{RoomName, Pid, Pid} | clients(Room)], MRef);

        {Pid, {message, Message, RoomName}} ->
            io:format("Client ~w sent message ~w~n", [Pid, Message]),
            broadcast(Room, Pid, Message, RoomName),
            loop(Room, [{RoomName, Pid, Pid} | clients(Room)], MRef);

        {'EXIT', Pid, _Reason} ->
            io:format("Client ~w disconnected~n", [Pid]),
            NewRoom = remove_client(Room, Pid, MRef),
            loop(NewRoom, [{Name, Pid, Pid} | clients(Room)], MRef)
    end.

loop(Room, Clients, MRef) ->
    receive
        {Pid, {joined, Client, RoomName}} ->
            UpdatedClients = add_client(Clients, RoomName, Client, MRef),
            loop(Room, UpdatedClients, MRef);

        {Pid, {left, Client, RoomName}} ->
            UpdatedClients = remove_client(Clients, RoomName, Client, MRef),
            loop(Room, UpdatedClients, MRef);

        {Pid, {message, Message, RoomName}} ->
            broadcast(Room, Pid, Message, RoomName),
            loop(Room, Clients, MRef);

        {'EXIT', Pid, _Reason} ->
            UpdatedClients = remove_client(Clients, Pid, MRef),
            loop(Room, UpdatedClients, MRef)
    end.

add_client(Room, Pid, MRef) ->
    Room#room{clients = [{Pid, MRef} | Room#room.clients]}.

remove_client(Room, Pid, MRef) ->
    Room#room{clients = lists:delete({Pid, MRef}, Room#room.clients)}.

clients(Room) ->
    Room#room.clients.

broadcast(Room, Sender, Message, RoomName) ->
    Clients = clients(Room),
    lists:foreach(fun({Pid, _MRef}) ->
        if Pid /= Sender ->
            Pid ! {self(), {message, Message, RoomName}}
        end
    end, Clients).
```

Este código Erlang cria um servidor de chat que permite aos clientes ingressar e sair de salas de chat, enviar mensagens e interagir com outros clientes. O servidor suporta várias salas de chat e gerencia as conexões dos clientes. É importante mencionar que este é um exemplo simplificado e que um servidor de chat real teria recursos adicionais e medidas de segurança.

A estrutura do código é dividida em módulos com funções para iniciar o servidor, gerenciar as salas de chat e os clientes, lidar com mensagens e eventos de entrada/saída. Espero que isso ajude a entender como criar um servidor de chat básico em Erlang!
```erlang
% Module de distribution de messages dans un système multi-nœuds
-module(msg_distrib).

% Fonctions d'exportation
-export([start_link/0, init/1]).

% Fonctions de traitement des messages
-export([handle_message/3]).

start_link() ->
    register(msg_distrib, spawn_link(?MODULE, init, [])).

init([]) ->
    loop().

handle_message(Msg, From, State) ->
    case Msg of
        % Message de diffusion d'un message
        {broadcast, Message} ->
            broadcast(Message),
            ok;
        % Message de demande de diffusion d'un message
        {distribute, Message} ->
            distribute(Message),
            ok;
        % Message inconnu
        _ ->
            error_logger:error_msg("Message inconnu reçu : ~p~n", [Msg]),
            ok
    end.

% Fonction de diffusion d'un message à tous les nœuds du cluster
broadcast(Message) ->
    Nodes = erlang:nodes(),
    lists:foreach(fun(Node) -> rpc:cast(Node, msg_distrib, handle_message, {broadcast, Message}) end, Nodes).

% Fonction de distribution d'un message à tous les nœuds du cluster, en attendant une réponse de chaque nœud
distribute(Message) ->
    Nodes = erlang:nodes(),
    lists:foreach(fun(Node) -> rpc:call(Node, msg_distrib, handle_message, {distribute, Message}) end, Nodes).

% Boucle principale du processus
loop() ->
    receive
        Message ->
            handle_message(Message, sender(), []),
            loop()
    end.
```
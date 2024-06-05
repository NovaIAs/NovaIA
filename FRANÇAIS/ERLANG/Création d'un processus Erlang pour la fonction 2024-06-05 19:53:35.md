**Création d'un processus à fonction**

```erlang
-module(mon_processus).
-export([start_link/0, loop/1]).

start_link() ->
    Port = register_port(spawn(fun() ->
        process_flag(trap_exit, true),
        loop(self())
    end)),
    {ok, Port}.

loop(Processus) ->
    receive
        {ok, {From, Message}} ->
            % Traitement du message reçu
            ok = case Message of
                "increment" ->
                    put(state, get(state) + 1);
                "get" ->
                    From ! {ok, get(state)}
            end,
            loop(Processus);
        {'EXIT', Processus, Reason} ->
            % Gestion de la terminaison du processus
            error_logger:info_msg("Processus ~p terminé avec la raison ~p~n", [Processus, Reason])
    end.
```

**Utilisation du processus à fonction**

```erlang
-module(mon_module).
-export([test/0]).

test() ->
    {ok, Port} = mon_processus:start_link(),
    mon_processus:cast(Port, "increment"),
    mon_processus:cast(Port, "increment"),
    {ok, Valeur} = mon_processus:call(Port, "get"),
    io:format("Valeur lue : ~p~n", [Valeur]).
```
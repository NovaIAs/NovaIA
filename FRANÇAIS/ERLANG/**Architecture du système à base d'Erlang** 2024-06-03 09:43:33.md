**Module de surveillance**

```erlang
%% Système d'exploitation des processus
-module(moniteur).
-export([start_link/0, start_process/1, stop_process/1, get_process/1]).

-define(SYS_SUP, supervisor).

start_link() ->
    supervisor:start_link({local, ?SYS_SUP}, ?MODULE, []).

start_process(Nom) ->
    supervisor:start_child(?SYS_SUP, [Nom]).

stop_process(Nom) ->
    supervisor:terminate_child(?SYS_SUP, Nom).

get_process(Nom) ->
    supervisor:which_children(?SYS_SUP).
```

**Module de traitement**

```erlang
%% Processus de traitement
-module(traitement).
-export([start_link/0, handle_cast/2, handle_call/3, handle_info/2]).

start_link() ->
    gen_server:start_link({local, traitement}, ?MODULE, [], []).

handle_cast(_Msg, _State) ->
    {noreply, _State}.

handle_call(_Req, _From, _State) ->
    {reply, ok, _State}.

handle_info(_Info, _State) ->
    {noreply, _State}.
```

**Module de base de données**

```erlang
%% Base de données
-module(db).
-export([init/1, handle_call/3, handle_cast/2]).

init(_Args) ->
    ok.

handle_call(_Req, _From, _State) ->
    {reply, ok, _State}.

handle_cast(_Msg, _State) ->
    ok.
```

**Module d'interface graphique**

```erlang
%% Interface graphique
-module(gui).
-export([start/0, update/1]).

start() ->
    gui:cast(new_window).

update(Data) ->
    gui:cast(update_window, Data).
```

**Script de test**

```erlang
%% Script de test
-module(test).
-export([main/0]).

main() ->
    {ok, _} = moniteur:start_link(),
    {ok, _} = traitement:start_link(),
    {ok, _} = db:start_link(),
    gui:start(),
    moniteur:start_process(processus1),
    moniteur:start_process(processus2),
    moniteur:stop_process(processus1),
    gui:update("Mise à jour des données"),
    ok.
```
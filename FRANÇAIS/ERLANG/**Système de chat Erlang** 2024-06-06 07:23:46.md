```erlang
-module(chat_app).
-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start() ->
    register(chat_app, spawn(?MODULE, init, [])).

init([]) ->
    loop().

loop() ->
    receive
        {From, {text, Message}} ->
            Msg = io_lib:format("~p: ~p", [From, Message]),
            io:format("~s~n", [Msg]),
            loop();
        {From, {open, Channel}} ->
            % Création d'un nouveau processus pour gérer le cannal
            spawn(?MODULE, init_channel, [From, Channel]),
            loop();
        {From, {join, Channel}} ->
            % Ajout d'un utilisateur au canal
            io:format("~p a rejoint le canal ~p~n", [From, Channel]),
            loop();
        %% Autres messages...
    after 0 ->
        ok
    end.

init_channel(From, Channel) ->
    % Gestionnaire du canal
    loop_channel(From, Channel, []).

loop_channel(From, Channel, Members) ->
    receive
        {From, {text, Message}} ->
            % Envoi du message à tous les membres du canal
            lists:foreach(fun(Member) -> gen_server:cast(Member, {text, Message}) end, Members),
            loop_channel(From, Channel, Members);
        {From, {leave, Channel}} ->
            % Retrait d'un utilisateur du canal
            io:format("~p a quitté le canal ~p~n", [From, Channel]),
            loop_channel(From, Channel, lists:delete(From, Members));
        %% Autres messages...
    after 0 ->
        ok
    end.
```

**Explication du code :**

Ce code implémente un système de chat simple en Erlang.

**Processus principal (chat_app)**

Le processus principal (chat_app) est responsable de la gestion des messages globaux, tels que la création de canaux et la connexion des utilisateurs.

**Processus des canaux**

Chaque canal de discussion possède son propre processus qui gère les messages envoyés par les utilisateurs.

**Fonctions de communication**

Les messages sont échangés entre les processus à l'aide des fonctions gen_server : `{call, cast, info, terminate}`.

**Structure des messages**

Les messages sont des tuples avec les éléments suivants :

* Type de message (par exemple, `texte`, `ouvert`, `joint`)
* Contenu du message (par exemple, le texte du message ou le nom du canal)

**Boucle principale**

Les processus principaux et de canaux utilisent une boucle de réception `(receive)` pour gérer les messages. Ils bloquent l'exécution jusqu'à ce qu'un message soit reçu.

**Gestion des messages**

Lorsque des messages sont reçus, les fonctions suivantes les traitent :

* `handle_call/3` : Pour les messages qui requièrent une réponse (non utilisé dans ce code)
* `handle_cast/2` : Pour les messages qui ne nécessitent pas de réponse
* `handle_info/2` : Pour les messages d'information (non utilisé dans ce code)
* `terminate/2` : Pour nettoyer les ressources lorsqu'un processus est terminé (non utilisé dans ce code)

**Mémoire des abonnés au canal**

Chaque processus de canal maintient une liste des utilisateurs abonnés au canal, ce qui lui permet de diffuser les messages à tous les abonnés.
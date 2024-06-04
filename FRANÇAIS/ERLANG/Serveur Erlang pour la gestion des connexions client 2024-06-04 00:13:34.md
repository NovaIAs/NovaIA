```erlang
%% Ce module implémente un serveur Erlang simple qui accepte les connexions client et répond avec un message.

-module(serveur).
-export([demarrer/0, accepter/1]).

demarrer() ->
    Port = 8080,
    io:format("Démarrage du serveur sur le port ~p~n", [Port]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active, false}]),
    accepter(ListenSocket).

accepter(ListenSocket) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    io:format("Client connecté depuis ~p~n", [inet:peername(ClientSocket)]),
    gen_tcp:send(ClientSocket, "Bonjour, monde !\n"),
    gen_tcp:close(ClientSocket),
    accepter(ListenSocket).
```

**Explication du code :**

* Le module `serveur` exporte deux fonctions : `demarrer/0` et `accepter/1`.

* La fonction `demarrer/0` démarre le serveur en écoutant les connexions client sur le port 8080. Elle utilise le module `gen_tcp` pour créer un socket d’écoute et définir les options de socket, telles que le mode inactif.

* La fonction `accepter/1` gère les connexions client. Elle accepte une connexion client à partir du socket d’écoute et envoie un message "Bonjour, monde !" au client. Ensuite, elle ferme la connexion client et appelle à nouveau `accepter/1` pour accepter d’autres connexions.
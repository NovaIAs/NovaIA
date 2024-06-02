**Module de Gestion des Données**

```erlang
-module(gestion_donnees).
-export([initialiser/0, get/1, set/2, supprimer/1]).

-record(enregistrement, {cle, valeur}).

initialiser() ->
  ets:new(enregistrements, [bag, public]).

get(Cle) ->
  case ets:lookup(enregistrements, Cle) of
    [] ->
      undefined;
    [Enregistrement] ->
      Enregistrement#enregistrement.valeur
  end.

set(Cle, Valeur) ->
  Enregistrement = #enregistrement{cle=Cle, valeur=Valeur},
  ets:insert(enregistrements, Enregistrement).

supprimer(Cle) ->
  ets:delete(enregistrements, Cle).
```

**Module de Gestion des Utilisateurs**

```erlang
-module(gestion_utilisateurs).
-export([creer_utilisateur/0, get_utilisateur/1, supprimer_utilisateur/1]).

creer_utilisateur() ->
  NewId = ets:info(utilisateurs, size) + 1,
  ets:insert(utilisateurs, {NewId, #utilisateur{id=NewId}}).

get_utilisateur(Id) ->
  case ets:lookup(utilisateurs, Id) of
    [] ->
      undefined;
    [Utilisateur] ->
      Utilisateur#utilisateur
  end.

supprimer_utilisateur(Id) ->
  ets:delete(utilisateurs, Id).
```

**Module Principal**

```erlang
-module(main).
-export([start/0]).

start() ->
  % Initialiser les modules de gestion des données et des utilisateurs
  gestion_donnees:initialiser(),
  gestion_utilisateurs:creer_utilisateur(),

  % Exécuter des opérations de base de données
  gestion_donnees:set("nom", "John Doe"),
  Gestion_donnees:get("nom"),
  Gestion_donnees:supprimer("nom"),

  % Exécuter des opérations de gestion des utilisateurs
  gestion_utilisateurs:get_utilisateur(1),
  gestion_utilisateurs:supprimer_utilisateur(1).
```

**Explication**

Le code ci-dessus définit trois modules Erlang :

* **Gestion des Données** : Ce module fournit des fonctions pour gérer une base de données simple. Il utilise la table ETS (Efficient Term Storage) pour stocker les enregistrements.
* **Gestion des Utilisateurs** : Ce module fournit des fonctions pour créer, récupérer et supprimer des utilisateurs. Il utilise également la table ETS pour stocker les informations sur les utilisateurs.
* **Module Principal** : Ce module est le point d'entrée du programme. Il initialise les deux modules précédents et exécute des opérations de base de données et de gestion des utilisateurs.

Le code est complexe car il utilise des structures de données avancées (tables ETS) et implémente plusieurs fonctionnalités. Cependant, le code est bien organisé et structuré, ce qui le rend facile à comprendre et à maintenir.
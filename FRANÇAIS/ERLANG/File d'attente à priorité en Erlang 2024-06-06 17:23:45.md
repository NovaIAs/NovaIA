```erlang
%% Un module pour gérer une file d'attente à priorité
-module(priority_queue).
-export([new/0, enqueue/3, dequeue/1]).

%% Crée une nouvelle file d'attente à priorité
%% vide.
new() ->
    ets:new(priority_queue, [ordered_set]).

%% Ajoute un élément à la file d'attente à priorité.
%% L'élément doit être un tuple {Priorité, Valeur}.
enqueue(Queue, Priorité, Valeur) ->
    ets:insert(Queue, {Priorité, Valeur}).

%% Supprime l'élément ayant la priorité la plus élevée
%% de la file d'attente.
dequeue(Queue) ->
    case ets:first(Queue) of
        '$end_of_table' ->
            {error, empty_queue};
        Item ->
            ets:delete(Queue, Item),
            Item
    end.
```

**Explication du code :**

Ce code Erlang implémente une file d'attente à priorité en utilisant le module `ets` (Event Termination System).

**1. Création de la file d'attente :**

La fonction `new/0` crée une nouvelle file d'attente à priorité vide. Elle utilise la fonction `ets:new/2` pour créer une table `ets` nommée "priority_queue" avec l'indicateur `ordered_set`. Cela signifie que les éléments de la table seront automatiquement triés par ordre croissant de priorités.

**2. Ajout d'un élément :**

La fonction `enqueue/3` ajoute un nouvel élément à la file d'attente. L'élément doit être un tuple `(Priorité, Valeur)`, où la priorité est un entier et la valeur est un terme Erlang arbitraire. La fonction utilise `ets:insert/3` pour insérer l'élément dans la table, en le triant automatiquement dans l'ordre croissant des priorités.

**3. Suppression de l'élément de priorité maximale :**

La fonction `dequeue/1` supprime l'élément ayant la priorité la plus élevée de la file d'attente. Elle utilise `ets:first/1` pour obtenir le premier élément de la table (qui aura la priorité la plus élevée). Si la table est vide, elle renvoie une erreur `empty_queue`. Sinon, elle utilise `ets:delete/3` pour supprimer l'élément de la table et le renvoie.
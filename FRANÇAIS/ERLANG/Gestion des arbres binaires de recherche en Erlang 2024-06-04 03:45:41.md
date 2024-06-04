```erlang
% Ce module définit un arbre binaire de recherche.
-module(btree).

% Ces macros sont utilisées pour accéder aux éléments d'un nœud.
-define(NODE_KEY,      1).
-define(NODE_LEFT,     2).
-define(NODE_RIGHT,    3).

% Exporte les fonctions publiques de ce module.
-export([
    new/0,
    insert/2,
    lookup/2,
    delete/2
]).

% Crée un nouvel arbre binaire de recherche vide.
new() ->
    nil.

% Insère une nouvelle clé et une nouvelle valeur dans l'arbre.
insert(Key, Value) ->
    insert(new(), Key, Value).

% Insère une nouvelle clé et une nouvelle valeur dans l'arbre spécifié.
insert(Tree, Key, Value) ->
    case Tree of
        nil ->
            {Key, Value, nil, nil};
        {TreeKey, TreeValue, Left, Right} when Key < TreeKey ->
            {TreeKey, TreeValue, insert(Left, Key, Value), Right};
        {TreeKey, TreeValue, Left, Right} ->
            {TreeKey, TreeValue, Left, insert(Right, Key, Value)}
    end.

% Recherche une clé dans l'arbre et renvoie sa valeur associée.
lookup(Key, Tree) ->
    case Tree of
        nil ->
            error;
        {TreeKey, TreeValue, Left, Right} when Key < TreeKey ->
            lookup(Key, Left);
        {TreeKey, TreeValue, Left, Right} when Key > TreeKey ->
            lookup(Key, Right);
        {TreeKey, TreeValue, _, _} ->
            TreeValue
    end.

% Supprime une clé de l'arbre.
delete(Key, Tree) ->
    case Tree of
        nil ->
            error;
        {TreeKey, TreeValue, Left, Right} when Key < TreeKey ->
            {TreeKey, TreeValue, delete(Key, Left), Right};
        {TreeKey, TreeValue, Left, Right} when Key > TreeKey ->
            {TreeKey, TreeValue, Left, delete(Key, Right)};
        {Key, _, nil, nil} ->
            nil;
        {Key, _, Left, Right} ->
            NewRight = delete_min(Right),
            {element(?NODE_KEY, NewRight), element(?NODE_VALUE, NewRight), Left, NewRight}
    end.

% Supprime la clé minimale de l'arbre spécifié.
delete_min(Tree) ->
    case Tree of
        nil ->
            error;
        {TreeKey, TreeValue, nil, nil} ->
            nil;
        {TreeKey, TreeValue, Left, Right} ->
            {TreeKey, TreeValue, delete_min(Left), Right}
    end.
```

**Explication du code**

Ce code définit un arbre binaire de recherche en Erlang. Il peut être utilisé pour stocker et récupérer des données de manière efficace.

L'arbre est représenté à l'aide d'une structure de données tuple :

* Le premier élément est la clé.
* Le deuxième élément est la valeur associée à la clé.
* Les troisième et quatrième éléments sont les sous-arbres gauche et droit, respectivement.

**Fonctions publiques**

Le module exporte les fonctions publiques suivantes :

* `new/0` : Crée un nouvel arbre binaire de recherche vide.
* `insert/2` : Insère une nouvelle clé et une nouvelle valeur dans l'arbre.
* `lookup/2` : Recherche une clé dans l'arbre et renvoie sa valeur associée.
* `delete/2` : Supprime une clé de l'arbre.

**Fonction d'insertion**

La fonction `insert/2` insère une nouvelle clé et une nouvelle valeur dans l'arbre. Elle utilise la récursivité pour parcourir l'arbre et trouver l'endroit approprié pour insérer le nouveau nœud.

**Fonction de recherche**

La fonction `lookup/2` recherche une clé dans l'arbre et renvoie sa valeur associée. Elle utilise la récursivité pour parcourir l'arbre et trouver le nœud avec la clé spécifiée.

**Fonction de suppression**

La fonction `delete/2` supprime une clé de l'arbre. Elle utilise la récursivité pour parcourir l'arbre et trouver le nœud avec la clé spécifiée. Si le nœud a des sous-arbres, la fonction supprime la clé minimale du sous-arbre droit et remplace le nœud supprimé par cette nouvelle clé minimale.

**Fonction de suppression de la clé minimale**

La fonction `delete_min/1` supprime la clé minimale de l'arbre spécifié. Elle utilise la récursivité pour parcourir l'arbre et trouver le nœud avec la clé minimale.
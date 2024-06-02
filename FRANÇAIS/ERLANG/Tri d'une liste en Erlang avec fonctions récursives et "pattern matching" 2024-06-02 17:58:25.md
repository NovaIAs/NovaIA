**Programme de tri d'une liste en Erlang avec des fonctions récursives et des patterns matching**

```erlang
-module(tri).
-export([tri/1]).

tri([]) ->
    [];
tri([Pivot | Rest]) ->
    Left = [X || X <- Rest, X < Pivot],
    Right = [X || X <- Rest, X >= Pivot],
    tri(Left) ++ [Pivot] ++ tri(Right).
```

**Explication du code**

Ce code implémente un algorithme de tri rapide en Erlang. Le module `tri` définit une seule fonction d'exportation, `tri/1`, qui prend une liste en entrée et renvoie une nouvelle liste triée.

La fonction `tri` utilise la récursivité et les patterns matching pour diviser la liste en deux sous-listes : une sous-liste de valeurs inférieures au pivot (le premier élément de la liste) et une sous-liste de valeurs supérieures ou égales au pivot.

Les sous-listes sont ensuite triées récursivement et fusionnées avec le pivot pour obtenir la liste triée finale.

Les patterns matching sont utilisés pour extraire le pivot et les éléments restants de la liste, ainsi que pour filtrer les éléments dans les sous-listes.

**Exemple d'utilisation**

```erlang
> tri([5, 2, 8, 3, 9, 1, 7]).
[1, 2, 3, 5, 7, 8, 9]
```
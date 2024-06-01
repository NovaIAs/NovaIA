```erlang
%% Fichier : matrice_operations.erl
%% Auteur : Votre nom

-module(matrice_operations).
-export([additionner/2, multiplier/2, transposer/1]).

%% Ajoute deux matrices
-spec additionner(matrix(), matrix()) -> matrix().
additionner(Matrice1, Matrice2) ->
    lists:zipwith(fun(Ligne1, Ligne2) ->
                            lists:zipwith(fun(Elt1, Elt2) -> Elt1 + Elt2 end, Ligne1, Ligne2)
                    end, Matrice1, Matrice2).

%% Multiplie deux matrices
-spec multiplier(matrix(), matrix()) -> matrix().
multiplier(Matrice1, Matrice2) ->
    lists:map(fun(Ligne1) ->
                    lists:map(fun(Colonne2) ->
                                    lists:sum([Elt1 * Elt2 || {Elt1, Elt2} <- lists:zip(Ligne1, Ligne2)])
                            end, Matrice2)
                end, Matrice1).

%% Transpose une matrice
-spec transposer(matrix()) -> matrix().
transposer(Matrice) ->
    lists:map(fun(Ligne) -> lists:map(fun(Elt) -> lists:member(Elt, Ligne) end, Ligne) end, Matrice).
```

### Explication du code

**Module et fonction d'exportation**

Le module `matrice_operations` exporte trois fonctions : `additionner/2`, `multiplier/2` et `transposer/1`.

**Fonction `additionner/2`**

Cette fonction ajoute deux matrices. Elle prend deux paramètres `Matrice1` et `Matrice2` de type `matrix`, qui sont des listes de listes d'entiers.

La fonction utilise la fonction `lists:zipwith/3` pour appliquer une fonction à chaque paire d'éléments correspondants des deux listes. La fonction appliquée est une fonction anonyme qui ajoute les éléments correspondants des deux lignes.

**Fonction `multiplier/2`**

Cette fonction multiplie deux matrices. Elle prend deux paramètres `Matrice1` et `Matrice2` de type `matrix`.

La fonction utilise la fonction `lists:map/2` pour appliquer une fonction à chaque ligne de la première matrice. La fonction appliquée est une fonction anonyme qui multiplie chaque ligne de la première matrice par chaque colonne de la deuxième matrice.

**Fonction `transposer/1`**

Cette fonction transpose une matrice. Elle prend un paramètre `Matrice` de type `matrix`.

La fonction utilise à nouveau la fonction `lists:map/2` pour appliquer une fonction à chaque ligne de la matrice. La fonction appliquée est une fonction anonyme qui vérifie si chaque élément est présent dans la ligne. Cela crée une nouvelle matrice où les lignes et les colonnes sont inversées.
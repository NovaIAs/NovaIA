```erlang
-module(logique_floue).
-export([floue_et/2, floue_ou/2, floue_non/1]).

% Fonction "ET" floue
floue_et(A, B) ->
    min(A, B).

% Fonction "OU" floue
floue_ou(A, B) ->
    max(A, B).

% Fonction "NON" floue
floue_non(A) ->
    1 - A.

% Définir des fonctions d'appartenance pour des ensembles flous
% Par exemple, une fonction d'appartenance triangulaire pour l'ensemble "Chaud" :
appartenance_triangulaire(x, a, b, c) ->
    if
        x < a -> 0;
        x >= a andalso x <= b -> (x - a) / (b - a);
        x >= b andalso x <= c -> (c - x) / (c - b);
        true -> 0
    end.

% Exemple d'utilisation des fonctions d'appartenance
temperature = 25,
degre_appartenance_chaud = appartenance_triangulaire(temperature, 20, 25, 30),
degre_appartenance_froid = appartenance_triangulaire(temperature, 10, 15, 20),
degre_appartenance_entre_deux = floue_et(degre_appartenance_chaud, degre_appartenance_froid).
```

**Explication:**

Ce code implémente des opérations floues de base en Erlang.

* **Fonctions "ET", "OU" et "NON" floues:**
    * `floue_et(A, B)`: calcule le minimum des valeurs floues `A` et `B`.
    * `floue_ou(A, B)`: calcule le maximum des valeurs floues `A` et `B`.
    * `floue_non(A)`: calcule le complément de la valeur floue `A`.

* **Fonctions d'appartenance:**
    * `appartenance_triangulaire(x, a, b, c)`: définit une fonction d'appartenance triangulaire pour l'ensemble flou spécifié.
    * Par exemple, la fonction triangulaire pour l'ensemble "Chaud" aura une valeur de 1 pour les températures entre 25 et 30 degrés, et une valeur décroissante pour les températures en dehors de cette plage.

* **Exemple d'utilisation:**
    * Le code calcule le degré d'appartenance d'une température donnée (25 degrés) aux ensembles "Chaud", "Froid" et "Entre deux".
    * Il utilise la fonction d'appartenance triangulaire pour déterminer le degré d'appartenance à "Chaud" et "Froid", puis utilise les opérations floues pour calculer le degré d'appartenance à "Entre deux".
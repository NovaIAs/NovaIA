**Problème : Résolution d'équations quadratiques**

**Code PROLOG :**

```prolog
% Résolution d'équations quadratiques
% Entrée : Coefficients a, b et c
% Sortie : Racine 1 et racine 2

resolve_quadratique(A, B, C, Root1, Root2) :-
    D is B*B - 4*A*C,
    (
        D < 0 -> % Pas de racines réelles
        Root1 is 0,
        Root2 is 0
    ;
        D = 0 -> % Une seule racine réelle
        Root1 is -B / (2*A),
        Root2 is Root1
    ;
        % Deux racines réelles
        Root1 is (-B + sqrt(D)) / (2*A),
        Root2 is (-B - sqrt(D)) / (2*A)
    ).
```

**Explication du code :**

* `resolve_quadratique/5` est le prédicat principal qui résout les équations quadratiques. Il prend les coefficients `A`, `B` et `C` comme arguments et unifie les variables `Root1` et `Root2` avec les racines de l'équation.
* Le prédicat utilise la formule quadratique pour calculer le discriminant `D` : `D = B^2 - 4AC`.
* Selon la valeur de `D`, il y a trois cas possibles :
    * `D < 0` : Pas de racines réelles. Les variables `Root1` et `Root2` sont unifiées avec `0`.
    * `D = 0` : Une seule racine réelle. Les variables `Root1` et `Root2` sont unifiées avec la même valeur, la racine unique.
    * `D > 0` : Deux racines réelles. Les variables `Root1` et `Root2` sont unifiées avec les deux racines distinctes.

**Exemple d'utilisation :**

```prolog
?- resolve_quadratique(1, -4, 4, Root1, Root2).
Root1 = 2,
Root2 = 2.
```
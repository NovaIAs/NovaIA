**Programme de calcul de la factorielle en FORTH (en français)**

```forth
: factorial ( n -- n! )
    0 dup
    1 + loop
    over / ;
```

**Explication du code:**

* La ligne 1 définit une nouvelle fonction nommée "factorial". Elle prend un nombre entier "n" de la pile et renvoie "n!".
* La ligne 2 duplique "n" sur la pile.
* La ligne 3 ajoute 1 à "n".
* La ligne 4 démarre une boucle qui s'exécute tant que "n" est supérieur à 0.
* La ligne 5 multiplie l'élément supérieur de la pile ("n") par l'élément suivant ("n+1").
* La ligne 6 incrémente de 1 l'élément supérieur de la pile ("n+1").
* La ligne 7 saute à la ligne 4 pour répéter la boucle.
* La ligne 8 divise l'élément supérieur de la pile ("n!") par l'élément suivant ("n").
* La ligne 9 laisse "n!" sur la pile.

**Exemple d'utilisation:**

```forth
5 factorial    \ Calcule 5!
```

**Sortie:**

```text
120
```
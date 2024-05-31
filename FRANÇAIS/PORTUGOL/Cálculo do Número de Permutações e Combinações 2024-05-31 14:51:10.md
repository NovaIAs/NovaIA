**Programme pour le calcul du nombre de permutations et de combinaisons**

**Constantes:**

```portuguol
ler(n, r)
fatorial(n) = se n=0 então 1 senão n * fatorial(n-1) fim se
```

**Fonctions:**

```portuguol
permutacao(n, r) = se n < r então 0 senão fatorial(n) / fatorial(n - r) fim se

combinacao(n, r) = se n < r então 0 senão fatorial(n) / (fatorial(r) * fatorial(n - r)) fim se
```

**Programme principal:**

```portuguol
escreva("Número de permutações: ")
escreva(permutacao(n, r))
escreva("Número de combinações: ")
escreva(combinacao(n, r))
```

**Explication du code:**

Ce programme calcule le nombre de permutations et de combinaisons possibles pour un ensemble de n éléments pris r à la fois. Les permutations sont des arrangements ordonnés des éléments, tandis que les combinaisons sont des ensembles d'éléments non ordonnés.

Les constantes définissent les fonctions factorielles pour calculer le nombre de façons d'organiser n éléments.

Les fonctions `permutacao` et `combinacao` utilisent les fonctions factorielles pour calculer le nombre de permutations et de combinaisons possibles.

Le programme principal demande les valeurs de n et r à l'utilisateur et affiche le nombre de permutations et de combinaisons correspondantes.

**Exemple d'exécution:**

Si on entre n = 5 et r = 3, le programme affichera :

```
Número de permutações: 60
Número de combinações: 10
```
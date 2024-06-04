**Programme de Tri d'un Tableau d'Entiers en Ordre Ascendant en COOL**

```cool
class TriTableauEntiers {
    attribut tableau : Array[Integer];

    méthode main() : Nil {
        tableau := [3, 5, 2, 7, 4, 8, 1, 6];

        Pour i := 0 Jusqu'à tableau.length - 1 Faire
            min := i;
            Pour j := i + 1 Jusqu'à tableau.length - 1 Faire
                Si tableau[j] < tableau[min] Alors
                    min := j;
                FinSi;
            FinPour;

            Si min <> i Alors
                t := tableau[min];
                tableau[min] := tableau[i];
                tableau[i] := t;
            FinSi;
        FinPour;

        ÉcrireString("Tableau trié : ");
        Pour i := 0 Jusqu'à tableau.length - 1 Faire
            ÉcrireInt(tableau[i], False);
            ÉcrireString(" ");
        FinPour;
        ÉcrireSautLigne();
    }
}
```

**Explication du Code**

Ce programme trie un tableau d'entiers en ordre ascendant en utilisant l'algorithme de tri par sélection. L'algorithme fonctionne comme suit :

1. Il parcourt le tableau en trouvant le plus petit élément restant et l'échange avec le premier élément non trié.
2. Il répète cette opération jusqu'à ce que tous les éléments soient triés.

Dans le code, la variable `i` représente l'index de l'élément actuel en cours de comparaison, et `min` représente l'index du plus petit élément trouvé dans le tableau restant. La boucle interne compare chaque élément après `i` avec l'élément actuel à `min`. Si un élément plus petit est trouvé, `min` est mis à jour pour pointer vers cet élément. Après la boucle interne, si `min` n'est pas égal à `i`, les éléments aux indices `i` et `min` sont échangés.

Le tableau trié est ensuite affiché à l'écran.

Ce code est complexe car il utilise deux boucles imbriquées et manipule un tableau d'entiers. Il démontre également les concepts de comparaison, de recherche du minimum et d'échange d'éléments dans un tableau.
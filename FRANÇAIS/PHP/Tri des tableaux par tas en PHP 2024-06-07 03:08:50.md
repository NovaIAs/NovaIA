**Implémentation du tri par tas en PHP**

Le tri par tas est un algorithme efficace pour trier des tableaux en temps O(n log n). Il fonctionne en construisant une structure de données appelée tas max, puis en extrayant itérativement les éléments racines du tas pour obtenir le tableau trié.

```php
function tri_par_tas(array $tableau) {
    // Construire le tas max
    construire_tas($tableau);

    // Trier le tableau
    $taille_tas = count($tableau);
    for ($i = $taille_tas - 1; $i >= 0; $i--) {
        // Échanger la racine avec le dernier élément
        $temp = $tableau[$i];
        $tableau[$i] = $tableau[0];
        $tableau[0] = $temp;

        // Reconstruire le tas max après l'échange
        tas_max($tableau, 0, $i);
    }

    return $tableau;
}

function construire_tas(array &$tableau) {
    $taille_tas = count($tableau);

    // Construire un tas max en partant des feuilles
    for ($i = floor($taille_tas / 2) - 1; $i >= 0; $i--) {
        tas_max($tableau, $i, $taille_tas);
    }
}

function tas_max(array &$tableau, int $index, int $taille_tas) {
    // Trouver le plus grand enfant
    $gauche = 2 * $index + 1;
    $droite = 2 * $index + 2;
    $plus_grand = $index;

    if ($gauche < $taille_tas && $tableau[$gauche] > $tableau[$plus_grand]) {
        $plus_grand = $gauche;
    }

    if ($droite < $taille_tas && $tableau[$droite] > $tableau[$plus_grand]) {
        $plus_grand = $droite;
    }

    // Échanger la racine avec le plus grand enfant
    if ($plus_grand != $index) {
        $temp = $tableau[$index];
        $tableau[$index] = $tableau[$plus_grand];
        $tableau[$plus_grand] = $temp;

        // Reconstruire le tas max récursivement
        tas_max($tableau, $plus_grand, $taille_tas);
    }
}
```

**Explication :**

* **Construire le tas max** : La fonction `construire_tas` utilise une approche ascendante pour construire un tas max à partir d'un tableau donné. Elle commence par les niveaux inférieurs du tas et remonte vers la racine.
* **Trier le tableau** : La fonction `tri_par_tas` extrait itérativement le maximum du tas en échangeant la racine avec le dernier élément. Elle reconstruit ensuite le tas max pour obtenir le prochain maximum. Ce processus se poursuit jusqu'à ce que le tas soit vide.
* **Tas max** : La fonction `tas_max` maintient la structure de tas max en échangeant les éléments avec leurs plus grands enfants. Elle rétablit la propriété de tas max en percolant l'élément échangé vers le bas du tas.
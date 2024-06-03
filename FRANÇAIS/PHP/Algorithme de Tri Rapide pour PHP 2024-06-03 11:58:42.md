**Algorithme de Tri Rapide en PHP**

```php
<?php

/**
 * Fonction de tri rapide pour trier un tableau d'entiers.
 *
 * @param array $tableau Le tableau à trier.
 * @param int $debut L'indice de début du sous-tableau à trier.
 * @param int $fin L'indice de fin du sous-tableau à trier.
 */
function triRapide(array &$tableau, int $debut, int $fin)
{
    // Vérifier si le sous-tableau est vide.
    if ($debut >= $fin) {
        return;
    }

    // Choisir un pivot.
    $pivot = $tableau[($debut + $fin) >> 1];

    // Partitionner le tableau.
    $indexPivot = partition($tableau, $debut, $fin, $pivot);

    // Trier récursivement les sous-tableaux.
    triRapide($tableau, $debut, $indexPivot - 1);
    triRapide($tableau, $indexPivot + 1, $fin);
}

/**
 * Fonction de partitionnement pour l'algorithme de tri rapide.
 *
 * @param array $tableau Le tableau à partitionner.
 * @param int $debut L'indice de début du sous-tableau à partitionner.
 * @param int $fin L'indice de fin du sous-tableau à partitionner.
 * @param mixed $pivot Le pivot autour duquel partitionner le tableau.
 *
 * @return int L'indice du pivot dans le tableau partitionné.
 */
function partition(array &$tableau, int $debut, int $fin, $pivot)
{
    // Initialiser les indices du pointeur gauche et droit.
    $gauche = $debut - 1;
    $droite = $fin + 1;

    // Boucle jusqu'à ce que les pointeurs gauche et droit se croisent.
    while (true) {
        // Avancer le pointeur gauche tant que l'élément est inférieur au pivot.
        do {
            $gauche++;
        } while ($tableau[$gauche] < $pivot);

        // Reculer le pointeur droit tant que l'élément est supérieur au pivot.
        do {
            $droite--;
        } while ($tableau[$droite] > $pivot);

        // Si les pointeurs se croisent, retourner l'indice du pivot.
        if ($gauche >= $droite) {
            return $gauche;
        }

        // Échanger les éléments pointés par les pointeurs gauche et droit.
        $temp = $tableau[$gauche];
        $tableau[$gauche] = $tableau[$droite];
        $tableau[$droite] = $temp;
    }
}

// Tableau d'exemple.
$tableau = [10, 7, 8, 9, 1, 5];

// Appeler la fonction de tri rapide.
triRapide($tableau, 0, count($tableau) - 1);

// Afficher le tableau trié.
echo "Tableau trié : " . implode(', ', $tableau);

?>
```

**Explication du code**

L'algorithme de tri rapide est un algorithme de tri efficace qui fonctionne en divisant récursivement un tableau en sous-tableaux plus petits jusqu'à ce que chaque sous-tableau contienne un seul élément.

La fonction `triRapide` initialise les indices de début et de fin du sous-tableau à trier. Elle appelle ensuite la fonction `partition` pour partitionner le tableau autour d'un pivot choisi. La fonction `partition` renvoie l'indice du pivot dans le tableau partitionné.

Les fonctions `triRapide` et `partition` appellent ensuite elles-mêmes récursivement pour trier les sous-tableaux à gauche et à droite du pivot. Cela se répète jusqu'à ce que tous les éléments du tableau soient triés.

Voici une explication étape par étape de l'algorithme de tri rapide :

1. Choisir un pivot dans le tableau.
2. Partitionner le tableau en deux sous-tableaux :
    - Un sous-tableau contenant tous les éléments inférieurs au pivot.
    - Un sous-tableau contenant tous les éléments supérieurs au pivot.
3. Appeler récursivement l'algorithme de tri rapide sur les deux sous-tableaux.
4. Répéter les étapes 1 à 3 jusqu'à ce que le tableau soit entièrement trié.

La complexité temporelle moyenne de l'algorithme de tri rapide est O(n log n), où n est le nombre d'éléments dans le tableau. Cependant, dans le pire des cas, la complexité temporelle peut être O(n^2), ce qui se produit lorsque le tableau est déjà trié dans l'ordre croissant ou décroissant.
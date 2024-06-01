**Programme de calcul de la factorielle en PHP**

La factorielle d'un nombre entier positif n est le produit de tous les nombres entiers positifs inférieurs ou égaux à n. Par exemple, 5 ! = 5 * 4 * 3 * 2 * 1 = 120.

```php
<?php

/**
 * Calcul de la factorielle d'un nombre entier positif.
 *
 * @param int $nombre Le nombre dont on veut calculer la factorielle.
 *
 * @return int La factorielle du nombre donné.
 */
function factorielle(int $nombre): int
{
    // Cas de base : la factorielle de 0 est 1.
    if ($nombre === 0) {
        return 1;
    }

    // Cas récursif : la factorielle de n est n fois la factorielle de n-1.
    else {
        return $nombre * factorielle($nombre - 1);
    }
}

// Test du programme avec différents nombres.
echo factorielle(5) . PHP_EOL; // Affiche 120
echo factorielle(10) . PHP_EOL; // Affiche 3628800
echo factorielle(15) . PHP_EOL; // Affiche 1307674368000

?>
```

**Explication du code**

* **Fonction factorielle()** : Cette fonction prend un nombre entier positif en paramètre et renvoie sa factorielle.
* **Cas de base** : La factorielle de 0 est définie comme étant 1.
* **Cas récursif** : La factorielle d'un nombre n est égale à n fois la factorielle de n-1.
* **Appels récursifs** : La fonction factorielle() s'appelle elle-même de façon récursive jusqu'à atteindre le cas de base (n = 0).
* **Valeur de retour** : La fonction renvoie la factorielle du nombre donné.
* **Test du programme** : Le code principal du programme teste la fonction factorielle() avec différents nombres.
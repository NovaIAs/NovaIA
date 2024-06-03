**Programme de calcul de nombres premiers en D**

```d
import std.stdio;
import std.math;
import std.array;
import std.allocator;
import std.traits;

void main() {
    // Créer un tableau pour stocker les nombres premiers
    auto primes = new!allocator!(Array!int)();
    const uint maxNum = 10000000;

    // Initialiser le tableau avec les nombres 2 et 3
    primes.append(2);
    primes.append(3);

    // Itérer sur les nombres impairs jusqu'à maxNum
    foreach (int i; 5 .. maxNum; i += 2) {
        bool isPrime = true;

        // Vérifier si le nombre est divisible par l'un des nombres premiers connus
        foreach (int p; primes) {
            if (i % p == 0) {
                isPrime = false;
                break;
            }

            // Optimisation : on n'a pas besoin de vérifier au-delà de la racine carrée du nombre
            if (p*p > i)
                break;
        }

        // Si le nombre est premier, l'ajouter au tableau
        if (isPrime)
            primes.append(i);
    }

    // Afficher les nombres premiers trouvés
    foreach (int p; primes)
        writefLn("%d", p);
}
```

**Explication du code**

**1. Importations**

Le code commence par importer plusieurs modules de la bibliothèque standard D :

* `std.stdio` pour les opérations d'entrée/sortie
* `std.math` pour les fonctions mathématiques
* `std.array` pour la gestion des tableaux
* `std.allocator` pour la gestion de la mémoire
* `std.traits` pour les informations sur les types

**2. Constantes**

Le code définit une constante `maxNum` pour spécifier la limite supérieure des nombres à vérifier pour les nombres premiers.

**3. Tableau des nombres premiers**

Un tableau `primes` est créé pour stocker les nombres premiers trouvés. L'allocation dynamique est utilisée pour gérer la taille du tableau au fur et à mesure que de nouveaux nombres premiers sont ajoutés.

**4. Initialisation**

Les nombres 2 et 3 sont ajoutés manuellement au tableau des nombres premiers, car ce sont les seuls nombres premiers impairs.

**5. Itération sur les nombres impairs**

Une boucle `foreach` est utilisée pour itérer sur tous les nombres impairs de 5 à `maxNum` par incréments de 2. Les nombres pairs sont ignorés car ils ne peuvent pas être premiers.

**6. Vérification de la primalité**

Pour chaque nombre impair `i`, une variable booléenne `isPrime` est initialisée à `true`. Une boucle `foreach` interne parcourt les nombres premiers connus dans le tableau `primes` pour vérifier si `i` est divisible par l'un d'entre eux. Si c'est le cas, `isPrime` est défini sur `false` et la boucle interne est interrompue.

**7. Optimisation**

L'optimisation empêche la boucle interne de vérifier les nombres premiers au-delà de la racine carrée de `i`, car aucun nombre premier plus grand ne peut diviser `i`.

**8. Ajout des nombres premiers**

Si `isPrime` est toujours `true` après la boucle interne, cela signifie que `i` est un nombre premier, et il est ajouté au tableau `primes`.

**9. Affichage des résultats**

Une fois que tous les nombres impairs ont été vérifiés, la boucle `foreach` finale parcourt le tableau `primes` pour afficher tous les nombres premiers trouvés.
**Programme de Tri d'un Tableau d'Entiers**

```cool
-- Définition de la classe Tri
class Tri {
    -- Attributs
    entiers : Liste[entier];

    -- Constructeur
    initialiser(entiers : Liste[entier]) {
        this.entiers = entiers;
    }

    -- Méthodes
    trier() {
        -- Algorithme de tri par insertion
        pour i allant de 1 à this.entiers.longueur {
            clef = this.entiers[i];
            j = i - 1;
            tant que j >= 0 et clef < this.entiers[j] {
                this.entiers[j + 1] = this.entiers[j];
                j = j - 1;
            }
            this.entiers[j + 1] = clef;
        }

        -- Retourne le tableau trié
        return this.entiers;
    }
}

-- Création d'une instance de la classe Tri
entiers = [5, 3, 1, 2, 4];
trieur = Tri.initialiser(entiers);

-- Tri du tableau
entiers_tries = trieur.trier();

-- Affichage du tableau trié
pour i allant de 0 à entiers_tries.longueur {
    écrire(entiers_tries[i]);
    si i < entiers_tries.longueur - 1 {
        écrire(", ");
    }
}
```

**Explication du Code**

Ce programme définit une classe `Tri` qui représente un tableau d'entiers pouvant être trié. La classe possède les attributs suivants :

* `entiers` : Une liste d'entiers contenant les valeurs du tableau.

La classe `Tri` possède également les méthodes suivantes :

* `initialiser` : Crée une instance de la classe avec un tableau d'entiers spécifié.
* `trier` : Implémente l'algorithme de tri par insertion pour trier le tableau.

L'utilisation du programme est illustrée dans le code principal :

* Un tableau d'entiers non triés est créé.
* Une instance de la classe `Tri` est créée avec le tableau non trié.
* La méthode `trier` est appelée sur l'instance pour trier le tableau.
* Le tableau trié est affiché.
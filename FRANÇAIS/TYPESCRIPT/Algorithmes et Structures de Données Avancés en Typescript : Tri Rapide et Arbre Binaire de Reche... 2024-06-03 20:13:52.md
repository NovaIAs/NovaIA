```typescript
// Implémentation d'un algorithme de tri rapide en Typescript

// Fonction principale de tri rapide
function quickSort(tableau: number[]): number[] {
    // Cas de base : si le tableau est vide ou contient un seul élément, il est déjà trié
    if (tableau.length <= 1) {
        return tableau;
    }

    // Choix du pivot (élément qui divisera le tableau)
    const pivot = tableau[Math.floor(tableau.length / 2)];

    // Partitionnement du tableau en deux parties : inférieure et supérieure au pivot
    const partitionInferieure: number[] = [];
    const partitionSuperieure: number[] = [];
    for (let i = 0; i < tableau.length; i++) {
        if (tableau[i] < pivot) {
            partitionInferieure.push(tableau[i]);
        } else if (tableau[i] > pivot) {
            partitionSuperieure.push(tableau[i]);
        }
    }

    // Tri récursif des deux partitions
    const triPartitionInferieure = quickSort(partitionInferieure);
    const triPartitionSuperieure = quickSort(partitionSuperieure);

    // Concaténation des partitions triées et du pivot pour obtenir le tableau trié
    return [...triPartitionInferieure, pivot, ...triPartitionSuperieure];
}

// Utilisation d'un objet de type générique pour représenter un arbre binaire
class NoeudBinaire<T> {
    valeur: T;
    gauche: NoeudBinaire<T> | null;
    droite: NoeudBinaire<T> | null;

    constructor(valeur: T) {
        this.valeur = valeur;
        this.gauche = null;
        this.droite = null;
    }
}

// Implémentation d'un arbre binaire de recherche
class ArbreBinaire<T> {
    racine: NoeudBinaire<T> | null;

    constructor() {
        this.racine = null;
    }

    // Méthode d'insertion d'un nœud dans l'arbre
    inserer(valeur: T): void {
        if (this.racine === null) {
            this.racine = new NoeudBinaire<T>(valeur);
            return;
        }

        let noeudCourant = this.racine;
        while (true) {
            if (valeur < noeudCourant.valeur) {
                if (noeudCourant.gauche === null) {
                    noeudCourant.gauche = new NoeudBinaire<T>(valeur);
                    return;
                } else {
                    noeudCourant = noeudCourant.gauche;
                }
            } else {
                if (noeudCourant.droite === null) {
                    noeudCourant.droite = new NoeudBinaire<T>(valeur);
                    return;
                } else {
                    noeudCourant = noeudCourant.droite;
                }
            }
        }
    }

    // Méthode de recherche d'un nœud dans l'arbre
    rechercher(valeur: T): NoeudBinaire<T> | null {
        let noeudCourant = this.racine;
        while (noeudCourant !== null) {
            if (valeur === noeudCourant.valeur) {
                return noeudCourant;
            } else if (valeur < noeudCourant.valeur) {
                noeudCourant = noeudCourant.gauche;
            } else {
                noeudCourant = noeudCourant.droite;
            }
        }

        return null;
    }

    // Méthode de suppression d'un nœud de l'arbre
    supprimer(valeur: T): boolean {
        return this.supprimerNoeud(valeur, this.racine);
    }

    // Méthode privée récursive pour supprimer un nœud de l'arbre
    private supprimerNoeud(valeur: T, noeudCourant: NoeudBinaire<T> | null): boolean {
        if (noeudCourant === null) {
            return false;
        }

        if (valeur === noeudCourant.valeur) {
            // Cas 1 : Nœud sans enfant
            if (noeudCourant.gauche === null && noeudCourant.droite === null) {
                noeudCourant = null;
                return true;
            }
            // Cas 2 : Nœud avec un seul enfant
            else if (noeudCourant.gauche === null) {
                noeudCourant = noeudCourant.droite;
                return true;
            } else if (noeudCourant.droite === null) {
                noeudCourant = noeudCourant.gauche;
                return true;
            }
            // Cas 3 : Nœud avec deux enfants
            else {
                // Recherche du nœud le plus à gauche dans la sous-arbre droite
                let successeur = noeudCourant.droite;
                while (successeur.gauche !== null) {
                    successeur = successeur.gauche;
                }

                // Remplacement de la valeur du nœud à supprimer par celle du successeur
                noeudCourant.valeur = successeur.valeur;

                // Suppression du successeur
                return this.supprimerNoeud(successeur.valeur, noeudCourant.droite);
            }
        } else if (valeur < noeudCourant.valeur) {
            return this.supprimerNoeud(valeur, noeudCourant.gauche);
        } else {
            return this.supprimerNoeud(valeur, noeudCourant.droite);
        }
    }
}
```

**Explication du code :**

* L'algorithme de tri rapide est implémenté dans la fonction `quickSort`. Il divise le tableau en deux partitions (inférieure et supérieure au pivot) et les trie récursivement.

* L'arbre binaire de recherche est implémenté dans la classe `ArbreBinaire`. Il utilise des nœuds de type générique pour stocker des valeurs.

* La classe `ArbreBinaire` fournit des méthodes pour insérer, rechercher et supprimer des valeurs de l'arbre.

Ce code combine des concepts avancés de programmation, tels que la récursivité, les structures de données génériques et les algorithmes de tri, ce qui le rend complexe et difficile à reproduire.
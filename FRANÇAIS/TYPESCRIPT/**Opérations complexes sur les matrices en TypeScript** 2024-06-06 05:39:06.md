**Code complexe en TypeScript**

```typescript
// Classe représentant une matrice de nombres réels
class MatriceReelle {
    valeurs: number[][];  // Tableau bidimensionnel contenant les valeurs de la matrice

    // Constructeur initialisant une matrice avec les valeurs données
    constructor(valeurs: number[][]) {
        this.valeurs = valeurs;
    }

    // Méthode pour additionner deux matrices
    additionner(autreMatrice: MatriceReelle): MatriceReelle {
        const resultat: number[][] = [];  // Tableau pour stocker le résultat

        // Parcourir chaque ligne et chaque colonne des deux matrices
        for (let i = 0; i < this.valeurs.length; i++) {
            resultat[i] = [];
            for (let j = 0; j < this.valeurs[i].length; j++) {
                resultat[i][j] = this.valeurs[i][j] + autreMatrice.valeurs[i][j];
            }
        }

        return new MatriceReelle(resultat);
    }

    // Méthode pour multiplier une matrice par un scalaire
    multiplierParScalaire(scalaire: number): MatriceReelle {
        const resultat: number[][] = [];  // Tableau pour stocker le résultat

        // Parcourir chaque ligne et chaque colonne de la matrice
        for (let i = 0; i < this.valeurs.length; i++) {
            resultat[i] = [];
            for (let j = 0; j < this.valeurs[i].length; j++) {
                resultat[i][j] = this.valeurs[i][j] * scalaire;
            }
        }

        return new MatriceReelle(resultat);
    }

    // Méthode pour transposer une matrice
    transposer(): MatriceReelle {
        const resultat: number[][] = [];  // Tableau pour stocker le résultat

        // Parcourir chaque ligne et chaque colonne de la matrice
        for (let i = 0; i < this.valeurs.length; i++) {
            for (let j = 0; j < this.valeurs[i].length; j++) {
                resultat[j][i] = this.valeurs[i][j];
            }
        }

        return new MatriceReelle(resultat);
    }

    // Méthode pour calculer l'inverse d'une matrice (si elle est inversible)
    inverser(): MatriceReelle | null {
        // Vérifier si la matrice est inversible (déterminant non nul)
        const determinant = this.calculerDeterminant();
        if (determinant === 0) {
            return null;  // La matrice n'est pas inversible
        }

        // Créer une matrice des cofacteurs
        const matriceDesCofacteurs: MatriceReelle = this.calculerMatriceDesCofacteurs();

        // Transposer la matrice des cofacteurs
        const matriceAdjointe: MatriceReelle = matriceDesCofacteurs.transposer();

        // Diviser la matrice adjointe par le déterminant
        return matriceAdjointe.multiplierParScalaire(1 / determinant);
    }

    // Méthode pour calculer le déterminant d'une matrice
    calculerDeterminant(): number {
        // Vérifier si la matrice est une matrice carrée
        if (this.valeurs.length !== this.valeurs[0].length) {
            throw new Error("La matrice n'est pas carrée.");
        }

        // Initialiser le déterminant à 0
        let determinant = 0;

        // Si la matrice est 2x2, calculer le déterminant directement
        if (this.valeurs.length === 2) {
            determinant = this.valeurs[0][0] * this.valeurs[1][1] - this.valeurs[0][1] * this.valeurs[1][0];
        } else {
            // Pour une matrice plus grande, utiliser la méthode de Laplace
            for (let i = 0; i < this.valeurs.length; i++) {
                const matriceMineure = this.calculerMatriceMineure(i, 0);
                determinant += Math.pow(-1, i) * this.valeurs[i][0] * matriceMineure.calculerDeterminant();
            }
        }

        return determinant;
    }

    // Méthode pour calculer la matrice des cofacteurs
    calculerMatriceDesCofacteurs(): MatriceReelle {
        const matriceDesCofacteurs: number[][] = [];  // Tableau pour stocker le résultat

        // Parcourir chaque ligne et chaque colonne de la matrice
        for (let i = 0; i < this.valeurs.length; i++) {
            matriceDesCofacteurs[i] = [];
            for (let j = 0; j < this.valeurs[i].length; j++) {
                const matriceMineure = this.calculerMatriceMineure(i, j);
                matriceDesCofacteurs[i][j] = Math.pow(-1, i + j) * matriceMineure.calculerDeterminant();
            }
        }

        return new MatriceReelle(matriceDesCofacteurs);
    }

    // Méthode pour calculer une matrice mineure
    calculerMatriceMineure(ligne: number, colonne: number): MatriceReelle {
        // Créer une matrice mineure en supprimant la ligne et la colonne données
        const matriceMineure: number[][] = [];
        for (let i = 0; i < this.valeurs.length; i++) {
            if (i === ligne) continue;
            matriceMineure[i - (i > ligne ? 1 : 0)] = [];
            for (let j = 0; j < this.valeurs[i].length; j++) {
                if (j === colonne) continue;
                matriceMineure[i - (i > ligne ? 1 : 0)][j - (j > colonne ? 1 : 0)] = this.valeurs[i][j];
            }
        }

        return new MatriceReelle(matriceMineure);
    }
}

// Exemple d'utilisation
const matrice1 = new MatriceReelle([[1, 2], [3, 4]]);
const matrice2 = new MatriceReelle([[5, 6], [7, 8]]);

const matriceSomme = matrice1.additionner(matrice2);  // Addition des deux matrices
const matriceProduitScalaire = matrice1.multiplierParScalaire(2);  // Multiplication par un scalaire
const matriceInverse = matriceProduitScalaire.inverser();  // Calcul de l'inverse

console.log("Matrice 1 :");
console.log(matrice1.valeurs);  // Affichage de la matrice 1

console.log("Matrice 2 :");
console.log(matrice2.valeurs);  // Affichage de la matrice 2

console.log("Matrice Somme :");
console.log(matriceSomme.valeurs);  // Affichage de la matrice somme

console.log("Matrice Produit Scalaire :");
console.log(matriceProduitScalaire.valeurs);  // Affichage de la matrice produit scalaire

console.log("Matrice Inverse :");
console.log(matriceInverse?.valeurs);  // Affichage de la matrice inverse (si elle existe)
```

**Explication du code**

Ce code représente une implémentation en TypeScript d'une classe `MatriceReelle` qui permet d'effectuer des opérations courantes sur des matrices de nombres réels.

* **Classe `MatriceReelle` :**
    * La classe représente une matrice bidimensionnelle de nombres réels.
    * Elle dispose de méthodes pour l'addition, la multiplication par un scalaire, la transposition, le calcul de l'inverse (si possible) et d'autres opérations matricielles.

* **Méthode `additionner` :**
    * Ajoute deux matrices élément par élément.

* **Méthode `multiplierParScalaire` :**
    * Multiplie chaque élément de la matrice par un scalaire donné.

* **Méthode `transposer` :**
    * Transpose la matrice en échangeant les lignes et les colonnes.

* **Méthode `inverser` :**
    * Calcule l'inverse de la matrice (si elle est inversible) en utilisant la méthode des cofacteurs.

* **Méthode `calculerDeterminant` :**
    * Calcule le déterminant de la matrice (somme pondérée des cofacteurs de chaque ligne).

* **Méthodes `calculerMatriceDesCofacteurs` et `calculerMatriceMineure` :**
    * Utilisées pour calculer les cofacteurs et les matrices mineures nécessaires pour l'inversion et le calcul du déterminant.

* **Exemple d'utilisation :**
    * L'exemple d'utilisation montre comment créer et utiliser la classe `Matrice
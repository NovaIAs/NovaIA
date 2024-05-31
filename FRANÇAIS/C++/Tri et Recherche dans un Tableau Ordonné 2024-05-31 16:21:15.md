**Programme de tri et de recherche d'un tableau d'entiers triés**

**Explication :**

Ce code implémente un programme complexe qui tri un tableau d'entiers à l'aide de l'algorithme de tri rapide et recherche ensuite un élément cible dans le tableau trié à l'aide de la recherche binaire.

**Code :**

```c++
#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

// Fonction de tri rapide
void triRapide(vector<int>& tableau, int gauche, int droite) {
    if (gauche >= droite) {
        return;
    }

    // Partitionner le tableau
    int pivot = tableau[droite];
    int i = gauche - 1;
    for (int j = gauche; j < droite; j++) {
        if (tableau[j] < pivot) {
            i++;
            swap(tableau[i], tableau[j]);
        }
    }
    swap(tableau[i + 1], tableau[droite]);
    int pivotIndex = i + 1;

    // Récursivement trier les sous-tableaux
    triRapide(tableau, gauche, pivotIndex - 1);
    triRapide(tableau, pivotIndex + 1, droite);
}

// Fonction de recherche binaire
int rechercheBinaire(const vector<int>& tableau, int cible) {
    int gauche = 0;
    int droite = tableau.size() - 1;

    while (gauche <= droite) {
        int milieu = (gauche + droite) / 2;

        if (tableau[milieu] == cible) {
            return milieu;
        } else if (tableau[milieu] < cible) {
            gauche = milieu + 1;
        } else {
            droite = milieu - 1;
        }
    }

    return -1;
}

int main() {
    // Créer un tableau d'entiers
    vector<int> tableau = {5, 2, 9, 1, 3, 7, 8, 6};

    // Trier le tableau
    triRapide(tableau, 0, tableau.size() - 1);

    // Imprimer le tableau trié
    cout << "Tableau trié : ";
    for (int nombre : tableau) {
        cout << nombre << " ";
    }
    cout << endl;

    // Rechercher un élément cible
    int cible = 4;
    int index = rechercheBinaire(tableau, cible);

    if (index != -1) {
        cout << "La cible " << cible << " trouvée à l'index " << index << endl;
    } else {
        cout << "La cible " << cible << " n'est pas trouvée dans le tableau" << endl;
    }

    return 0;
}
```
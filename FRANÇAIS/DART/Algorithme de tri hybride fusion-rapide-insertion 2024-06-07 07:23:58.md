```dart
// Créez un algorithme de tri complexe basé sur la fusion et le tri rapide

class TriFusionRapide {
  // Méthode de tri par fusion
  static void triFusion(List<dynamic> tableau, int debut, int fin) {
    if (fin <= debut) {
      return;
    }

    int milieu = (debut + fin) ~/ 2;
    triFusion(tableau, debut, milieu);
    triFusion(tableau, milieu + 1, fin);

    // Fusion des deux demi-tableaux triés
    List<dynamic> temp = List.filled(fin - debut + 1, null);
    int indexTemp = 0;
    int indexDebut = debut;
    int indexMilieu = milieu + 1;

    while (indexDebut <= milieu && indexMilieu <= fin) {
      if (tableau[indexDebut] <= tableau[indexMilieu]) {
        temp[indexTemp++] = tableau[indexDebut++];
      } else {
        temp[indexTemp++] = tableau[indexMilieu++];
      }
    }

    while (indexDebut <= milieu) {
      temp[indexTemp++] = tableau[indexDebut++];
    }

    while (indexMilieu <= fin) {
      temp[indexTemp++] = tableau[indexMilieu++];
    }

    for (int i = 0; i < temp.length; i++) {
      tableau[debut + i] = temp[i];
    }
  }

  // Méthode de tri rapide
  static void triRapide(List<dynamic> tableau, int debut, int fin) {
    if (fin <= debut) {
      return;
    }

    int pivot = tableau[fin];
    int i = debut - 1;

    for (int j = debut; j < fin; j++) {
      if (tableau[j] <= pivot) {
        i++;
        tableau[i] ^= tableau[j];
        tableau[j] ^= tableau[i];
        tableau[i] ^= tableau[j];
      }
    }

    tableau[i + 1] ^= tableau[fin];
    tableau[fin] ^= tableau[i + 1];
    tableau[i + 1] ^= tableau[fin];

    triRapide(tableau, debut, i);
    triRapide(tableau, i + 2, fin);
  }

  // Méthode principale qui sélectionne et exécute le meilleur tri
  static void tri(List<dynamic> tableau) {
    if (tableau.isEmpty) {
      return;
    }

    if (tableau.length <= 100) {
      triInsertion(tableau);
    } else {
      // Si le tableau contient plus de 100 éléments, utilisez le tri par fusion
      triFusion(tableau, 0, tableau.length - 1);

      // Si le tableau est déjà presque trié, utilisez le tri rapide
      if (estPresqueTrie(tableau)) {
        triRapide(tableau, 0, tableau.length - 1);
      }
    }
  }

  // Méthode qui vérifie si le tableau est presque trié
  static bool estPresqueTrie(List<dynamic> tableau) {
    for (int i = 1; i < tableau.length; i++) {
      if (tableau[i] < tableau[i - 1]) {
        return false;
      }
    }
    return true;
  }

  // Méthode de tri par insertion (pour les petits tableaux)
  static void triInsertion(List<dynamic> tableau) {
    for (int i = 1; i < tableau.length; i++) {
      dynamic clé = tableau[i];
      int j = i - 1;

      while (j >= 0 && tableau[j] > clé) {
        tableau[j + 1] = tableau[j];
        j--;
      }

      tableau[j + 1] = clé;
    }
  }
}
```

**Explication du code :**

* **Algorithme de tri par fusion :** Divise le tableau en deux moitiés, tri chaque moitié récursivement, puis fusionne les deux moitiés triées.

* **Algorithme de tri rapide :** Sélectionne un élément pivot, partitionne le tableau en deux sous-tableaux basés sur le pivot, puis tri chaque sous-tableau récursivement.

* **Méthode de tri principale (tri ):**

    * Choisit le tri par insertion pour les petits tableaux (≤ 100 éléments).
    * Utilise le tri par fusion pour les tableaux plus grands, puis utilise le tri rapide si le tableau est presque trié.

* **Méthode isNearlySorted :** Vérifie si le tableau est presque trié en vérifiant s’il existe une inversion (un élément plus petit suit un élément plus grand).

* **Algorithme de tri par insertion :** Tri un tableau directement en insérant chaque élément à sa position correcte dans le tableau partiellement trié.
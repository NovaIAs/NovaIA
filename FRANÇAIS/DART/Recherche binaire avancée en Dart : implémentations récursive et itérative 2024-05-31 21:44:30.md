**Implémentation avancée de l'algorithme de recherche binaire en Dart**

```dart
// Recherche binaire récursive en O(log(n))
int rechercheBinaireRecursive(List<int> tableau, int valeurCiblee, int debut, int fin) {
  // Cas de base : tableau vide ou valeur cible non trouvée
  if (debut > fin) {
    return -1;
  }

  // Calculer l'index du milieu
  int milieu = ((debut + fin) / 2).floor();

  // Comparer la valeur cible avec l'élément du milieu
  if (tableau[milieu] == valeurCiblee) {
    return milieu;
  } else if (tableau[milieu] > valeurCiblee) {
    return rechercheBinaireRecursive(tableau, valeurCiblee, debut, milieu - 1);
  } else {
    return rechercheBinaireRecursive(tableau, valeurCiblee, milieu + 1, fin);
  }
}

// Recherche binaire itérative en O(log(n))
int rechercheBinaireIterative(List<int> tableau, int valeurCiblee) {
  int debut = 0;
  int fin = tableau.length - 1;

  // Boucle jusqu'à ce que les indices se croisent
  while (debut <= fin) {
    // Calculer l'index du milieu
    int milieu = ((debut + fin) / 2).floor();

    // Comparer la valeur cible avec l'élément du milieu
    if (tableau[milieu] == valeurCiblee) {
      return milieu;
    } else if (tableau[milieu] > valeurCiblee) {
      fin = milieu - 1;
    } else {
      debut = milieu + 1;
    }
  }

  // Valeur cible non trouvée
  return -1;
}
```

**Explication du code**

Ce code implémente l'algorithme de recherche binaire en Dart, qui permet de rechercher efficacement un élément dans un tableau trié. Il existe deux versions : une récursive et une itérative.

**Version récursive**

* La fonction `rechercheBinaireRecursive` prend en paramètres un tableau trié, une valeur cible, les indices de début et de fin.
* Elle vérifie si le tableau est vide ou si la valeur cible n'est pas trouvée (cas de base).
* Elle calcule l'index du milieu et compare la valeur cible avec l'élément du milieu.
* Selon la comparaison, elle appelle récursivement la fonction avec les indices ajustés.
* Elle renvoie l'index de la valeur cible si elle est trouvée, ou -1 sinon.

**Version itérative**

* La fonction `rechercheBinaireIterative` prend en paramètres un tableau trié et une valeur cible.
* Elle initialise les indices de début et de fin.
* Elle boucle tant que les indices ne se croisent pas.
* Elle calcule l'index du milieu, compare la valeur cible et ajuste les indices en fonction du résultat.
* Elle renvoie l'index de la valeur cible si elle est trouvée, ou -1 sinon.
**Module de tri d'un tableau d'entiers en ordre croissant**

```modula-2
MODULE TriTableau;
IMPORT Ordinal;

VAR
  tableau: ARRAY 1..100 OF CARDINAL;
  i, j, temp: CARDINAL;

PROCEDURE TriBulle;
BEGIN
  FOR i TO HIGH(tableau) - 1 DO
    FOR j TO HIGH(tableau) - i DO
      IF tableau[j] > tableau[j + 1] THEN
        temp := tableau[j];
        tableau[j] := tableau[j + 1];
        tableau[j + 1] := temp;
      FI;
    OD;
  OD;
END TriBulle;

PROCEDURE AffichageTableau;
BEGIN
  FOR i TO HIGH(tableau) DO
    WRITE(tableau[i], 0);
    WRITE(" ");
  OD;
  WRITELN;
END AffichageTableau;

BEGIN
  tableau := [5, 2, 8, 3, 1, 9, 4, 7, 6];
  WRITELN("Tableau avant tri:");
  AffichageTableau;

  TriBulle;

  WRITELN("Tableau après tri:");
  AffichageTableau;
END TriTableau.
```

**Explication du code:**

* Le module `TriTableau` définit un tableau d'entiers `tableau` et implémente deux procédures : `TriBulle` pour trier le tableau et `AffichageTableau` pour afficher le tableau.
* La procédure `TriBulle` utilise l'algorithme de tri à bulles pour trier le tableau en ordre croissant. Il parcourt le tableau à plusieurs reprises, en comparant chaque paire d'éléments adjacents et en les échangeant si l'élément de gauche est supérieur à l'élément de droite.
* La procédure `AffichageTableau` utilise la procédure `WRITE` pour afficher chaque élément du tableau, séparés par des espaces.
* Dans le code principal, un tableau est initialisé avec une séquence d'entiers.
* Le tableau est affiché avant et après le tri pour démontrer l'efficacité de l'algorithme de tri à bulles.
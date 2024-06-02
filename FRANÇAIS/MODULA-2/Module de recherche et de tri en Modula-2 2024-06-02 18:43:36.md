**Module de recherche et de tri**

```modula-2
MODULE RechercheEtTri;
FROM Storage IMPORT Elements;
FROM Output IMPORT Printf;

TYPE
  Cellule = RECORD
    Valeur: CARDINAL;
    Suivant: ^Cellule;
  END;

VAR
  Liste: ^Cellule;

PROCEDURE Recherche(Element: CARDINAL): BOOLEAN;
  VAR
    CelluleCourante: ^Cellule := Liste;
  BEGIN
    WHILE CelluleCourante <> NIL DO
      IF CelluleCourante^.Valeur = Element THEN
        RETURN TRUE;
      END;
      CelluleCourante := CelluleCourante^.Suivant;
    END;
    RETURN FALSE;
  END Recherche;

PROCEDURE Insertion(Element: CARDINAL);
  VAR
    CellulePrec, CelluleCourante: ^Cellule := NIL;
  BEGIN
    WHILE CelluleCourante <> NIL AND CelluleCourante^.Valeur < Element DO
      CellulePrec := CelluleCourante;
      CelluleCourante := CelluleCourante^.Suivant;
    END;
    NEW(CelluleTemp, Cellule);
    CelluleTemp^.Valeur := Element;
    IF CellulePrec = NIL THEN
      CelluleTemp^.Suivant := Liste;
      Liste := CelluleTemp;
    ELSE
      CelluleTemp^.Suivant := CellulePrec^.Suivant;
      CellulePrec^.Suivant := CelluleTemp;
    END;
  END Insertion;

PROCEDURE Tri();
  VAR
    CelluleCourante, CelluleMin: ^Cellule := Liste;
    ElementMin: CARDINAL;
  BEGIN
    WHILE CelluleCourante <> NIL DO
      CelluleMin := CelluleCourante;
      ElementMin := CelluleMin^.Valeur;
      CelluleMin := CelluleCourante^.Suivant;
      WHILE CelluleMin <> NIL DO
        IF CelluleMin^.Valeur < ElementMin THEN
          CelluleMin := CelluleMin;
          ElementMin := CelluleMin^.Valeur;
        END;
        CelluleMin := CelluleMin^.Suivant;
      END;
      CelluleTemp := CelluleCourante;
      CelluleTemp^.Valeur := ElementMin;
      CelluleMin^.Valeur := CelluleCourante^.Valeur;
      CelluleCourante := CelluleCourante^.Suivant;
    END;
  END Tri;

PROCEDURE Afficher();
  VAR
    CelluleCourante: ^Cellule := Liste;
  BEGIN
    WHILE CelluleCourante <> NIL DO
      Printf(" %d", CelluleCourante^.Valeur);
      CelluleCourante := CelluleCourante^.Suivant;
    END;
  END Afficher;

BEGIN
  Insertion(45);
  Insertion(23);
  Insertion(78);
  Insertion(6);
  Insertion(10);
  Printf("Liste avant tri:");
  Afficher();
  Tri();
  Printf("Liste après tri:");
  Afficher();
  IF Recherche(78) THEN
    Printf("L'élément 78 a été trouvé.");
  ELSE
    Printf("L'élément 78 n'a pas été trouvé.");
  END;
END RechercheEtTri.
```

**Explication**

Ce code crée un module contenant les fonctions nécessaires à la recherche et au tri d'une liste chaînée de valeurs entières.

**Type de donnée**

Le type de donnée `Cellule` représente un élément de la liste chaînée, avec un champ `Valeur` qui stocke la valeur entière et un champ `Suivant` qui pointe vers l'élément suivant de la liste.

**Variables**

La variable globale `Liste` est la tête de la liste chaînée.

**Procédures**

* `Recherche` : prend un élément en entrée et recherche sa présence dans la liste, renvoyant `TRUE` s'il est trouvé et `FALSE` sinon.
* `Insertion` : insère un nouvel élément dans la liste, en la maintenant triée dans l'ordre croissant.
* `Tri` : trie la liste entière dans l'ordre croissant en utilisant l'algorithme de tri par sélection.
* `Afficher` : affiche les valeurs de la liste sur la console.

**Module principal**

Le module principal insère cinq valeurs dans la liste, les affiche, les trie, les affiche à nouveau et recherche la valeur `78`. Le résultat de la recherche est affiché sur la console.

Ce code fournit un exemple d'implémentation pour manipuler une liste chaînée, rechercher et trier son contenu en MODULA-2.
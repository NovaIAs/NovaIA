```ada
-- Paquet d'exemple complexe en Ada
package Exemple is

   -- Type d'enregistrement
   type Personne_t is record
      Nom : String;
      Prenom : String;
      Age : Integer;
   end record;

   -- Tableau générique
   generic
      type Element_t is private;
      Taille : constant Positive;
   package Tableau_t is
      type Tableau is array (Positive range 1 .. Taille) of Element_t;
      procedure Afficher (T : in Tableau);
      procedure Trier (T : in out Tableau);
   end Tableau_t;

   -- Liste générique
   generic
      type Element_t is private;
   package Liste_t is
      type Liste is access all Personne_t;
      procedure Inserer (L : in out Liste; E : in Element_t);
      procedure Supprimer (L : in out Liste; E : in Element_t);
      procedure Afficher (L : in Liste);
   end Liste_t;

   -- Procédure de test
   procedure Test is
      -- Variables
      P : Personne_t := (Nom => "Dupont", Prenom => "Pierre", Age => 35);
      T : Tableau_t.Tableau := (1, 3, 2);
      L : Liste_t.Liste;

      -- Affichage d'une personne
      System.Put_Line ("Personne : " & P.Nom & " " & P.Prenom);

      -- Affichage d'un tableau
      Tableau_t.Afficher (T);

      -- Insertion dans une liste
      Liste_t.Inserer (L, P);

      -- Affichage de la liste
      Liste_t.Afficher (L);

      -- Tri d'un tableau
      Tableau_t.Trier (T);

      -- Affichage du tableau trié
      Tableau_t.Afficher (T);

      -- Suppression d'un élément de la liste
      Liste_t.Supprimer (L, P);

      -- Affichage de la liste après suppression
      Liste_t.Afficher (L);
   end Test;

end Exemple;
```

**Explication du code**

Le code implémente un certain nombre de structures de données et d'algorithmes complexes :

* **Type d'enregistrement** : Le type `Personne_t` représente une personne avec des champs pour le nom, le prénom et l'âge.
* **Tableau générique** : Le package `Tableau_t` fournit des opérations génériques sur les tableaux, telles que l'affichage et le tri.
* **Liste générique** : Le package `Liste_t` fournit des opérations génériques sur les listes liées, telles que l'insertion, la suppression et l'affichage.
* **Procédure de test** : La procédure `Test` crée des instances des structures de données et effectue diverses opérations sur elles, notamment l'affichage, l'insertion, la suppression et le tri.

Ce code illustre la puissance et la flexibilité d'Ada pour gérer des données complexes et des algorithmes. Il utilise des fonctionnalités telles que les génériques, les packages et les types définis par l'utilisateur pour créer un code réutilisable et maintenable.
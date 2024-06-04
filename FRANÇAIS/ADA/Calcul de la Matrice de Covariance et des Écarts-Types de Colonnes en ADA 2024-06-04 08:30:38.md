**Programme de Calcul de la Matrice de Covariance et de l'Écart-Type de Colonnes**

```ada
with Ada.Text_IO;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

procedure Covariance_and_Stdev is
   type Colonne is range natural;
   type Ligne is range natural;
   type Matrice is array (Colonne range<>, Ligne range<>) of Float;
   type Ligne_Matrice is array (Ligne) of Matrice;

   -- Déclaration des variables
   N : constant Positive := 10; -- Nombre de lignes et de colonnes
   M : Ligne_Matrice;          -- Matrice des données
   C : Matrice;                 -- Matrice de covariance
   S : array (Colonne) of Float; -- Vecteur des écarts-types de colonnes

   -- Initialisation de la matrice des données
   for L in Ligne loop
      for C in Colonne loop
         M(C, L) := Float (Random (100));
      end loop;
   end loop;

   -- Calcul de la matrice de covariance
   for I in Colonne loop
      for J in Colonne loop
         C(I, J) := 0.0;
         for L in Ligne loop
            C(I, J) := C(I, J) + (M(I, L) - Mean (M(I, _))) * (M(J, L) - Mean (M(J, _))) / N;
         end loop;
      end loop;
   end loop;

   -- Calcul des écarts-types de colonnes
   for C in Colonne loop
      S(C) := Sqrt (C(C, C));
   end loop;

   -- Affichage des résultats
   Ada.Text_IO.Put_Line ("Matrice de covariance :");
   for L in Ligne loop
      for C in Colonne loop
         Ada.Text_IO.Put (Item (C, L), Width => 10);
         Ada.Text_IO.Put (" ");
      end loop;
      Ada.Text_IO.New_Line;
   end loop;

   Ada.Text_IO.Put_Line ("Écarts-types de colonnes :");
   for C in Colonne loop
      Ada.Text_IO.Put (Item (S, C), Width => 10);
      Ada.Text_IO.Put (" ");
   end loop;
   Ada.Text_IO.New_Line;
end Covariance_and_Stdev;
```

**Explication du Code**

Ce code calcule la matrice de covariance et les écarts-types de colonnes d'une matrice de données.

**Variables**

* `N` : Nombre de lignes et de colonnes de la matrice
* `M` : Matrice des données
* `C` : Matrice de covariance
* `S` : Vecteur des écarts-types de colonnes

**Fonctions utilisées**

* `Mean` : Calcule la moyenne d'un vecteur de Float
* `Sqrt` : Calcule la racine carrée d'un Float

**Algorithme**

1. Initialisation de la matrice des données avec des valeurs aléatoires
2. Calcul de la matrice de covariance en utilisant la formule de covariance
3. Calcul des écarts-types de colonnes en utilisant la formule de l'écart-type
4. Affichage des résultats

**Complexité**

La complexité temporelle de ce code est O(n^3), où n est le nombre de lignes et de colonnes de la matrice.
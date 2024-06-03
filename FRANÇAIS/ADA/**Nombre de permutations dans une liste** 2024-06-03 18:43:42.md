**Objectif :** Calculer le nombre de permutations possibles d'une liste de n éléments.

**Code :**
```ada
with Ada.Containers.Ordered_Maps;

procedure Permutation is
   type Resultat is new Ada.Containers.Ordered_Maps.Ordered_Map(Positive, Positive);
   Resultat: Resultat;
   n, a: Positive;

begin
   Resultat := Ada.Containers.Ordered_Maps.Empty(Resultat);
   Resultat.Insert(0, 1);
   for n in 1 .. 100 loop
      a := 0;
      Resultat.First(n, a);
      if (a = 0) then
         Resultat.Delete(n);
      else
         Resultat.Insert(n + 1, n * a);
      end if;
   end loop;
   Resultat.First(100, n);
   Put_Line("Nombre de permutations de 100 éléments : " & n'Image(n));
end Permutation;
```

**Explication :**

* **Type Résultat :** Un dictionnaire (map) associant à chaque entier positif (nombre d'éléments) le nombre de permutations possibles.
* **Initialisation du dictionnaire :** Pour n = 0, une seule permutation est possible (liste vide).
* **Boucle principale :** Pour chaque n de 1 à 100, on calcule le nombre de permutations possibles pour n+1 éléments en fonction des valeurs précédentes.
* **Recherche du résultat :** On recherche le nombre de permutations pour n = 100 dans le dictionnaire.
* **Affichage du résultat :** On affiche le nombre de permutations.

**Complexité :**

* Le code utilise un dictionnaire, qui a une complexité O(1) pour les opérations de recherche et d'insertion.
* La boucle principale s'exécute 100 fois, ce qui donne une complexité totale de O(100) = O(1).
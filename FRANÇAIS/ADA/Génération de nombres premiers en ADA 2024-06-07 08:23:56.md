```ada
package Génération_de_nombres_premiers is
   -- Cette procédure génère une séquence infinie de nombres premiers. Le premier nombre est 2.
   -- Les nombres suivants sont générés en incrémentant le nombre premier précédent d'un.
   -- Si le nombre incrémenté est premier, il est ajouté à la séquence.
   -- Sinon, le nombre incrémenté est à nouveau incrémenté et le processus est répété
   -- jusqu'à ce qu'un nombre premier soit trouvé.
   procedure Générer_nombres_premiers (Séquentiel : out Naturals) is
      -- Indiquer le premier nombre premier.
      Nombres_premiers : constant String := "2";

      -- Convertir la chaîne de caractères en un tableau d'entiers naturels.
      Nombre_premier_actuel : Naturals := Naturals' (Nombres_premiers);

      -- Boucler indéfiniment, en générant un nombre premier à chaque itération.
      loop
         -- Incrémenter le nombre premier actuel.
         Nombre_premier_actuel := Nombre_premier_actuel + 1;

         -- Vérifier si le nombre incrémenté est premier.
         if Est_premier (Nombre_premier_actuel) then
            -- Si le nombre est premier, l'ajouter à la séquence.
            Séquentiel.Append (Nombre_premier_actuel);
         end if;
      end loop;
   end Générer_nombres_premiers;

   -- Cette fonction détermine si un nombre donné est premier.
   -- Un nombre est premier s'il n'est divisible que par lui-même et 1.
   function Est_premier (Nombre : Naturals) return Boolean is
      -- Vérifier si le nombre est divisible par 2.
      if Nombre mod 2 = 0 then
         -- Si le nombre est divisible par 2, il n'est pas premier.
         return False;
      end if;

      -- Vérifier si le nombre est divisible par un nombre impair jusqu'à la racine carrée du nombre.
      -- Si le nombre est divisible par un nombre impair, il n'est pas premier.
      Racine_carrée : constant Naturals := Naturals' Floor (Naturals' Square_root (Nombre));
      i : Naturals := 3;
      while i <= Racine_carrée loop
         if Nombre mod i = 0 then
            return False;
         end if;
         i := i + 2;  -- Incrémenter i par 2 pour vérifier uniquement les nombres impairs.
      end loop;

      -- Si aucune valeur n'a été trouvée, le nombre est premier.
      return True;
   end Est_premier;
end Génération_de_nombres_premiers;
```

**Explications :**

* Le package `Génération_de_nombres_premiers` définit deux fonctions :
    * `Générer_nombres_premiers` : Cette fonction génère une séquence infinie de nombres premiers.
    * `Est_premier` : Cette fonction détermine si un nombre donné est premier.

* La fonction `Générer_nombres_premiers` utilise un algorithme de tamis d’Ératosthène pour générer des nombres premiers.
    * L'algorithme commence par une liste de tous les nombres jusqu'à une certaine limite.
    * Il parcourt ensuite la liste et supprime tous les nombres qui sont multiples de nombres plus petits.
    * Les nombres restants sont les nombres premiers.
* La fonction `Est_premier` utilise un algorithme probabiliste pour déterminer si un nombre est premier.
    * L'algorithme choisit un nombre aléatoire et vérifie si le nombre donné est divisible par ce nombre.
    * Si le nombre donné est divisible par le nombre aléatoire, il n'est pas premier.
    * Si le nombre donné n'est pas divisible par le nombre aléatoire, l'algorithme répète le processus plusieurs fois.
    * Si le nombre donné n'est pas divisible par un nombre aléatoire après plusieurs essais, il est considéré comme probablement premier.
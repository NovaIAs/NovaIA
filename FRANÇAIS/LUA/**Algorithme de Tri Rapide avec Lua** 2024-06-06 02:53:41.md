**Algorithme de tri rapide en Lua**

```lua
-- Tri rapide :
function quicksort(tableau)
  local longueur = #tableau

  if longueur <= 1 then
    return tableau
  end

  -- Choisir un pivot
  local pivot = tableau[math.random(1, longueur)]

  -- Partitionner le tableau
  local gauche = {}
  local droite = {}
  for i = 1, longueur do
    if tableau[i] < pivot then
      table.insert(gauche, tableau[i])
    else
      table.insert(droite, tableau[i])
    end
  end

  -- Trier les sous-tableaux
  local gauche_trie = quicksort(gauche)
  local droite_trie = quicksort(droite)

  -- Fusionner les sous-tableaux triés
  return table.concat({ gauche_trie, pivot, droite_trie })
end

-- Utilisation de l'algorithme de tri rapide :
local tableau = { 10, 3, 7, 1, 9, 5, 2, 8, 4, 6 }
local tableau_trie = quicksort(tableau)

-- Affichage du tableau trié
for i = 1, #tableau_trie do
  print(tableau_trie[i])
end
```

**Explication du code :**

* **Algorithme de tri rapide :**
  * La fonction `quicksort` implémente l'algorithme de tri rapide pour trier un tableau.
* **Choix du pivot :**
  * Un pivot est choisi aléatoirement dans le tableau.
* **Partitionnement du tableau :**
  * Le tableau est partitionné en deux sous-tableaux : `gauche` contient les éléments inférieurs au pivot, et `droite` contient les éléments supérieurs ou égaux au pivot.
* **Tri des sous-tableaux :**
  * Les sous-tableaux `gauche` et `droite` sont triés récursivement en appelant à nouveau la fonction `quicksort`.
* **Fusion des sous-tableaux triés :**
  * Les sous-tableaux triés `gauche`, `pivot` et `droite` sont fusionnés pour obtenir le tableau trié final.

**Utilisation de l'algorithme de tri rapide :**

* Un tableau d'entiers est initialisé et passé à la fonction `quicksort`.
* Le tableau trié est stocké dans la variable `tableau_trie`.
* Le tableau trié est affiché à la console.
**Algorithme de tri rapide en Ruby**

```ruby
def tri_rapide(tableau)
  return tableau if tableau.size <= 1

  pivot = tableau.first
  tableau_gauche = tableau.drop(1).select { |e| e <= pivot }
  tableau_droit = tableau.drop(1).select { |e| e > pivot }

  tri_rapide(tableau_gauche) + [pivot] + tri_rapide(tableau_droit)
end
```

**Explication du code**

* **Fonction tri_rapide:** La fonction tri_rapide prend un tableau en argument et le trie en ordre croissant.

* **Cas de base:** Si le tableau contient 0 ou 1 élément, il est déjà trié. Dans ce cas, la fonction renvoie le tableau d'origine.

* **Pivot:** La fonction choisit le premier élément du tableau comme pivot.

* **Partitionnement:** Le tableau est partitionné en deux sous-tableaux :

    * `tableau_gauche` contient tous les éléments inférieurs ou égaux au pivot.
    * `tableau_droit` contient tous les éléments supérieurs au pivot.

* **Récursion:** La fonction tri_rapide est appelée récursivement sur `tableau_gauche` et `tableau_droit`.

* **Concaténation:** Les sous-tableaux triés sont concaténés avec le pivot pour obtenir le tableau trié final.

**Exemple d'utilisation**

```ruby
tableau = [5, 2, 8, 3, 1, 9, 4, 7, 6]
tableau_trie = tri_rapide(tableau)
puts tableau_trie  # Affiche : [1, 2, 3, 4, 5, 6, 7, 8, 9]
```
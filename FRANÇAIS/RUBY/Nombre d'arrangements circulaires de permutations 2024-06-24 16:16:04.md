**Calcul du nombre d'arrangements de permutations circulaires**

Ce code calcule le nombre d'arrangements circulaires de permutations d'une liste donnée d'éléments. Une permutation circulaire est une permutation où le premier élément peut être n'importe quel élément de la liste, et où les autres éléments suivent dans l'ordre de la permutation.

```ruby
def nombre_arrangements_circulaires(liste)
  # Calculer le nombre total de permutations
  nombre_permutations = liste.permutation(liste.size).to_a.size

  # Calculer le nombre de permutations circulaires
  nombre_circulaires = 0
  liste.each_with_index do |element, index|
    nombre_circulaires += nombre_permutations / liste.size
    liste.rotate!
  end

  # Renvoyer le nombre de permutations circulaires
  nombre_circulaires
end
```

**Explication**

* La fonction `nombre_permutations` calcule d'abord le nombre total de permutations de la liste donnée en utilisant la méthode `permutation` de la classe `Array`.
* La variable `nombre_circulaires` est initialisée à 0.
* La boucle `each_with_index` parcourt chaque élément de la liste et son index.
* Pour chaque élément, le nombre de permutations circulaires est calculé en divisant le nombre total de permutations par la taille de la liste. Cela est dû au fait qu'une permutation circulaire peut commencer par n'importe quel élément de la liste.
* La liste est ensuite tournée à droite à l'aide de la méthode `rotate!` pour que le prochain élément devienne le premier élément.
* La boucle continue jusqu'à ce que tous les éléments de la liste aient été considérés.
* Enfin, le nombre de permutations circulaires est renvoyé.
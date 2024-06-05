**Fonction de tri avancé avec plusieurs critères de tri et options de personnalisation**

```kotlin
fun <T> triAvancé(
    liste: List<T>,
    criteresTri: List<KeyValuePair<Fonction<T, Comparable<out Any?>>, Boolean>>,
    optionsTri: OptionsTri = OptionsTri()
) : List<T> {
    // Créer une liste de comparateurs basés sur les critères de tri
    val comparateurs = criteresTri.map { (clé, ascendant) ->
        Comparator<T> { a, b ->
            val comparaison = clé(a).compareTo(clé(b))
            if (ascendant) comparaison else -comparaison
        }
    }

    // Créer une fonction de comparaison composée basée sur les comparateurs
    val comparateurComposé = Comparator<T> { a, b ->
        var résultat = 0
        for (comparateur in comparateurs) {
            résultat = comparateur.compare(a, b)
            if (résultat != 0) break
        }
        résultat
    }

    // Effectuer le tri en fonction des options de tri
    val listeTriée = liste.sortedWith(comparateurComposé)
    if (optionsTri.inverse) listeTriée.reversed()

    // Appliquer la pagination si nécessaire
    if (optionsTri.pagination != null) {
        listeTriée.subList(optionsTri.pagination!!.offset, optionsTri.pagination!!.limit)
    } else {
        listeTriée
    }
}

// Classe pour encapsuler les options de tri
data class OptionsTri(
    var inverse: Boolean = false,
    var pagination: Pagination? = null
)

// Classe pour encapsuler les informations de pagination
data class Pagination(
    var offset: Int = 0,
    var limit: Int = Int.MAX_VALUE
)

// Exemple d'utilisation
val listeEntiers = listOf(1, 3, 5, 2, 4)

val optionsTri = OptionsTri(inverse = true, pagination = Pagination(offset = 1, limit = 2))

val listeTriée = triAvancé(
    listeEntiers,
    listOf(KeyValuePair(::compareTo, true), KeyValuePair(::mod, false)),
    optionsTri
)
```

**Explication du code :**

* La fonction `triAvancé` prend en entrée une liste à trier, une liste de critères de tri (chaque critère étant défini par une fonction de clé et un ordre croissant ou décroissant) et des options de tri (inversion de l'ordre, pagination).
* Elle crée une liste de comparateurs basés sur les critères de tri, puis une fonction de comparaison composée qui combine ces comparateurs.
* Le tri est effectué à l'aide de la fonction `sortedWith` avec la fonction de comparaison composée.
* Les options de tri sont ensuite appliquées : inversion de l'ordre, pagination.
* L'exemple d'utilisation montre comment utiliser la fonction `triAvancé` avec des entiers, deux critères de tri (ordre naturel et modulo) et des options de tri (ordre décroissant et pagination).
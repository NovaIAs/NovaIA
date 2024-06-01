```swift
// Un algorithme complexe de tri rapide en Swift

// Notre fonction de tri rapide prend un tableau mutable d'entiers "array" et deux indices "left" et "right" représentant la partie du tableau à trier.
func quickSort(array: inout [Int], left: Int, right: Int) {
    // Si l'intervalle est vide, il est déjà trié.
    guard left < right else { return }

    // On choisit un élément pivot qui divisera le tableau en deux parties.
    let pivotIndex = partition(array: &array, left: left, right: right)

    // On trie récursivement les deux parties du pivot.
    quickSort(array: &array, left: left, right: pivotIndex - 1)
    quickSort(array: &array, left: pivotIndex + 1, right: right)
}

// La fonction "partition" divise le tableau en deux parties en fonction du pivot.
func partition(array: inout [Int], left: Int, right: Int) -> Int {
    // On choisit l'élément le plus à droite comme pivot.
    let pivot = array[right]

    // On initialise l'index de la limite entre les éléments plus petits et plus grands que le pivot.
    var i = left - 1

    // On parcourt le tableau de gauche à droite.
    for j in left..<right {
        // Si l'élément courant est plus petit ou égal au pivot, on l'échange avec l'élément à l'index limite.
        if array[j] <= pivot {
            i += 1
            array.swapAt(i, j)
        }
    }

    // On place le pivot à sa position finale.
    array.swapAt(i + 1, right)

    // On renvoie l'index de la position finale du pivot.
    return i + 1
}

// On crée un tableau d'entiers à trier.
var array = [10, 5, 2, 7, 9, 3, 1, 6, 4, 8]

// On appelle la fonction de tri rapide.
quickSort(array: &array, left: 0, right: array.count - 1)

// On affiche le tableau trié.
print(array)
```

**Explication du code :**

* La fonction `quickSort` prend un tableau d'entiers `array` et deux indices `left` et `right` représentant la partie du tableau à trier. Elle trie récursivement le tableau à l'aide de la méthode du tri rapide.
* La fonction `partition` divise le tableau en deux parties en utilisant un élément pivot choisi. Elle renvoie l'index de la position finale du pivot.
* Dans la fonction `quickSort`, on choisit l'élément le plus à droite comme pivot et on appelle la fonction `partition` pour diviser le tableau en deux parties.
* On applique ensuite récursivement le tri rapide sur les deux parties, jusqu'à ce que l'intervalle à trier soit vide.
* La fonction `swapAt` échange les éléments à deux indices donnés dans le tableau `array`.
* Après le tri, le tableau `array` est trié par ordre croissant.
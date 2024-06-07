**Algorithme de recherche par intervalles binaire**

Cet algorithme de recherche efficace recherche un élément dans un tableau trié en divisant à plusieurs reprises l'intervalle de recherche en deux jusqu'à ce que l'élément soit trouvé ou que l'intervalle devienne vide.

```go
package main

import (
    "fmt"
    "sort"
)

// RechercheBinaire recherche un élément dans un tableau trié en utilisant la recherche binaire.
// Renvoie l'index de l'élément s'il est trouvé, ou -1 sinon.
func RechercheBinaire(arr []int, val int) int {
    // Trier le tableau (si nécessaire)
    sort.Ints(arr)

    // Définir les limites de l'intervalle de recherche
    gauche := 0
    droite := len(arr) - 1

    // Boucle jusqu'à ce que l'intervalle soit vide
    for gauche <= droite {
        // Calculer le milieu de l'intervalle
        milieu := (gauche + droite) / 2

        // Déterminer si le milieu est égal à la valeur recherchée
        if arr[milieu] == val {
            // L'élément a été trouvé, renvoyer son index
            return milieu
        }

        // Si le milieu est supérieur à la valeur recherchée, déplacer la limite droite à milieu - 1
        if arr[milieu] > val {
            droite = milieu - 1
        }

        // Sinon, déplacer la limite gauche à milieu + 1
        else {
            gauche = milieu + 1
        }
    }

    // L'élément n'a pas été trouvé
    return -1
}

func main() {
    // Créer un tableau trié
    arr := []int{1, 3, 5, 7, 9, 11, 13, 15}

    // Rechercher une valeur dans le tableau
    valeur := 11
    index := RechercheBinaire(arr, valeur)

    // Afficher le résultat
    if index == -1 {
        fmt.Println("La valeur", valeur, "n'a pas été trouvée dans le tableau.")
    } else {
        fmt.Println("La valeur", valeur, "a été trouvée à l'index", index, "dans le tableau.")
    }
}
```

**Explication du code**

* La fonction `RechercheBinaire` prend un tableau trié `arr` et une valeur `val` à rechercher dans le tableau.
* Elle trie d'abord le tableau (si nécessaire) à l'aide de la fonction `sort.Ints`.
* Elle définit ensuite les limites de l'intervalle de recherche `[gauche, droite]` sur l'ensemble du tableau.
* La fonction entre dans une boucle qui se poursuit tant que l'intervalle de recherche n'est pas vide (c'est-à-dire que `gauche` est inférieur ou égal à `droite`).
* À chaque itération de la boucle, elle calcule le milieu de l'intervalle et vérifie si la valeur au milieu est égale à la valeur recherchée.
* Si c'est le cas, elle renvoie l'index du milieu.
* Sinon, elle divise l'intervalle en deux en fonction de si la valeur au milieu est supérieure ou inférieure à la valeur recherchée. Elle ajuste ensuite les limites de l'intervalle `[gauche, droite]` en conséquence.
* Si la boucle se termine sans trouver la valeur recherchée, elle renvoie -1.
* La fonction `main` crée un tableau trié, recherche une valeur dans le tableau à l'aide de la fonction `RechercheBinaire` et affiche le résultat.
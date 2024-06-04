```swift
// Fonction de tri rapide en Swift
func triRapide(_ tableau: inout [Int]) {
    // Si le tableau est vide ou contient un seul élément, il est déjà trié
    guard tableau.count > 1 else { return }

    // Choisir un pivot dans le tableau (par exemple, le premier élément)
    let pivot = tableau[0]

    // Partitionner le tableau en deux sous-tableaux : les éléments inférieurs ou égaux au pivot et les éléments supérieurs au pivot
    var gauche = [Int]()
    var droite = [Int]()
    for élément in tableau[1...] {
        if élément <= pivot {
            gauche.append(élément)
        } else {
            droite.append(élément)
        }
    }

    // Trier récursivement les sous-tableaux gauche et droit
    triRapide(&gauche)
    triRapide(&droite)

    // Reconstruire le tableau trié en concaténant les sous-tableaux gauche, le pivot et les sous-tableaux droit
    tableau = gauche + [pivot] + droite
}

// Exemple d'utilisation
var tableau = [10, 5, 2, 8, 7, 1, 4, 9, 6, 3]
print("Tableau avant tri : \(tableau)")

triRapide(&tableau)

print("Tableau après tri : \(tableau)")

```

**Explication du code :**

* La fonction `triRapide` prend en entrée un tableau d'entiers non trié.
* Elle partitionne récursivement le tableau en deux sous-tableaux : les éléments inférieurs ou égaux au pivot (l'élément choisi au hasard dans le tableau) et les éléments supérieurs au pivot.
* Elle trie ensuite récursivement les sous-tableaux gauche et droit.
* Le tableau trié est obtenu en concaténant les sous-tableaux gauche, le pivot et les sous-tableaux droit.

**Complexité du code :**

La complexité du code est O(n log n) en moyenne, où n est la taille du tableau. Cependant, dans le pire des cas, la complexité peut être O(n^2).
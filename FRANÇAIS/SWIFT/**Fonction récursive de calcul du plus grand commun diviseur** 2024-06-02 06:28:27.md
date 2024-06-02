**Fonction récursive pour trouver le plus grand commun diviseur**

```swift
func gcd(_ a: Int, _ b: Int) -> Int {
    if b == 0 {
        return a
    } else {
        return gcd(b, a % b)
    }
}
```

**Explication :** Cette fonction récursive calcule le plus grand commun diviseur (PGCD) de deux nombres entiers `a` et `b` en utilisant l'algorithme d'Euclide. L'idée est que le PGCD de `a` et `b` est le même que le PGCD de `b` et le reste de la division de `a` par `b`. 

**Structure de données de pile**

```swift
struct Stack<T> {
    private var storage: [T] = []

    mutating func push(_ element: T) {
        storage.append(element)
    }

    mutating func pop() -> T? {
        return storage.popLast()
    }

    var isEmpty: Bool {
        return storage.isEmpty
    }
}
```

**Explication :** Cette structure implémente une structure de données de pile en utilisant un tableau. Elle permet de gérer une collection d'éléments LIFO ("last-in-first-out"), où le dernier élément ajouté à la pile est le premier à être supprimé.

**Algorithme de tri rapide**

```swift
func quickSort(_ array: inout [Int]) {
    if array.count <= 1 {
        return
    }

    let pivot = array.removeLast()
    var left: [Int] = []
    var right: [Int] = []

    for element in array {
        if element < pivot {
            left.append(element)
        } else {
            right.append(element)
        }
    }

    quickSort(&left)
    quickSort(&right)
    array = left + [pivot] + right
}
```

**Explication :** Cette fonction implémente l'algorithme de tri rapide, un algorithme de tri efficace basé sur la technique de "diviser pour régner". Il fonctionne en partitionnant le tableau en deux sous-tableaux : l'un contenant les éléments inférieurs au "pivot" (élément central) et l'autre contenant les éléments supérieurs ou égaux au pivot. Les deux sous-tableaux sont triés séparément, puis fusionnés pour obtenir le tableau trié.

**Analyseur syntaxique JSON**

```swift
func parseJSON(data: Data) -> [String: Any]? {
    do {
        return try JSONSerialization.jsonObject(with: data) as? [String: Any]
    } catch {
        print("Erreur lors de l'analyse du JSON : \(error)")
        return nil
    }
}
```

**Explication :** Cette fonction utilise l'API JSONSerialization pour analyser une chaîne JSON et la convertir en un dictionnaire Swift. Elle gère les erreurs potentielles et renvoie le dictionnaire analysé s'il est valide, ou nil s'il y a une erreur.
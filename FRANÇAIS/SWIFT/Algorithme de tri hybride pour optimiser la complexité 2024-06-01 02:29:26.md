**Algorithme de tri complexe utilisant une stratégie hybride**

**Objectif:** Trier un tableau d'entiers en utilisant une approche hybride qui combine le tri par bulles avec le tri par insertion.

**Code:**

```swift
func triHybride(tableau: [Int]) -> [Int] {
    // Étape 1: Tri par bulles pour réduire les grands écarts
    var estTrié = false
    while !estTrié {
        estTrié = true
        for i in 0..<tableau.count-1 {
            if tableau[i] > tableau[i+1] {
                tableau.swapAt(i, i+1)
                estTrié = false
            }
        }
    }
    
    // Étape 2: Tri par insertion pour affiner le tri
    for i in 1..<tableau.count {
        var clé = tableau[i]
        var j = i - 1
        while j >= 0 && tableau[j] > clé {
            tableau[j+1] = tableau[j]
            j -= 1
        }
        tableau[j+1] = clé
    }
    
    return tableau
}
```

**Explication:**

1. **Tri par bulles:** Dans la première étape, un tri par bulles est utilisé pour identifier et échanger les éléments très mal classés. Cela réduit les grands écarts entre les éléments.

2. **Tri par insertion:** Dans la deuxième étape, un tri par insertion est utilisé pour affiner le tri. Il insère chaque élément à sa position correcte dans le sous-tableau trié.

3. **Sortie:** La fonction renvoie le tableau trié par ordre croissant.
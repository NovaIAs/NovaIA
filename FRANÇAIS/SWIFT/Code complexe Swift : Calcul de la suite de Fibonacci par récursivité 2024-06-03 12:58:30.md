**Code complexe en Swift**

**Explication du code**

Ce code complexe en Swift implémente une fonction récursive qui calcule la série de Fibonacci. La série de Fibonacci est une séquence dans laquelle chaque terme est la somme des deux termes précédents.

* La fonction `fibonacci(n)` prend un entier `n` en argument et renvoie la valeur du n-ième terme de la suite de Fibonacci.
* Si `n` est inférieur à 2, la fonction renvoie simplement `n`. Cela sert de cas de base pour l'appel récursif.
* Si `n` est supérieur ou égal à 2, la fonction appelle récursivement elle-même avec `n-1` et `n-2` comme arguments. Les valeurs de retour de ces appels récursifs sont additionnées pour obtenir la valeur du n-ième terme.

**Code en Swift**

```swift
func fibonacci(_ n: Int) -> Int {
    if n < 2 {
        return n
    }
    return fibonacci(n - 1) + fibonacci(n - 2)
}
```

**Exemple d'utilisation**

L'exemple suivant montre comment utiliser la fonction `fibonacci(n)` pour calculer le 10ème terme de la suite de Fibonacci :

```swift
let result = fibonacci(10)
print("Le 10ème terme de la suite de Fibonacci est \(result)")
```

Cet exemple produira la sortie suivante :

```
Le 10ème terme de la suite de Fibonacci est 55
```
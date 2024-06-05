**Programme de Calcul d'Équation Quadratique**

```swift
import Foundation

// Déclaration des variables
var a, b, c: Double

// Saisie des coefficients
print("Entrez la valeur de a :")
if let input = readLine() {
    a = Double(input)!
}

print("Entrez la valeur de b :")
if let input = readLine() {
    b = Double(input)!
}

print("Entrez la valeur de c :")
if let input = readLine() {
    c = Double(input)!
}

// Calcul des racines
let discriminant = b * b - 4 * a * c
if discriminant < 0 {
    print("L'équation n'a pas de solution réelle.")
} else {
    let x1 = (-b + sqrt(discriminant)) / (2 * a)
    let x2 = (-b - sqrt(discriminant)) / (2 * a)
    
    // Affichage des racines
    print("Les racines de l'équation sont :")
    print("x1 = \(x1)")
    print("x2 = \(x2)")
}
```

**Explication**

Ce code en Swift calcule les solutions d'une équation quadratique de la forme `ax² + bx + c = 0`.

* **Déclaration des variables** : Les variables `a`, `b` et `c` sont déclarées pour stocker les coefficients de l'équation.
* **Saisie des coefficients** : L'utilisateur est invité à saisir les valeurs de `a`, `b` et `c`.
* **Calcul des racines** : Le discriminant est calculé en utilisant la formule `b² - 4ac`. Si le discriminant est négatif, l'équation n'a pas de solution réelle. Sinon, les deux racines sont calculées à l'aide des formules `(-b ± √(discriminant)) / (2a)`.
* **Affichage des racines** : Les racines calculées sont affichées à l'utilisateur.
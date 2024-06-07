**Code SWIFT Complexe**

Ce code crée une classe personnalisée appelée `Complexe` qui représente un nombre complexe composé d'une partie réelle et d'une partie imaginaire. Il implémente des opérations arithmétiques de base, telles que l'addition, la soustraction, la multiplication et la division, ainsi que des méthodes pour obtenir le module et l'argument du nombre complexe.

```swift
// Définition de la classe Complexe
class Complexe {
    // Attributs pour les parties réelle et imaginaire
    private var reel: Double
    private var imaginaire: Double

    // Initialiseur avec des valeurs par défaut
    init() {
        reel = 0
        imaginaire = 0
    }

    // Initialiseur avec des valeurs spécifiées
    init(reel: Double, imaginaire: Double) {
        self.reel = reel
        self.imaginaire = imaginaire
    }

    // Opérateurs arithmétiques

    // Addition
    static func +(lhs: Complexe, rhs: Complexe) -> Complexe {
        return Complexe(reel: lhs.reel + rhs.reel, imaginaire: lhs.imaginaire + rhs.imaginaire)
    }

    // Soustraction
    static func -(lhs: Complexe, rhs: Complexe) -> Complexe {
        return Complexe(reel: lhs.reel - rhs.reel, imaginaire: lhs.imaginaire - rhs.imaginaire)
    }

    // Multiplication
    static func *(lhs: Complexe, rhs: Complexe) -> Complexe {
        let r1 = lhs.reel
        let i1 = lhs.imaginaire
        let r2 = rhs.reel
        let i2 = rhs.imaginaire
        return Complexe(reel: r1 * r2 - i1 * i2, imaginaire: r1 * i2 + i1 * r2)
    }

    // Division
    static func /(lhs: Complexe, rhs: Complexe) -> Complexe {
        let d = rhs.reel * rhs.reel + rhs.imaginaire * rhs.imaginaire
        let r1 = lhs.reel
        let i1 = lhs.imaginaire
        let r2 = rhs.reel
        let i2 = rhs.imaginaire
        return Complexe(reel: (r1 * r2 + i1 * i2) / d, imaginaire: (i1 * r2 - r1 * i2) / d)
    }

    // Méthodes pour obtenir le module et l'argument

    func module() -> Double {
        return sqrt(reel * reel + imaginaire * imaginaire)
    }

    func argument() -> Double {
        return atan2(imaginaire, reel)
    }

    // Surcharge de l'opérateur de description
    var description: String {
        return "\(reel) + \(imaginaire)i"
    }
}

// Création de deux nombres complexes
let c1 = Complexe(reel: 3, imaginaire: 4)
let c2 = Complexe(reel: -2, imaginaire: 5)

// Opérations arithmétiques
let somme = c1 + c2
let difference = c1 - c2
let produit = c1 * c2
let quotient = c1 / c2

// Affichage des résultats
print("Somme : \(somme)")
print("Différence : \(difference)")
print("Produit : \(produit)")
print("Quotient : \(quotient)")

print("Module de c1 : \(c1.module())")
print("Argument de c1 : \(c1.argument())")
```

**Explication du Code**

* La classe `Complexe` a deux attributs privés, `reel` et `imaginaire`, qui stockent respectivement les parties réelle et imaginaire du nombre complexe.
* Il existe deux initialiseurs, l'un avec des valeurs par défaut pour initialiser un nombre complexe à zéro et l'autre avec des valeurs spécifiées pour définir des valeurs personnalisées.
* Les opérateurs arithmétiques sont surchargés pour permettre l'exécution d'opérations arithmétiques sur les nombres complexes.
* Les méthodes `module()` et `argument()` permettent de calculer le module et l'argument du nombre complexe respectivement.
* L'opérateur `description` est surchargé pour fournir une représentation lisible par l'utilisateur du nombre complexe.
* Dans l'exemple d'utilisation, deux nombres complexes sont créés, `c1` et `c2`, et des opérations arithmétiques sont effectuées sur eux. Les résultats sont ensuite affichés.

Ce code est complexe car il implémente des opérations arithmétiques personnalisées, gère des valeurs réelles et imaginaires distinctes, et fournit des méthodes pour des calculs avancés. Il est destiné à être utilisé dans des scénarios nécessitant des opérations mathématiques complexes sur des nombres complexes.
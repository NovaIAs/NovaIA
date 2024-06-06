```kotlin
// Définissez une classe "Complexe" pour représenter des nombres complexes
class Complexe(real: Double, imaginaire: Double) {

    // Propriétés
    private val real: Double = real
    private val imaginaire: Double = imaginaire

    // Méthodes

    // Addition de deux nombres complexes
    operator fun plus(autre: Complexe): Complexe {
        return Complexe(real + autre.real, imaginaire + autre.imaginaire)
    }

    // Soustraction de deux nombres complexes
    operator fun minus(autre: Complexe): Complexe {
        return Complexe(real - autre.real, imaginaire - autre.imaginaire)
    }

    // Multiplication de deux nombres complexes
    operator fun times(autre: Complexe): Complexe {
        return Complexe(
            real * autre.real - imaginaire * autre.imaginaire,
            real * autre.imaginaire + imaginaire * autre.real
        )
    }

    // Division de deux nombres complexes
    operator fun div(autre: Complexe): Complexe {
        val dénominateur = autre.real * autre.real + autre.imaginaire * autre.imaginaire
        return Complexe(
            (real * autre.real + imaginaire * autre.imaginaire) / dénominateur,
            (imaginaire * autre.real - real * autre.imaginaire) / dénominateur
        )
    }

    // Modulo d'un nombre complexe
    fun modulo(): Double {
        return Math.sqrt(real * real + imaginaire * imaginaire)
    }

    // Conjugaison d'un nombre complexe
    fun conjugue(): Complexe {
        return Complexe(real, -imaginaire)
    }

    // Représentation sous forme de chaîne de caractères
    override fun toString(): String {
        return "$real ${if (imaginaire >= 0) "+" else "-"} $imaginairei"
    }
}

// Créez un objet complexe et affichagez-le
val complexe1 = Complexe(3.0, 4.0)
val complexe2 = Complexe(2.0, 5.0)

println("Nombre complexe 1 : $complexe1")
println("Nombre complexe 2 : $complexe2")

// Effectuez des opérations sur les nombres complexes
val addition = complexe1 + complexe2
val soustraction = complexe1 - complexe2
val multiplication = complexe1 * complexe2
val division = complexe1 / complexe2

println("Addition : $addition")
println("Soustraction : $soustraction")
println("Multiplication : $multiplication")
println("Division : $division")

// Montrez le modulo et le conjugué
println("Modulo de $complexe1 : ${complexe1.modulo()}")
println("Conjugué de $complexe2 : ${complexe2.conjugue()}")
```

**Explications du code :**

Ce code crée une classe "Complexe" qui représente des nombres complexes sous la forme `a + bi`, où `a` est la partie réelle et `b` est la partie imaginaire.

**Propriétés :**

* `real` et `imaginaire` : Propriétés privées pour stocker la partie réelle et imaginaire du nombre complexe.

**Méthodes (Opérateurs surchargés) :**

* Les opérateurs `+`, `-`, `*` et `/` sont surchargés pour permettre des opérations arithmétiques sur les nombres complexes.
* L'opérateur `/` effectue la division complexe en utilisant la formule de conjugaison.

**Autres méthodes :**

* `modulo()` : Calcule le modulo (grandeur) du nombre complexe.
* `conjugue()` : Renvoie le conjugué du nombre complexe, qui est obtenu en changeant le signe de la partie imaginaire.
* `toString()` : Surcharge la méthode `toString()` pour afficher le nombre complexe sous forme de chaîne de caractères.

**Utilisation :**

Dans la partie d'utilisation du code, deux objets complexes sont créés et affichés. Différentes opérations arithmétiques sont ensuite effectuées sur ces nombres complexes, et les résultats sont affichés. Le modulo et le conjugué de l'un des nombres complexes sont également calculés et affichés.
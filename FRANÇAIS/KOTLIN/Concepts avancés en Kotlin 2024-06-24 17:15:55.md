**Classe de base**

```kotlin
class Vehicule {
    var marque: String
    var modele: String
    var annee: Int

    constructor(marque: String, modele: String, annee: Int) {
        this.marque = marque
        this.modele = modele
        this.annee = annee
    }

    fun rouler() {
        println("$marque $modele roule")
    }
}
```

**Classe fille**

```kotlin
class Voiture(marque: String, modele: String, annee: Int, var nbPortes: Int) : Vehicule(marque, modele, annee) {

    override fun rouler() {
        super.rouler()
        println("avec $nbPortes portes")
    }
}
```

**Classe abstraite**

```kotlin
abstract class Forme {
    abstract var surface: Double

    fun afficherSurface() {
        println("Surface : $surface")
    }
}
```

**Classes implémentant l'interface**

```kotlin
class Carre(var cote: Double) : Forme() {
    override var surface: Double = cote * cote
}

class Rectangle(var longueur: Double, var largeur: Double) : Forme() {
    override var surface: Double = longueur * largeur
}
```

**Classe utilisant des génériques**

```kotlin
class Liste<T>(private val elements: MutableList<T>) {

    fun ajouter(element: T) {
        elements.add(element)
    }

    fun get(index: Int): T {
        return elements[index]
    }

    fun size(): Int {
        return elements.size
    }
}
```

**Fonction de haut niveau**

```kotlin
fun calculerMoyenne(liste: List<Double>): Double {
    return liste.sum() / liste.size
}
```

**Fonction utilisant Kotlin coroutines**

```kotlin
suspend fun recupererDonnees(): List<String> {
    val deferred = async {
        // Code asynchrone exécuté en parallèle
        // ...
    }
    return deferred.await()
}
```

**Utilisation**

```kotlin
// Créer un véhicule
val voiture = Voiture("Renault", "Clio", 2020, 5)

// Appeler une méthode de la classe fille
voiture.rouler()

// Créer une forme
val carre = Carre(5.0)

// Afficher la surface de la forme
carre.afficherSurface()

// Créer une liste
val listeNombres = Liste<Int>(mutableListOf(1, 3, 5))

// Ajouter un élément à la liste
listeNombres.ajouter(7)

// Calculer la moyenne d'une liste
val moyenne = calculerMoyenne(listOf(10.0, 12.5, 15.3))

// Lancer une coroutine
runBlocking {
    val donnees = recupererDonnees()
    // Utiliser les données récupérées
}
```

**Explications**

Ce code illustre des fonctionnalités avancées de Kotlin, telles que :

* **Héritage et polymorphisme** (classes de base et filles)
* **Classes abstraites** (définition d'interfaces communes)
* **Génériques** (listes typées)
* **Fonctions de haut niveau** (fonctions globales pouvant être utilisées comme des variables)
* **Kotlin coroutines** (exécution asynchrone)
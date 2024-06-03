**Classe de Gestion des Avions**

```kotlin
class Avion {

    private var modele: String = ""
    private var capacite: Int = 0
    private var destinations: ArrayList<Destination> = ArrayList()

    constructor(modele: String, capacite: Int) {
        this.modele = modele
        this.capacite = capacite
    }

    fun ajouterDestination(destination: Destination) {
        destinations.add(destination)
    }

    fun getDestinations(): List<Destination> {
        return destinations
    }

    override fun toString(): String {
        return "Modèle : $modele, Capacité : $capacite, Destinations : ${destinations.joinToString(", ")}"
    }
}
```

**Classe de Gestion des Destinations**

```kotlin
class Destination {

    private var nom: String = ""
    private var pays: String = ""
    private var distance: Int = 0

    constructor(nom: String, pays: String, distance: Int) {
        this.nom = nom
        this.pays = pays
        this.distance = distance
    }

    fun getNom(): String {
        return nom
    }

    fun getPays(): String {
        return pays
    }

    fun getDistance(): Int {
        return distance
    }

    override fun toString(): String {
        return "Nom : $nom, Pays : $pays, Distance : $distance"
    }
}
```

**Classe Principale**

```kotlin
fun main(args: Array<String>) {

    val avion1 = Avion("Boeing 747", 400)
    avion1.ajouterDestination(Destination("Paris", "France", 1000))
    avion1.ajouterDestination(Destination("Londres", "Angleterre", 800))
    avion1.ajouterDestination(Destination("New York", "États-Unis", 5000))

    val avion2 = Avion("Airbus A380", 500)
    avion2.ajouterDestination(Destination("Tokyo", "Japon", 7000))
    avion2.ajouterDestination(Destination("Sydney", "Australie", 8500))

    val avions = listOf(avion1, avion2)

    println("**Liste des avions :**")
    avions.forEach { println(it) }

    // Trier les avions par capacité
    val avionsTries = avions.sortedBy { it.capacite }

    println("\n**Liste des avions triés par capacité :**")
    avionsTries.forEach { println(it) }

    // Filtrer les avions avec une distance de destination supérieure à 4000 km
    val avionsAVitesse = avions.filter { it.getDestinations().any { dest -> dest.getDistance() > 4000 } }

    println("\n**Liste des avions avec une destination à plus de 4000 km :**")
    avionsAVitesse.forEach { println(it) }
}
```

**Explication du Code**

Ce code modélise un système de gestion d'avions et de leurs destinations. Il définit deux classes : `Avion` et `Destination`, ainsi qu'une classe principale qui utilise ces classes pour gérer et manipuler des informations sur les avions et leurs destinations.

Les classes `Avion` et `Destination` représentent respectivement les avions et leurs destinations. Elles contiennent des propriétés et des méthodes pour accéder et modifier les informations associées.

La classe principale crée des instances d'avions, ajoute des destinations et affiche les informations sur l'avion et ses destinations. Elle trie également les avions par capacité et filtre les avions ayant des destinations à plus de 4000 km de distance.

Ce code illustre la programmation orientée objet, la gestion des données, le tri et le filtrage en Kotlin.
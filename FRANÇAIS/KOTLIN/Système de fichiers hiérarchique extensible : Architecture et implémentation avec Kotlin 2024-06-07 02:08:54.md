**Objectif :** Créer un système de gestion de fichiers hiérarchique extensible

**Code :**

```kotlin
abstract class FSElement {
    val nom: String
    val parent: FSElement?

    constructor(nom: String, parent: FSElement?) {
        this.nom = nom
        this.parent = parent
    }

    abstract fun taille(): Long
    abstract fun contenu(): List<String>?
}

class Fichier(nom: String, parent: FSElement?, val contenu: String) : FSElement(nom, parent) {
    override fun taille(): Long = contenu.length.toLong()
    override fun contenu(): List<String> = listOf(contenu)
}

class Répertoire(nom: String, parent: FSElement?) : FSElement(nom, parent) {
    private val éléments: MutableList<FSElement> = mutableListOf()

    override fun taille(): Long = éléments.sumOf { it.taille() }
    override fun contenu(): List<String>? = null

    fun ajouter(élément: FSElement) {
        éléments.add(élément)
    }
}

class SystèmeDeFichiers {
    private val racine: Répertoire = Répertoire("/", null)

    fun ajouter(chemin: String, élément: FSElement) {
        val parties = chemin.split("/")
        var répertoireCourant = racine
        for (partie in parties) {
            if (partie.isEmpty()) continue
            répertoireCourant = répertoireCourant.éléments.find { it.nom == partie } as Répertoire
        }
        répertoireCourant.ajouter(élément)
    }

    fun taille(chemin: String): Long {
        val parties = chemin.split("/")
        var répertoireCourant = racine
        for (partie in parties) {
            if (partie.isEmpty()) continue
            répertoireCourant = répertoireCourant.éléments.find { it.nom == partie } as Répertoire
        }
        return répertoireCourant.taille()
    }

    fun contenu(chemin: String): List<String>? {
        val parties = chemin.split("/")
        var répertoireCourant = racine
        for (partie in parties) {
            if (partie.isEmpty()) continue
            répertoireCourant = répertoireCourant.éléments.find { it.nom == partie } as Répertoire
        }
        return répertoireCourant.contenu()
    }

    fun lister(chemin: String): List<String> {
        val parties = chemin.split("/")
        var répertoireCourant = racine
        for (partie in parties) {
            if (partie.isEmpty()) continue
            répertoireCourant = répertoireCourant.éléments.find { it.nom == partie } as Répertoire
        }
        return répertoireCourant.éléments.map { it.nom }
    }
}
```

**Explication :**

Ce code crée un système de fichiers hiérarchique extensible en Java. Il définit trois classes :

* **FSElement** : La classe de base qui représente un élément du système de fichiers (fichier ou répertoire).
* **Fichier** : Une classe qui représente un fichier avec un contenu textuel.
* **Répertoire** : Une classe qui représente un répertoire contenant des éléments du système de fichiers.

Le code définit également une classe **SystèmeDeFichiers** qui gère la hiérarchie des fichiers et permet d'ajouter, de supprimer et de parcourir les éléments du système de fichiers.

**Fonctionnement :**

* Les éléments du système de fichiers sont organisés hiérarchiquement, avec un répertoire racine `/`.
* Les fichiers et les répertoires peuvent être ajoutés au système de fichiers en utilisant la méthode `ajouter()` de la classe `SystèmeDeFichiers`.
* La taille d'un répertoire est calculée en sommant les tailles de ses éléments, tandis que la taille d'un fichier correspond à la longueur de son contenu.
* Le contenu d'un répertoire est une liste des noms des éléments qu'il contient.
* La méthode `lister()` renvoie une liste des noms des éléments d'un répertoire.